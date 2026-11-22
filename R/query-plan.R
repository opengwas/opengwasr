# The query plan: how a query function hands its Jobs to the Job client, and the one place that
# knows how those Jobs are run (waited for, or only submitted).
#
# THE CONTRACT for a query function (og_tophits(), og_associations(), the splitter, ...):
#
#   1. Prepare synchronously: validate arguments, look up Store Releases and Analyses, resolve rsIDs.
#      Preparation may run Jobs of its own through run_job(), or other query functions; these always
#      run to completion, even under og_submit().
#   2. Build a plan with query_plan(requests, finish, unit):
#        requests  a list of list(path, body): every Job the answer needs (one, or many when a
#                  query is split).
#        finish    function(results): `results` is one list(rows, job) per request, in order
#                  (`job` as run_job() returns it); returns the query's final value (shaped rows,
#                  warnings, provenance). It must use only what was pinned at plan time (in its
#                  closure), never look anything up again, so collecting later gives the very same
#                  answer as the synchronous call.
#   3. Return run_query(src, plan, deadline) as the last step.
#
# And every query method's FIRST line is `deadline <- local_query(src)`: it starts the user's call
# (one deadline for every request in it, including the lookups of preparation) and counts query
# depth, so run_query() knows whether it belongs to the outermost query the user asked for, however
# many wrappers, lambdas or helpers sit between. Nothing else needs marking.
#
# run_query() submits every request, then either waits for all of them and calls finish() (a
# normal call), or, when og_submit() is running the outermost query, returns the `og_jobs` handle
# without waiting (og_submit() refuses a function that runs two queries, or that does something
# with the handle instead of returning it). og_job_result() later waits for the handle's Jobs and calls the same finish().

query_plan <- function(requests, finish, unit = "rows") {
  ok <- is.list(requests) && all(vapply(requests, function(r) {
    is.list(r) && is.character(r$path) && length(r$path) == 1 && is.list(r$body)
  }, NA))
  if (!ok || !is.function(finish)) {
    og_abort("A query plan needs requests (each a list of `path` and `body`) and a `finish` function.",
             class = "opengwasr_argument")
  }
  structure(list(requests = requests, finish = finish, unit = unit), class = "og_query_plan")
}

# The state of the user's call that is running, one scoped place: when its deadline is (on the
# `job_now()` clock), how many queries deep we are, and whether og_submit() is collecting a handle.
query_state <- new.env()
query_state$depth <- 0L

call_deadline <- function() query_state$deadline

# The first line of every query method (`query = TRUE`), and of the Job functions (`query = FALSE`:
# they set the deadline but are not queries). The outermost call sets the deadline (the Source's
# timeout from now) and the rest share it; everything is restored on exit. Returns the deadline.
local_query <- function(src, query = TRUE, env = parent.frame()) {
  outermost <- is.null(query_state$deadline)
  if (outermost) query_state$deadline <- job_now() + src$timeout
  if (query) query_state$depth <- query_state$depth + 1L
  do.call(on.exit, list(bquote({
    if (.(query)) query_state$depth <- query_state$depth - 1L
    if (.(outermost)) query_state$deadline <- NULL
  }), TRUE), envir = env)
  query_state$deadline
}

# og_submit() runs the query function with `submit` set: the outermost run_query() then returns its
# handle instead of waiting, and a second one is refused.
with_submit <- function(code) {
  old <- query_state$submit
  query_state$submit <- new.env()
  on.exit(query_state$submit <- old, add = TRUE)
  force(code)
  query_state$submit$handle
}

run_query <- function(src, plan, deadline = call_deadline() %||% (job_now() + src$timeout),
                      call = rlang::caller_env()) {
  force(call)
  submit_only <- !is.null(query_state$submit) && query_state$depth == 1L
  if (submit_only && !is.null(query_state$submit$handle)) {
    og_abort(c("This function runs more than one query, so it can't be submitted as one.",
               i = "Call `og_submit()` for each query separately."),
             class = "opengwasr_argument", call = call)
  }
  if (!submit_only) {
    return(plan$finish(run_window(src, plan$requests, deadline, plan$unit, call, plan)))
  }
  # og_submit(): every piece is submitted at once, so they must fit the per-user queue. Kept: a split
  # limit; refused before submitting anything, rather than leave half a query queued.
  n <- length(plan$requests)
  if (n > max_queued_jobs_per_user) {
    og_abort(c(paste0("This query splits into ", n, " Jobs, more than the ", max_queued_jobs_per_user,
                      " one user may have queued (Jobs you already have queued count against the same ", max_queued_jobs_per_user, ")."),
               i = "Call the function itself instead of `og_submit()`: it runs a few Jobs at a time and keeps within the queue.",
               i = "Or ask for less at a time."),
             class = "opengwasr_too_many_jobs", refusal = TRUE, n_jobs = n,
             limit = max_queued_jobs_per_user, call = call)
  }
  st <- new_run_state(plan, n)
  guard_jobs(src, st, call, for (r in plan$requests) submit_next(src, st, r, deadline, call))
  handle <- new_og_jobs(plan, st$jobs)
  query_state$submit$handle <- handle
  handle
}

# What a run knows of its Jobs: those submitted (in `jobs`), how many were collected, the plan.
new_run_state <- function(plan, n) {
  st <- new.env()
  st$jobs <- list()
  st$collected <- 0L
  st$plan <- plan
  st$n <- n
  st
}

submit_next <- function(src, st, request, deadline, call) {
  job <- job_submit(src, request$path, request$body, deadline, call)
  job_reattached(job)
  st$jobs[[length(st$jobs) + 1]] <- job
  invisible(job)
}

# The one place that knows what a failure while running Jobs owes the user, for run_query() (submit
# mode) and run_window(): an interrupt reports every Job started, a refusal as too large cancels the
# Jobs started and not collected (never split round it), and any other error carries what was
# submitted: `e$jobs` (an `og_jobs` handle, `partial` unless every Job was submitted) and `e$job_ids`.
guard_jobs <- function(src, st, call, expr) {
  ids <- function() vapply(st$jobs, function(j) j$job_id, "")
  handle <- function() new_og_jobs(st$plan, st$jobs, partial = length(st$jobs) < st$n)
  tryCatch(expr,
    interrupt = function(e) {
      if (length(st$jobs) == 0) rlang::interrupt()
      job_interrupted(ids(), handle(), call)
    },
    opengwasr_request_too_large = function(e) {
      e$cancelled <- cancel_jobs(src, ids()[seq_along(st$jobs) > st$collected], call)
      rlang::cnd_signal(e)
    },
    error = function(e) {
      if (length(st$jobs) > 0 && is.null(e$jobs)) {
        e$jobs <- handle()
        e$job_ids <- e$jobs$job_ids
        if (inherits(e, "opengwasr_timeout") && !isTRUE(e$jobs$partial)) {
          e$footer <- c(i = "Collect it with `og_job_result(src, e$jobs)`.")
        }
      }
      rlang::cnd_signal(e)
    })
}

# Run `requests` as Jobs with at most `src$max_in_flight` submitted and not yet collected at once:
# the first ones are submitted, each is waited for in request order, and the next is submitted as
# soon as one is collected. The whole run is inside the call's one deadline. Returns one
# `list(rows, job)` per request, in request order (what finish() takes). Used by run_query() and by
# preparation that splits (the rsID lookup). `plan` is what an interrupt's handle carries to collect
# with; a lookup has none.
run_window <- function(src, requests, deadline, unit, call = rlang::caller_env(), plan = NULL) {
  force(call)
  n <- length(requests)
  results <- vector("list", n)
  st <- new_run_state(plan %||% list(requests = requests, unit = unit), n)
  if (n > 1 && progress_enabled()) {
    cli::cli_alert_info("Split into {n} Jobs, at most {src$max_in_flight} at a time.")
  }
  guard_jobs(src, st, call, {
    while (st$collected < n) {
      while (length(st$jobs) < n && length(st$jobs) - st$collected < src$max_in_flight) {
        submit_next(src, st, requests[[length(st$jobs) + 1]], deadline, call)
      }
      results[[st$collected + 1]] <- collect_job(src, st$jobs[[st$collected + 1]], deadline, unit, call)
      st$collected <- st$collected + 1L
    }
  })
  results
}

# Best-effort cancel of Jobs a refused query had already submitted. Returns the ids it asked to cancel.
# It has a short grace of its own, outside the call's deadline: cancelling after a failure (possibly
# a timeout) must still be tried.
cancel_jobs <- function(src, ids, call) {
  deadline <- job_now() + 30
  for (id in ids) {
    tryCatch(job_call(src, deadline, id, "unknown", call, paste0("api/jobs/", id), method = "DELETE"),
             error = function(e) NULL)
  }
  ids
}

# Wait for every Job of a handle, read its rows and call the plan's finish(). `fetch`: the handle
# was made earlier, so look each Job up first (its submit answer holds no result).
collect_plan <- function(src, handle, deadline, fetch, call) {
  if (isTRUE(handle$partial)) {
    og_abort(c(paste0("Only ", length(handle$job_ids), " of the query's Jobs were submitted, so there is no whole answer to collect."),
               i = "Cancel them with `og_job_cancel()`, or run the same call again: the API reattaches to Jobs it already has."),
             class = "opengwasr_argument", call = call)
  }
  results <- tryCatch(
    lapply(handle$jobs, function(job) {
      if (fetch) job <- utils::modifyList(job, job_status(src, job$job_id, deadline, call))
      collect_job(src, job, deadline, handle$plan$unit, call)
    }),
    interrupt = function(e) job_interrupted(handle$job_ids, handle, call),
    opengwasr_timeout = function(e) {
      e$jobs <- handle
      e$footer <- c(i = "Collect it with `og_job_result(src, e$jobs)`.")
      rlang::cnd_signal(e)
    }
  )
  handle$plan$finish(results)
}

# The handle og_submit() returns: the plan, the Jobs as the API accepted them (their cost estimate
# and whether they were reattached), and their ids. `partial`: an interrupt or error stopped the
# submission part-way; the handle holds the Jobs that were accepted.
new_og_jobs <- function(plan, jobs, query = NULL, partial = FALSE) {
  structure(list(job_ids = vapply(jobs, function(j) j$job_id, ""),
                 status = vapply(jobs, function(j) j$status, ""),
                 jobs = jobs, plan = plan, query = query, partial = partial),
            class = "og_jobs")
}
