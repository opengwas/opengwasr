# The query plan: how a query hands its Jobs to the Job client, and the one place that knows how
# those Jobs are run (waited for, or only submitted).
#
# A query that returns Associations doesn't use this directly: association_query()
# (R/association-query.R) does all of it. A query of another kind (LD, #12) follows the contract:
#
#   1. Its first line is `deadline <- local_query(src)`: it starts the user's call (one deadline for
#      every request in it, lookups included) and counts query depth, so run_query() knows whether
#      it belongs to the outermost query, however many wrappers sit between.
#   2. Prepare synchronously: validate arguments, look up Store Releases and Analyses, resolve rsIDs
#      (resolve_variants(), whose Jobs run through run_window()). Preparation always runs to
#      completion, even under og_submit().
#   3. Build query_plan(requests, finish):
#        requests  a list of list(path, body): every Job the answer needs (several when split).
#        finish    function(results): `results` is one list(rows, job) per request, in order (as
#                  collect_job() returns them); returns the query's value. It must use only what its
#                  closure pinned at plan time, never look anything up, so collecting later gives the
#                  very same answer as the synchronous call.
#   4. Return run_query(src, plan, deadline).
#
# run_query() submits every request, then either waits for all of them and calls finish() (a
# normal call), or, when og_submit() is running the outermost query, returns the `og_jobs` handle
# without waiting (og_submit() refuses a function that runs two queries, or that does something
# with the handle instead of returning it). og_job_result() later waits for the handle's Jobs and
# calls the same finish().

query_plan <- function(requests, finish) list(requests = requests, finish = finish)

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

run_query <- function(src, plan, deadline, call = rlang::caller_env()) {
  force(call)
  submit_only <- !is.null(query_state$submit) && query_state$depth == 1L
  if (submit_only && !is.null(query_state$submit$handle)) {
    og_abort(c("This function runs more than one query, so it can't be submitted as one.",
               i = "Call `og_submit()` for each query separately."),
             class = "opengwasr_argument", call = call)
  }
  if (!submit_only) return(plan$finish(run_window(src, plan$requests, deadline, call, plan)))
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
  jobs <- run_window(src, plan$requests, deadline, call, plan, wait = FALSE)
  query_state$submit$handle <- new_og_jobs(plan, jobs)
}

# The one place that knows what a failure while running Jobs owes the user (`st` is run_window()'s
# state: the Jobs submitted, how many were collected, the plan): an interrupt reports every Job
# started, a refusal as too large cancels the Jobs started and not collected (never split round it),
# and any other error carries what was submitted: `e$jobs` (an `og_jobs` handle, `partial` unless
# every Job was submitted) and `e$job_ids`.
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
# `list(rows, job)` per request, in request order (what finish() takes). With `wait = FALSE`
# (og_submit()), submits every request and returns the accepted Jobs. Used by run_query() and by
# preparation that splits (the rsID lookup). `plan` is what an interrupt's handle carries to collect
# with; a lookup has none.
run_window <- function(src, requests, deadline, call = rlang::caller_env(), plan = NULL, wait = TRUE) {
  force(call)
  n <- length(requests)
  results <- vector("list", n)
  st <- list2env(list(jobs = list(), collected = 0L, n = n, plan = plan %||% list(requests = requests)))
  if (wait && n > 1 && progress_enabled()) {
    cli::cli_alert_info("Split into {n} Jobs, at most {src$max_in_flight} at a time.")
  }
  guard_jobs(src, st, call, repeat {
    while (length(st$jobs) < n && (!wait || length(st$jobs) - st$collected < src$max_in_flight)) {
      r <- requests[[length(st$jobs) + 1]]
      job <- job_submit(src, r$path, r$body, deadline, call)
      job_reattached(job)
      st$jobs[[length(st$jobs) + 1]] <- job
    }
    if (!wait || st$collected == n) break
    results[[st$collected + 1]] <- collect_job(src, st$jobs[[st$collected + 1]], deadline, call)
    st$collected <- st$collected + 1L
  })
  if (wait) results else st$jobs
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

# The handle og_submit() returns: the plan, the Jobs as the API accepted them (their cost estimate
# and whether they were reattached), and their ids. `partial`: an interrupt or error stopped the
# submission part-way; the handle holds the Jobs that were accepted.
new_og_jobs <- function(plan, jobs, query = NULL, partial = FALSE) {
  structure(list(job_ids = vapply(jobs, function(j) j$job_id, ""),
                 status = vapply(jobs, function(j) j$status, ""),
                 jobs = jobs, plan = plan, query = query, partial = partial),
            class = "og_jobs")
}
