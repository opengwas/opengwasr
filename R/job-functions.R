# The exported Job functions. They work for every query function through query plans
# (see R/query-plan.R), so a new query function needs nothing to be submitted and collected this way.

# Kept: an R argument's type; without a Source there is no API to ask.
check_source <- function(src, call = rlang::caller_env()) {
  if (!inherits(src, "og_api_source")) {
    og_abort("`src` must be a Source, from `og_api()`.", class = "opengwasr_argument", call = call)
  }
}

#' Run queries as Jobs without waiting
#'
#' `og_submit()` starts a query and returns as soon as the API has accepted its Jobs, so you can
#' start many and collect them yourself. It takes a query function (such as [og_tophits()]) and that
#' function's arguments after the Source. `og_job_status()` looks at Jobs without waiting;
#' `og_job_result()` waits for them and returns what the query function would have returned, with
#' the same columns and [og_provenance()]; `og_job_cancel()` cancels them.
#'
#' The query function runs as usual up to submitting its Jobs: its argument checks and lookups of
#' Analyses (and any Job it needs to prepare, such as resolving rsIDs) happen in `og_submit()`, and
#' finish before it returns. What the result is shaped from is fixed then, so `og_job_result()`
#' returns the same answer however much later you collect it. A query that needs several Jobs gives
#' one `og_jobs` object holding all of them.
#'
#' `og_job_status()` and `og_job_cancel()` take the `og_jobs` object or Job ids (text).
#' `og_job_result()` needs the `og_jobs` object: a bare Job id doesn't say how its rows are to be
#' shaped, and it refuses (`opengwasr_argument`). Rerun the same call instead (it reattaches to the
#' Job), or pass `raw = TRUE` for the API's rows exactly as they are, of class `og_job_raw`, without
#' opengwasr's columns or provenance.
#'
#' Pressing Esc or Ctrl-C while a query function waits doesn't cancel its Jobs. The error
#' (`opengwasr_interrupted`) carries their ids (`e$job_ids`) and the `og_jobs` object (`e$jobs`) to
#' collect with `og_job_result()`. A result the API has deleted (already collected, or expired)
#' raises `opengwasr_result_expired`.
#'
#' @param src A Source, from [og_api()].
#' @param fn A query function, such as [og_tophits()].
#' @param ... Its arguments after `src`.
#' @param job An `og_jobs` object from `og_submit()`; for `og_job_status()` and `og_job_cancel()`
#'   also Job ids.
#' @param raw For `og_job_result()` with a bare Job id: return the API's unshaped rows.
#' @return `og_submit()`: an `og_jobs` object. `og_job_status()`, `og_job_cancel()`: a data frame
#'   with `job_id`, `status` and `queue_position`. `og_job_result()`: the query function's result.
#' @examples
#' \dontrun{
#' src <- og_api()
#' job <- og_submit(src, og_tophits, "OGS-00011:GCST003116", clump = FALSE)
#' og_job_status(src, job)
#' top <- og_job_result(src, job)
#' }
#' @export
og_submit <- function(src, fn, ...) {
  # Kept: og_submit()'s contract with the query function it runs, which the API never sees.
  check_source(src)
  if (!is.function(fn)) {
    og_abort("`fn` must be a query function, such as `og_tophits`.", class = "opengwasr_argument")
  }
  args <- list(...)
  value <- NULL
  handle <- with_submit(value <- do.call(fn, c(list(src), args)))
  if (is.null(handle)) {
    og_abort(c("`fn` finished without submitting a Job.",
               i = "`og_submit()` takes a query function that runs on the API, such as `og_tophits`."),
             class = "opengwasr_argument")
  }
  if (!identical(value, handle)) {
    og_abort(c("`fn` did something with the query's answer instead of returning it.",
               i = "`og_submit()` takes a function that returns a query's result, such as `og_tophits`; do the rest after `og_job_result()`."),
             class = "opengwasr_argument")
  }
  handle$query <- job_query_text(substitute(fn), args)
  handle
}

# `og_tophits("OGS-00011:GCST003116", clump = FALSE)`, for printing.
job_query_text <- function(fn, args) {
  shown <- vapply(args, function(a) paste(deparse(a, width.cutoff = 60L)[[1]]), "")
  names <- names(args) %||% rep("", length(args))
  paste0(paste(deparse(fn), collapse = ""), "(",
         paste0(ifelse(nzchar(names), paste0(names, " = "), ""), shown, collapse = ", "), ")")
}

#' @export
print.og_jobs <- function(x, ...) {
  n <- length(x$job_ids)
  cat("<og_jobs> ", n, if (n == 1) " Job" else " Jobs", " for ", x$query, "\n", sep = "")
  print(data.frame(job_id = x$job_ids, status = x$status, row.names = NULL), row.names = FALSE)
  invisible(x)
}

# The Job ids of an og_jobs object or text. Kept: an R argument's type.
job_ids <- function(job, call = rlang::caller_env()) {
  ids <- if (inherits(job, "og_jobs")) job$job_ids else job
  if (!is.character(ids) || length(ids) == 0 || anyNA(ids) || any(!nzchar(ids))) {
    og_abort("`job` must be an `og_jobs` object from `og_submit()`, or Job ids as text.",
             class = "opengwasr_argument", call = call)
  }
  ids
}

#' @rdname og_submit
#' @export
og_job_status <- function(src, job) {
  check_source(src)
  ids <- job_ids(job)
  local_query(src, query = FALSE)
  statuses <- lapply(ids, function(id) {
    status <- job_status(src, id)
    job_state(status)
    status
  })
  job_frame(ids, statuses)
}

job_frame <- function(ids, statuses) {
  data.frame(
    job_id = ids,
    status = vapply(statuses, function(s) s$status, ""),
    queue_position = vapply(statuses, function(s) as.integer(s$queue_position %||% NA_integer_), 1L),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

#' @rdname og_submit
#' @export
og_job_result <- function(src, job, raw = FALSE) {
  check_source(src)
  # Kept: only a handle says how its rows are shaped, and whether it is whole; the API can't say.
  if (!inherits(job, "og_jobs")) {
    ids <- job_ids(job)
    if (!isTRUE(raw)) {
      og_abort(c("A Job id alone can't be collected: it doesn't say how the rows are to be shaped.",
                 i = "Pass the `og_jobs` object `og_submit()` returned, or run the same call again (it reattaches to the Job).",
                 i = "Or pass `raw = TRUE` for the API's rows exactly as they are."),
               class = "opengwasr_argument")
    }
    if (length(ids) != 1) {
      og_abort("`raw = TRUE` collects one Job id at a time.", class = "opengwasr_argument")
    }
    deadline <- local_query(src, query = FALSE)
    rows <- tryCatch(
      collect_job(src, utils::modifyList(list(job_id = ids), job_status(src, ids, deadline)), deadline,
                  rlang::current_env())$rows,
      interrupt = function(e) job_interrupted(ids, call = rlang::current_env())
    )
    return(structure(rows, class = c("og_job_raw", "data.frame")))
  }
  if (isTRUE(job$partial)) {
    og_abort(c(paste0("Only ", length(job$job_ids), " of the query's Jobs were submitted, so there is no whole answer to collect."),
               i = "Cancel them with `og_job_cancel()`, or run the same call again: the API reattaches to Jobs it already has."),
             class = "opengwasr_argument")
  }
  deadline <- local_query(src, query = FALSE)
  call <- rlang::current_env()
  # Wait for every Job, read its rows, and finish the plan pinned at submission. The handle's Jobs are
  # their submit answers, which hold no result, so each is looked up first.
  results <- tryCatch(
    lapply(job$jobs, function(j) {
      collect_job(src, utils::modifyList(j, job_status(src, j$job_id, deadline, call)), deadline, call)
    }),
    interrupt = function(e) job_interrupted(job$job_ids, job, call),
    opengwasr_timeout = function(e) {
      e$jobs <- job
      e$footer <- c(i = "Collect it with `og_job_result(src, e$jobs)`.")
      rlang::cnd_signal(e)
    }
  )
  job$plan$finish(results)
}

#' @rdname og_submit
#' @export
og_job_cancel <- function(src, job) {
  check_source(src)
  ids <- job_ids(job)
  deadline <- local_query(src, query = FALSE)
  statuses <- lapply(ids, function(id) {
    resp <- job_call(src, deadline, id, "unknown", rlang::current_env(), paste0("api/jobs/", id),
                     method = "DELETE")
    status <- api_parse(resp)
    job_state(status)
    status
  })
  out <- job_frame(ids, statuses)
  for (i in seq_along(ids)) {
    if (out$status[[i]] == "cancelled") {
      cli::cli_alert_success("Cancelled Job {job_short(ids[[i]])}.")
    } else {
      cli::cli_alert_info("Job {job_short(ids[[i]])} was already {out$status[[i]]}; nothing to cancel.")
    }
  }
  invisible(out)
}
