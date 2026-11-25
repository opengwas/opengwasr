# The Job client: the one place that runs a query as a Job. The API answers every data query with a
# Job (ADR 0002): opengwasr submits it, waits for it, and reads every page of its result.
#
#   job_submit(src, path, body)  -> Job (id, poll_after_seconds, existing, queue_position, ...)
#   job_wait(src, job)           -> the finished Job's status; polls at the server's interval with a
#                                   capped backoff, until the call's deadline, with progress
#   job_rows(src, job_id, n)     -> every row of the result, cursor by cursor, in one pass, checked
#                                   against the manifest's row count `n`
#   collect_job(src, job, ...)   -> wait and read: `list(rows, job)`, `job` being what provenance
#                                   needs (job_id, total, truncated, releases, existing, cost)
#
# Queries don't call these: they hand requests to the query plan (R/query-plan.R), which runs them.
# One deadline covers submit, wait and every result page; each request's own timeout is what is
# left of it. Anything the API sends that can't be trusted (an unknown status, a page without rows,
# rows that don't add up) is `opengwasr_job_invalid`.

job_states_active <- c("queued", "running")
job_states_done <- c("succeeded", "failed", "cancelled", "expired")

# The Job client's clock and sleep, the one seam tests replace (with `local_mocked_bindings()`) to
# drive waiting and deadlines without really waiting. Seconds on a monotonic clock.
job_now <- function() proc.time()[["elapsed"]]
job_sleep <- function(seconds) Sys.sleep(seconds)

# What one Job takes (the API's limits). Kept client-side because the splitter (R/split.R) needs
# them to cut a request into Jobs before sending any; the API's own refusals are never re-checked.
max_releases_per_job <- 10L    # GWAS Stores (Store Releases) in `releases`
max_variants_per_job <- 1000L  # variants in POST /api/associations
max_analyses_per_job <- 100L   # Analyses in POST /api/associations
max_rsids_per_job <- 1000L     # rsIDs in POST /api/variants/rsid
max_ld_matrix_per_job <- 500L  # variants in POST /api/ld/matrix
max_ld_proxies_per_job <- 1000L  # variants in POST /api/ld/proxies
max_ld_reflookup_per_job <- 10000L  # variants in POST /api/ld/reflookup
max_positions_per_job <- 1000L # positions in POST /api/variants/chrpos
max_range_analyses_per_job <- 10L        # Analyses in POST /api/associations/range
max_range_positions_per_job <- 10000000L # positions (inclusive) in one range
max_rows_per_job <- 100000L    # rows one Job's result may hold (errors guide: `request` failure beyond)
max_tophits_per_analysis <- 10000L  # `limit` in POST /api/tophits
max_phewas_per_variant <- 10000L    # `limit` in POST /api/phewas
max_queued_jobs_per_user <- 100L  # Jobs one user may have queued (429 user_queue_full)

# An answer the client can't trust: an unknown status, a result with no rows, pages that don't add up.
# Never papered over with a partial result. Kept: a Job-protocol invariant the API can't report.
job_invalid <- function(what, job_id = NULL, call = rlang::caller_env()) {
  og_abort(c("The OpenGWAS API's answer about a Job can't be trusted.", x = what,
             i = "This is a bug in opengwasr, or the API has changed. Please report it."),
           class = "opengwasr_job_invalid", job_id = job_id, call = call)
}

# The Job's status, which must be one of the API's six states.
job_state <- function(status, call = rlang::caller_env()) {
  state <- status$status
  known <- c(job_states_active, job_states_done)
  if (!is.character(state) || length(state) != 1 || !state %in% known) {
    # What the server sent is not echoed: it is not ours to put in a message.
    job_invalid(paste0("Its status is not one of ", paste(known, collapse = ", "), "."),
                status$job_id, call)
  }
  state
}

# Seconds this call may still use. Raises opengwasr_timeout when none are left.
job_remaining <- function(src, deadline, job_id, state, call) {
  remaining <- deadline - job_now()
  if (remaining <= 0) job_timed_out(src, job_id, state, call)
  remaining
}

# One request inside a call's budget. Its own timeout is what is left, and running out of it is the
# call's timeout, not an unreachable API.
job_call <- function(src, deadline, job_id, state, call, ...) {
  remaining <- job_remaining(src, deadline, job_id, state, call)
  resp <- tryCatch(
    api_response(src, ..., call = call, timeout = max(remaining, 0.001), deadline = deadline),
    # The API's 410 doesn't name the Job; the condition carries its id.
    opengwasr_result_expired = function(e) {
      e$job_id <- job_id
      rlang::cnd_signal(e)
    },
    opengwasr_unreachable = function(e) {
      if (job_now() >= deadline - 0.001) job_timed_out(src, job_id, state, call)
      rlang::cnd_signal(e)
    }
  )
  # An answer that arrives after the deadline is not accepted, whatever it says.
  if (job_now() > deadline) job_timed_out(src, job_id, state, call)
  resp
}

#' Submit a query and return the accepted Job.
#'
#' `ignored_fields` names fields opengwasr sent that the API didn't use, so the query ran differently
#' from the one asked for. That is opengwasr's bug or the API's drift, never something to ignore.
#'
#' @param path The query's endpoint, such as `"api/tophits"`.
#' @param body The query as a list.
#' @return A list: `job_id`, `status`, `existing`, `queue_position`, `poll_after_seconds`, and the
#'   `cost_estimate` the API sent in `X-OGDB-Cost-Estimate` (`NA` if it sent none).
#' @noRd
job_submit <- function(src, path, body, deadline = job_now() + src$timeout, call = rlang::caller_env()) {
  force(call)
  resp <- job_call(src, deadline, NULL, "not yet submitted", call, path, body = body)
  job <- api_parse(resp, call = call)
  if (!is.character(job$job_id) || length(job$job_id) != 1) {
    og_abort("The OpenGWAS API accepted the query but gave no Job id.", class = "opengwasr_api_error",
             status = httr2::resp_status(resp), api_error = NA_character_, call = call)
  }
  job_state(job, call)
  ignored <- api_redact(src, as.character(unlist(job$ignored_fields)))
  # Kept: the API accepts the query and only lists what it ignored, so opengwasr must refuse it.
  if (length(ignored) > 0) {
    og_abort(
      c("The API ignored part of the query, so it would not have answered the question asked.",
        x = paste0("Fields the API didn't use: ", paste0("`", ignored, "`", collapse = ", "), "."),
        i = "This is a bug in opengwasr, or the API has changed. Please report it."),
      class = "opengwasr_ignored_fields", ignored_fields = ignored, job_id = job$job_id, call = call
    )
  }
  # The header is the same number as the body's; either is enough.
  if (is.null(job$poll_after_seconds)) {
    job$poll_after_seconds <- suppressWarnings(as.numeric(httr2::resp_header(resp, "Retry-After", "1")))
  }
  job$cost_estimate <- suppressWarnings(as.numeric(httr2::resp_header(resp, "X-OGDB-Cost-Estimate", NA)))
  job
}

#' One look at a Job's status.
#' @noRd
job_status <- function(src, job_id, deadline = job_now() + src$timeout, call = rlang::caller_env()) {
  force(call)
  resp <- job_call(src, deadline, job_id, "unknown", call, paste0("api/jobs/", job_id))
  api_parse(resp, call = call)
}

# Seconds until the next look: the server's interval, growing by half again with each look, up to a
# cap (option `opengwasr.poll_cap`, 30 s; tests lower it). Never below the server's own interval,
# which the cap doesn't shorten, and never below 10 ms, so a server that says 0 is not hammered.
poll_delay <- function(poll_after, looks) {
  floor <- max(poll_after %||% 1, 0.01)
  min(max(getOption("opengwasr.poll_cap", 30), floor), floor * 1.5^looks)
}

#' Wait for a Job to finish.
#'
#' The call's deadline is hard: no request starts after it, each request's own timeout is what is
#' left, and no answer that arrives after it is accepted (checked in `job_call()`).
#'
#' @param job The accepted Job, or its latest status: a list with `job_id`, `status` and
#'   `poll_after_seconds`.
#' @param deadline When to give up, on the `job_now()` clock.
#' @return The finished Job's status (`succeeded`, with its `result`); anything else raises a
#'   classed error.
#' @noRd
job_wait <- function(src, job, deadline = job_now() + src$timeout, call = rlang::caller_env()) {
  force(call)
  # Progress: one status line (state, queue position, time waited), updated at every look.
  bar <- if (progress_enabled()) {
    cli::cli_progress_bar(format = "Job {job_short(job$job_id)} {cli::pb_extra$state} | {cli::pb_elapsed}",
                          extra = list(state = "submitted"), total = NA, clear = FALSE)
  }
  status <- job
  looks <- 0
  repeat {
    state <- job_state(status, call)
    if (!is.null(bar)) {
      at <- if (!is.null(status$queue_position)) paste0(" (position ", status$queue_position, ")")
      cli::cli_progress_update(id = bar, extra = list(state = paste0(state, at)), force = TRUE)
    }
    if (state == "succeeded") {
      # Reattaching to a finished Job: the submit answer says succeeded but holds no result.
      if (is.null(status$result)) status <- job_status(src, job$job_id, deadline, call)
      if (!identical(status$status, "succeeded") || !is.list(status$result)) {
        job_invalid("The Job succeeded but has no result to read.", job$job_id, call)
      }
      break
    }
    if (state %in% job_states_done) job_ended(src, status, call)
    remaining <- job_remaining(src, deadline, job$job_id, state, call)
    job_sleep(min(poll_delay(status$poll_after_seconds, looks), remaining))
    status <- job_status(src, job$job_id, deadline, call)
    looks <- looks + 1
  }
  status
}

# Kept: the call's deadline is opengwasr's own, so its message is too.
job_timed_out <- function(src, job_id, state, call) {
  og_abort(
    c(paste0("Gave up waiting after ", format(src$timeout), " s.",
             if (!is.null(job_id)) paste0(" Job ", job_id, " is still ", state, ".")),
      i = "Run the same call again: the API recognises the query and reattaches you to this Job.",
      i = if (!is.null(job_id)) job_advice(job_id)),
    class = "opengwasr_timeout", job_ids = job_id, timeout = src$timeout, status = state, call = call
  )
}

# How to let a Job go.
job_advice <- function(job_id) {
  paste0("Cancel it with `og_job_cancel(src, \"", job_id, "\")`.")
}

# What a user pressing Esc or Ctrl-C while the client waits gets: the Jobs are left as they are, and
# the error carries the `og_jobs` handle (when the query has one) to collect them with.
job_interrupted <- function(job_ids, jobs = NULL, call) {
  n <- length(job_ids)
  og_abort(c(paste0("Interrupted. ", if (n == 1) paste0("Job ", job_ids) else paste(n, "Jobs"),
                    if (n == 1) " is" else " are", " still running (or queued)."),
             i = if (!is.null(jobs) && !isTRUE(jobs$partial)) "Collect with `og_job_result(src, e$jobs)` in this session, or run the same call again: it reattaches."
                 else "Run the same call again: it reattaches to the Job.",
             i = paste0("Cancel with `og_job_cancel(src, ", if (n == 1) paste0("\"", job_ids, "\"") else "e$job_ids", ")`.")),
           class = "opengwasr_interrupted", job_ids = job_ids, jobs = jobs, call = call)
}

# A Job that ended without a result: failed (with the server's reason, scrubbed of the key),
# cancelled, or expired (the worker running it stopped responding on every attempt).
job_ended <- function(src, status, call) {
  id <- status$job_id
  if (identical(status$status, "failed")) {
    kind <- api_redact(src, as.character(status$error_kind %||% NA_character_))
    message <- api_redact(src, as.character(status$error %||% "no reason given"))
    # The API's reason is the message; `error_kind` says whether running it again may help.
    og_abort(message, class = "opengwasr_job_failed", job_id = id, error_kind = kind, api_message = message,
             retryable = identical(kind, "temporary"), call = call)
  }
  if (identical(status$status, "cancelled")) {
    og_abort(c(paste0("Job ", id, " was cancelled."), i = "Run the call again to run the query afresh."),
             class = "opengwasr_job_cancelled", job_id = id, call = call)
  }
  og_abort(c(paste0("Job ", id, " expired: the worker running it stopped responding."),
             i = "Run the call again to run the query afresh."),
           class = "opengwasr_job_expired", job_id = id, call = call)
}

#' Every row of a finished Job's result, page by page, in one pass.
#'
#' The API deletes a result a few minutes after its last page is read, so this reads all of them
#' without stopping, inside the call's deadline. The rows must add up to the `expected` count the
#' Job's manifest gave, or the result is refused.
#' @return A data frame (no columns when the result is empty).
#' @noRd
job_rows <- function(src, job_id, expected, deadline = job_now() + src$timeout,
                     call = rlang::caller_env()) {
  force(call)
  if (!is.numeric(expected) || length(expected) != 1 || is.na(expected)) {
    job_invalid("Its manifest gives no row count to check the result against.", job_id, call)
  }
  pages <- list()
  cursor <- NULL
  repeat {
    resp <- job_call(src, deadline, job_id, "being read", call,
                     paste0("api/jobs/", job_id, "/result"), query = list(cursor = cursor))
    page <- api_parse(resp, call = call)
    if (!is.character(page$ndjson) || length(page$ndjson) != 1) {
      job_invalid("A page of its result has no rows member (`ndjson`).", job_id, call)
    }
    pages[[length(pages) + 1]] <- ndjson_rows(page$ndjson, job_id, call)
    if (isTRUE(page$final)) break
    cursor <- page$next_cursor
    if (!is.character(cursor) || length(cursor) != 1) {
      job_invalid("Its result has more pages but gives no cursor to read them.", job_id, call)
    }
  }
  pages <- Filter(function(p) nrow(p) > 0, pages)
  rows <- if (length(pages) == 0) data.frame() else bind_frames(pages)
  if (nrow(rows) != expected) {
    job_invalid(paste0("Its manifest says ", expected, " rows but ", nrow(rows), " were read."), job_id, call)
  }
  rownames(rows) <- NULL
  rows
}

# One page's newline-delimited JSON as a data frame. A member that is null in some rows is NA.
ndjson_rows <- function(ndjson, job_id, call) {
  lines <- strsplit(ndjson, "\n", fixed = TRUE)[[1]]
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) return(data.frame())
  text <- paste0("[", paste(lines, collapse = ","), "]")
  # The parser's own message quotes the text it choked on, which is the server's, so it is not kept.
  rows <- tryCatch(jsonlite::fromJSON(text, simplifyVector = TRUE), error = function(e) NULL)
  if (!is.data.frame(rows)) job_invalid("A page of its result isn't valid newline-delimited JSON.", job_id, call)
  exact_numbers(rows, text)
}

# A member holding numbers in some rows and text in others (`eaf` and `n` hold ".", ogquery's marker
# for none) is made text by jsonlite, each number written with 15 significant digits, which loses
# the last digits of a double. Such a member (a text column holding ".") is read again from the
# page parsed as it is: its numbers exactly, and its text as a number, as jsonlite reads "NaN" or
# "Inf" ("." is NA). A member holding only text, such as `rsid`, is left as it is.
exact_numbers <- function(rows, text) {
  mixed <- names(rows)[vapply(rows, function(x) is.character(x) && any(x %in% "."), NA)]
  if (length(mixed) == 0) return(rows)
  raw <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  for (name in mixed) {
    values <- lapply(raw, `[[`, name)
    if (!any(vapply(values, is.numeric, NA))) next
    rows[[name]] <- vapply(values, function(v) {
      if (is.numeric(v)) as.double(v) else if (is.character(v)) suppressWarnings(as.double(v)) else NA_real_
    }, 0)
  }
  rows
}

# Say so when the API reattached a submission to a Job it already had.
job_reattached <- function(job) {
  if (isTRUE(job$existing) && progress_enabled()) {
    cli::cli_alert_info("Reattached to Job {job_short(job$job_id)} (already {job$status}).")
  }
}

# Wait for a submitted Job and read its rows. `job` is the submit answer, or that merged with a
# later status. Does not catch interrupts: its callers know what to tell the user.
collect_job <- function(src, job, deadline, call) {
  status <- job_wait(src, job, deadline, call = call)
  rows <- job_rows(src, job$job_id, status$result$manifest$row_count, deadline, call)
  metadata <- status$result$metadata
  list(
    rows = rows,
    job = list(
      job_id = job$job_id,
      existing = isTRUE(job$existing),
      cost_estimate = job$cost_estimate,
      total = metadata$total,
      truncated = metadata$truncated,
      metadata = metadata,  # whatever else the endpoint reports (an LD Job's `absent`, ...)
      releases = job_releases(status)
    )
  )
}

# The Store Releases a finished Job read, as "<store_id>/<release_id>".
job_releases <- function(status) {
  releases <- status$result$manifest$releases
  if (length(releases) > 0) {
    return(vapply(releases, function(r) paste0(r$store_id, "/", r$release_id), ""))
  }
  release_id <- status$release_id %||% status$result$release_id
  if (is.null(release_id)) character() else strsplit(release_id, ",", fixed = TRUE)[[1]]
}

# --- Progress -----------------------------------------------------------------------------------

# Progress is on in interactive sessions, and by option `opengwasr.progress` otherwise.
progress_enabled <- function() isTRUE(getOption("opengwasr.progress", interactive()))

job_short <- function(job_id) substr(job_id, 1, 8)
