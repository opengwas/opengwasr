# Building and performing an HTTP request, and mapping every API error response to a classed
# condition. The one request builder and the one error mapper.

# The user agent, looked up once per session (`packageVersion()` reads the DESCRIPTION file each time).
http_state <- new.env()
user_agent_string <- function(caller = NULL) {
  http_state$user_agent <- http_state$user_agent %||% paste0("opengwasr/", utils::packageVersion("opengwasr"))
  # A package calling opengwasr names itself (`og_api(caller =)`), so the API's logs can tell the tools apart.
  if (is.null(caller)) http_state$user_agent else paste(http_state$user_agent, caller)
}

api_request <- function(src, path, query = NULL, body = NULL, timeout = NULL, method = NULL) {
  req <- httr2::request(src$url) |>
    httr2::req_url_path_append(path) |>
    httr2::req_user_agent(user_agent_string(src$caller)) |>
    httr2::req_timeout(min(src$timeout, timeout %||% Inf)) |>
    httr2::req_error(is_error = function(resp) FALSE)
  if (!is.null(method)) req <- httr2::req_method(req, method)
  req <- api_authorise(src, req)
  if (length(query) > 0) req <- httr2::req_url_query(req, !!!query)
  if (!is.null(body)) {
    # An `r2` is a threshold an LD Store compares with its `t_far`: it must not round below it.
    if (is.list(body) && is.numeric(body$r2)) body$r2 <- round_up_15(body$r2)
    # 15 significant digits (`digits = NA`), not httr2's 22. R on Apple Silicon parses a literal such
    # as 1e-50 one ulp low, and 22 digits would send that ulp; 15 sends the same bytes everywhere,
    # which the vignette recordings (matched by body) rely on.
    req <- httr2::req_body_json(req, body, auto_unbox = TRUE, null = "null", digits = NA)
  }
  req
}

#' Perform one request, map an error response to its classed condition, and return the response
#' (status, headers, body). Callers that need a `202`, `Retry-After` or other headers (#6, #10) use
#' this; most callers use `api_perform()`.
#'
#' @param path A path under the API's base URL, such as `"api/gwasinfo"`.
#' @param body A list sent as a JSON body (a POST), or `NULL` (a GET).
#' @param timeout Seconds this one call may take, at most the Source's timeout. The Job client passes
#'   what is left of a call's overall budget.
#' @param method An HTTP method other than the default (`"DELETE"`).
#' @param deadline When the user's call must be over, on the `job_now()` clock. Every request is
#'   capped at what is left of it, and a `429` or `503` with `Retry-After` is waited out and tried
#'   again (at least a second later) if the wait ends before it; otherwise the call stops with
#'   `opengwasr_busy`. Defaults to the deadline the public call set (`local_query()`), or
#'   the Source's timeout from now.
#' @noRd
api_response <- function(src, path, query = NULL, body = NULL, call = rlang::caller_env(),
                         timeout = NULL, method = NULL, deadline = NULL) {
  force(call)
  if (inherits(src, "og_no_source")) source_required(call)
  deadline <- deadline %||% call_deadline() %||% (job_now() + src$timeout)
  repeat {
    budget <- deadline - job_now()
    if (budget <= 0) job_timed_out(src, NULL, "not yet sent", call)
    resp <- tryCatch(
      httr2::req_perform(api_request(src, path, query, body, max(min(timeout %||% Inf, budget), 0.001),
                                     method)),
      httr2_failure = function(e) {
        # Running out of the call's time is its timeout, not an unreachable API.
        if (job_now() >= deadline - 0.001) job_timed_out(src, NULL, "not yet answered", call)
        # No `parent`: the failure holds the authenticated request, and so the key.
        og_abort(c(paste0("Can't reach the OpenGWAS API at ", src$url, "."),
                   i = api_redact(src, conditionMessage(e))),
                 class = "opengwasr_unreachable", call = call)
      }
    )
    wait <- busy_wait(resp)
    if (is.na(wait)) break
    # The wait must fit in the call's time, or the call is over now rather than after it.
    if (job_now() + wait >= deadline) api_abort(src, resp, call, retry_after = wait)
    if (progress_enabled()) {
      cli::cli_alert_warning("The API is busy ({busy_code(src, resp)}): retrying in {format(wait)} s.")
    }
    job_sleep(wait)
  }
  # An answer that arrives after the deadline is not accepted, whatever it says.
  if (job_now() > deadline) job_timed_out(src, NULL, "not yet answered", call)
  if (httr2::resp_status(resp) >= 400) api_abort(src, resp, call)
  resp
}

# Seconds the API asks us to wait on a 429 or 503 (its `Retry-After`), `NA` for any other answer.
busy_wait <- function(resp) {
  if (!httr2::resp_status(resp) %in% c(429L, 503L)) return(NA_real_)
  wait <- suppressWarnings(as.numeric(httr2::resp_header(resp, "Retry-After", NA)))
  # A floor, so a Retry-After of 0 never becomes a loop of instant retries.
  if (length(wait) != 1 || is.na(wait) || wait < 0) NA_real_ else max(wait, 1)
}
# The API's code for a busy answer, scrubbed of the caller's key (it is the server's text).
busy_code <- function(src, resp) {
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  code <- if (is.list(body) && is.character(body$error) && length(body$error) == 1) body$error else "busy"
  api_redact(src, code)
}

#' Perform one request and return the parsed JSON body.
#' @param simplify Parse arrays of objects into data frames (`TRUE`) or keep lists (`FALSE`).
#' @noRd
api_perform <- function(src, path, query = NULL, body = NULL, simplify = FALSE,
                        call = rlang::caller_env()) {
  force(call)
  resp <- api_response(src, path, query, body, call)
  api_parse(resp, simplify, call)
}

# The JSON body of a successful response. A body that isn't JSON is an API error, not a parser error.
api_parse <- function(resp, simplify = FALSE, call = rlang::caller_env()) {
  force(call)
  tryCatch(
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = simplify),
    error = function(e) {
      og_abort(c("The OpenGWAS API answered with a body that isn't valid JSON.",
                 x = paste0("Status ", httr2::resp_status(resp), ".")),
               class = "opengwasr_api_error", status = httr2::resp_status(resp),
               api_error = NA_character_, call = call)
    }
  )
}

# Every API error becomes a condition whose message is the API's own (key redacted) and whose class
# is the API's code: `opengwasr_<code>`, for every well-formed code, listed here or not. This table
# adds the parents (ADR 0002's note of 22 Nov 2026), from the API's error catalogue (ogdb-api
# docs/guide/errors.qmd): `opengwasr_refusal` (the API understood the request and won't answer it
# exactly as asked) and `opengwasr_busy` (wait and try again). `og_abort()` adds `opengwasr_error`.
# Two rules sit beside the table: a 401 is `unauthorized` whatever its body (a proxy may answer in
# plain text), and a 429 or 503 whose code isn't listed is busy too. An answer with no code, or
# a body that isn't an error body, is `opengwasr_api_error`.
api_error_parents <- list(
  # A retired Analysis is also one that names no Analysis now, so one handler can catch both.
  analysis_retired = "opengwasr_analysis_not_found",
  unsupported_threshold = "opengwasr_refusal",
  query_too_broad = "opengwasr_refusal",
  request_too_large = "opengwasr_refusal",
  ld_refusal = "opengwasr_refusal",
  user_queue_full = "opengwasr_busy",
  queue_wait_too_long = "opengwasr_busy",
  results_quota_exceeded = "opengwasr_busy",
  server_busy = "opengwasr_busy",
  results_cache_full = "opengwasr_busy",
  intake_paused = "opengwasr_busy",
  job_store_unavailable = "opengwasr_busy",
  store_busy = "opengwasr_busy",
  store_timeout = "opengwasr_busy",
  search_busy = "opengwasr_busy"
)

# The classes of an error answer with `code` (or `NA`) and HTTP `status`.
api_error_class <- function(code, status) {
  busy <- if (status %in% c(429L, 503L)) "opengwasr_busy"
  if (is.na(code) || !grepl("^[a-z][a-z0-9_]*$", code)) return(c("opengwasr_api_error", busy))
  unique(c(paste0("opengwasr_", code), api_error_parents[[code]] %||% busy))
}

# Body members that are the condition's own (or `og_abort()`'s and `rlang::abort()`'s arguments),
# never copied from the API's body.
api_reserved_fields <- c("error", "message", "detail", "status", "api_error", "api_message", "api_body",
                         "call", "class", "parent", "trace", "footer", "body", "use_cli_format", "refusal",
                         ".internal", ".file", ".frame", ".trace_bottom", ".subclass")

# Raise the condition for an error response: the API's message, a class from its code, and the
# fields `status`, `api_error` (the code), `api_message`, `api_body` and every other member of the
# body (`superseded_by`, `indexed_thresholds`, `cost_estimate`, ...), all with the key scrubbed out.
api_abort <- function(src, resp, call = rlang::caller_env(), retry_after = NULL) {
  force(call)
  status <- httr2::resp_status(resp)
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (!is.list(body)) body <- list(message = trimws(httr2::resp_body_string(resp)))
  body <- api_redact(src, body)
  code <- if (is.character(body$error) && length(body$error) == 1) body$error else NA_character_
  # A 401 is the API's `unauthorized` whatever the body (a proxy may answer in plain text).
  if (status == 401) code <- "unauthorized"
  message <- if (is.character(body$message) && length(body$message) == 1 && nzchar(body$message)) body$message
             else httr2::resp_status_desc(resp) %||% paste("HTTP", status)
  class <- api_error_class(code, status)
  fields <- body[setdiff(names(body), api_reserved_fields)]
  fields <- fields[nzchar(names(fields))]
  fields <- c(fields, list(status = status, api_error = code, api_message = message, api_body = body))
  # The API names the candidates only in its message.
  if (identical(code, "ambiguous_analysis")) fields$candidates <- analysis_candidates(message)
  if ("opengwasr_busy" %in% class) fields$retry_after <- retry_after %||% busy_wait(resp)
  rlang::exec(og_abort, message, class = class, !!!fields, call = call)
}
