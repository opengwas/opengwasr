# Building and performing an HTTP request, and mapping every API error response to a classed
# condition. The one request builder and the one error mapper.

api_request <- function(src, path, query = NULL, body = NULL, timeout = NULL, method = NULL) {
  req <- httr2::request(src$url) |>
    httr2::req_url_path_append(path) |>
    httr2::req_user_agent(paste0("opengwasr/", utils::packageVersion("opengwasr"))) |>
    httr2::req_timeout(min(src$timeout, timeout %||% Inf)) |>
    httr2::req_error(is_error = function(resp) FALSE)
  if (!is.null(method)) req <- httr2::req_method(req, method)
  req <- api_authorise(src, req)
  if (length(query) > 0) req <- httr2::req_url_query(req, !!!query)
  if (!is.null(body)) req <- httr2::req_body_json(req, body, auto_unbox = TRUE, null = "null")
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
    if (job_now() + wait >= deadline) api_abort(src, resp, call, retry_after = wait, out_of_time = TRUE)
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

# The API's error catalogue (docs/guide/errors.qmd) as classes. A code not listed here becomes
# `opengwasr_api_error`. #6 added the unsupported-threshold (400) and result-expired (410) rows;
# #10 added the busy (429, 503) rows; #9 adds the too-large (413) and LD refusal (422) rows.
api_error_classes <- list(
  unauthorized = "opengwasr_auth",
  analysis_not_found = "opengwasr_unknown_analysis",
  analysis_retired = c("opengwasr_retired_analysis", "opengwasr_unknown_analysis"),
  ambiguous_analysis = "opengwasr_ambiguous_analysis",
  release_not_found = "opengwasr_unknown_release",
  release_selection_required = "opengwasr_release_selection_required",
  unsupported_threshold = c("opengwasr_unsupported_threshold", "opengwasr_refusal"),
  result_expired = "opengwasr_result_expired",
  request_too_large = c("opengwasr_request_too_large", "opengwasr_refusal"),
  user_queue_full = "opengwasr_busy", queue_wait_too_long = "opengwasr_busy",
  results_quota_exceeded = "opengwasr_busy", server_busy = "opengwasr_busy",
  results_cache_full = "opengwasr_busy", intake_paused = "opengwasr_busy",
  job_store_unavailable = "opengwasr_busy"
)

# Raise the classed condition for an error response. Every condition carries `status`,
# `api_error` (the API's code), `api_message` and `api_body`, so callers can read the server's
# reason, with this Source's key scrubbed out of them.
api_abort <- function(src, resp, call = rlang::caller_env(), retry_after = NULL, out_of_time = FALSE) {
  force(call)
  status <- httr2::resp_status(resp)
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (!is.list(body)) body <- list(message = trimws(httr2::resp_body_string(resp)))
  body <- api_redact(src, body)
  code <- body$error %||% NA_character_
  api_message <- body$message %||% httr2::resp_status_desc(resp)
  class <- api_error_classes[[code]] %||% "opengwasr_api_error"
  # A 401 is an auth failure whatever the body (a proxy may answer in plain text).
  if (status == 401) class <- "opengwasr_auth"
  # A 429 or 503 the API didn't name (or the Gateway's own allowance) is still "busy".
  if (status %in% c(429L, 503L) && class[[1]] == "opengwasr_api_error") class <- "opengwasr_busy"

  message <- switch(class[[1]],
    opengwasr_auth = c("The OpenGWAS API didn't accept your key.", i = auth_advice(),
                       x = paste0("API: ", api_message)),
    opengwasr_ambiguous_analysis = c("An identifier names more than one Analysis.",
                                     x = paste0("API: ", api_message)),
    opengwasr_unsupported_threshold = c(paste0("The API refused `threshold`: ", api_message, "."),
                                        i = "Each GWAS Store indexes 5e-08, 5e-06 and 5e-04."),
    opengwasr_busy = c(paste0("The API is busy (", if (is.na(code)) status else code, ")",
                              if (isTRUE(out_of_time)) paste0(" and asks for ", format(retry_after), " s, which is more than is left of this call's time")
                              else if (is.null(retry_after)) " and didn't say when to try again" else "", "."),
                       i = if (isTRUE(out_of_time)) "Try again later, or raise `og_api(timeout = )` so the call can wait longer."
                           else "Try again in a little while.",
                       x = paste0("API: ", api_message)),
    # The API words its own refusal (it says where to download the Store Release): never ours.
    opengwasr_request_too_large = api_message,
    opengwasr_result_expired = c("The Job's result is no longer available: it was already collected, or it expired.",
                                 i = "Run the query again; the API will run it afresh.",
                                 x = paste0("API: ", api_message)),
    c(paste0("The OpenGWAS API refused the request (", status, ")."), x = paste0("API: ", api_message))
  )
  fields <- list(status = status, api_error = code, api_message = api_message, api_body = body)
  if (identical(code, "ambiguous_analysis")) fields$candidates <- analysis_candidates(api_message)
  if (identical(class[[1]], "opengwasr_request_too_large")) {
    fields$cost_estimate <- suppressWarnings(as.numeric(body$cost_estimate %||% NA))
    fields$max_cost <- suppressWarnings(as.numeric(body$max_cost %||% NA))
  }
  if (identical(code, "analysis_retired")) fields$superseded_by <- body$superseded_by
  if (identical(class[[1]], "opengwasr_busy")) fields$retry_after <- retry_after %||% busy_wait(resp)
  if (identical(class[[1]], "opengwasr_auth")) fields$setting <- auth_setting()
  rlang::exec(og_abort, message, class = class, !!!fields, call = call)
}

