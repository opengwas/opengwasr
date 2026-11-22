# Building and performing an HTTP request, and mapping every API error response to a classed
# condition. The one request builder and the one error mapper.

api_request <- function(src, path, query = NULL, body = NULL) {
  req <- httr2::request(src$url) |>
    httr2::req_url_path_append(path) |>
    httr2::req_user_agent(paste0("opengwasr/", utils::packageVersion("opengwasr"))) |>
    httr2::req_timeout(src$timeout) |>
    httr2::req_error(is_error = function(resp) FALSE)
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
#' @noRd
api_response <- function(src, path, query = NULL, body = NULL, call = rlang::caller_env()) {
  force(call)
  resp <- tryCatch(
    httr2::req_perform(api_request(src, path, query, body)),
    httr2_failure = function(e) {
      # No `parent`: the failure holds the authenticated request, and so the key.
      og_abort(c(paste0("Can't reach the OpenGWAS API at ", src$url, "."),
                 i = api_redact(src, conditionMessage(e))),
               class = "opengwasr_unreachable", call = call)
    }
  )
  if (httr2::resp_status(resp) >= 400) api_abort(src, resp, call)
  resp
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
# `opengwasr_api_error`. #6 adds the busy (429, 503), too-large (413) and result-expired (410) rows,
# and #10 the LD refusal (422).
api_error_classes <- list(
  unauthorized = "opengwasr_auth",
  analysis_not_found = "opengwasr_unknown_analysis",
  analysis_retired = c("opengwasr_retired_analysis", "opengwasr_unknown_analysis"),
  ambiguous_analysis = "opengwasr_ambiguous_analysis",
  release_not_found = "opengwasr_unknown_release",
  release_selection_required = "opengwasr_release_selection_required"
)

# Raise the classed condition for an error response. Every condition carries `status`,
# `api_error` (the API's code), `api_message` and `api_body`, so callers can read the server's
# reason, with this Source's key scrubbed out of them.
api_abort <- function(src, resp, call = rlang::caller_env()) {
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

  message <- switch(class[[1]],
    opengwasr_auth = c("The OpenGWAS API didn't accept your key.", i = auth_advice(),
                       x = paste0("API: ", api_message)),
    opengwasr_ambiguous_analysis = c("An identifier names more than one Analysis.",
                                     x = paste0("API: ", api_message)),
    c(paste0("The OpenGWAS API refused the request (", status, ")."), x = paste0("API: ", api_message))
  )
  fields <- list(status = status, api_error = code, api_message = api_message, api_body = body)
  if (identical(code, "ambiguous_analysis")) fields$candidates <- analysis_candidates(api_message)
  if (identical(code, "analysis_retired")) fields$superseded_by <- body$superseded_by
  if (identical(class[[1]], "opengwasr_auth")) fields$setting <- auth_setting()
  rlang::exec(og_abort, message, class = class, !!!fields, call = call)
}
