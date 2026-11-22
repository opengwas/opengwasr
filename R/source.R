#' Create the API Source
#'
#' The API Source answers queries through the OpenGWAS API. Every query function takes a Source as
#' its first argument.
#'
#' Credentials come from the environment: `OPENGWAS_URL` and `OPENGWAS_DEV_KEY`, with
#' `OPENGWAS_DEV_USER` naming the user (default `opengwasr-dev`). This developer access reaches the
#' Data API directly and is for developing opengwasr, not for analyses. The key is never printed.
#'
#' @param url The API's base URL. Defaults to the `opengwasr.url` option, then `OPENGWAS_URL`.
#' @param timeout Seconds to wait for any one call before giving up. Defaults to the
#'   `opengwasr.timeout` option, then 3600.
#' @param max_in_flight How many Jobs one call may run at once. Defaults to the
#'   `opengwasr.max_in_flight` option, then 4; at most 100, the Jobs one user may have queued.
#' @return An `og_api_source`.
#' @examples
#' \dontrun{
#' src <- og_api()
#' src
#' }
#' @export
og_api <- function(url = getOption("opengwasr.url"),
                   timeout = getOption("opengwasr.timeout", 3600),
                   max_in_flight = getOption("opengwasr.max_in_flight", 4)) {
  creds <- api_credentials(url)
  if (!is.numeric(timeout) || length(timeout) != 1 || is.na(timeout) || !is.finite(timeout) ||
      timeout <= 0) {
    og_abort("`timeout` must be one positive number of seconds.", class = "opengwasr_argument")
  }
  if (!is.numeric(max_in_flight) || length(max_in_flight) != 1 || is.na(max_in_flight) ||
      !is.finite(max_in_flight) || max_in_flight < 1 || max_in_flight > max_queued_jobs_per_user ||
      max_in_flight != round(max_in_flight)) {
    og_abort("`max_in_flight` must be one whole number from 1 to 100 (the Jobs one user may have queued).", class = "opengwasr_argument")
  }
  structure(
    list(
      url = creds$url,
      timeout = timeout,
      max_in_flight = as.integer(max_in_flight),
      signed_in_with = creds$scheme,
      user = creds$user,
      # A closure, so the key is not a visible field: str(), dput() and print() never show it.
      credentials = local({
        key <- creds$key
        function() list(key = key, user = creds$user)
      })
    ),
    class = "og_api_source"
  )
}

#' @export
format.og_api_source <- function(x, ...) {
  c(
    "<og_api_source>",
    paste0("\u2022 URL: ", x$url),
    paste0("\u2022 Signed in with: ", x$signed_in_with, " (as ", x$user, ")"),
    paste0("\u2022 Timeout: ", format(x$timeout), " s"),
    paste0("\u2022 Jobs in flight: at most ", x$max_in_flight)
  )
}

#' @export
print.og_api_source <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
