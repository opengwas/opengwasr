#' Check the API and your access
#'
#' `og_status()` reports whether the API is up and the limits it applies. `og_user()` reports who
#' you are signed in as, and your allowance.
#'
#' @param src A Source, from [og_api()].
#' @return A list (class `og_status` or `og_user`, with a print method). Read its fields in scripts:
#'   `status$ready`, `status$execution$limits`; `user$user`, `user$tier`, `user$allowance`,
#'   `user$concurrency_budget`.
#' @examples
#' \dontrun{
#' src <- og_api()
#' og_status(src)
#' og_user(src)
#' }
#' @export
og_status <- function(src) UseMethod("og_status")

#' @rdname og_status
#' @export
og_user <- function(src) UseMethod("og_user")

#' @export
og_status.og_api_source <- function(src) {
  structure(api_perform(src, "api/status"), class = c("og_status", "list"))
}

#' @export
og_user.og_api_source <- function(src) {
  structure(api_perform(src, "api/user"), class = c("og_user", "list"))
}

#' @export
print.og_status <- function(x, ...) {
  limits <- x$execution$limits
  cat("\u2500\u2500 OpenGWAS API \u2500\u2500\n")
  cat(if (isTRUE(x$ready)) "\u2714 Ready\n" else "\u2716 Not ready\n")
  if (!is.null(limits$result_rows)) {
    cat("\u2022 Most Associations one Job returns: ", format(limits$result_rows, big.mark = ","), "\n", sep = "")
  }
  if (!is.null(limits$result_unread_seconds)) {
    cat("\u2022 Unread Job results kept for: ", format_duration(limits$result_unread_seconds), "\n", sep = "")
  }
  queue <- limits$admission$principal_queue_capacity
  if (!is.null(queue)) cat("\u2022 Jobs one user may have queued: ", queue, "\n", sep = "")
  invisible(x)
}

#' @export
print.og_user <- function(x, ...) {
  cat("\u2500\u2500 Signed in \u2500\u2500\n")
  cat("\u2022 User: ", sub("^user:", "", x$user), "\n", sep = "")
  cat("\u2022 Tier: ", x$tier, "\n", sep = "")
  cat("\u2022 Allowance: ", format(x$allowance, big.mark = ","), "\n", sep = "")
  cat("\u2022 Jobs running at once: ", x$concurrency_budget, "\n", sep = "")
  invisible(x)
}

format_duration <- function(seconds) {
  if (seconds %% 3600 == 0) paste0(seconds / 3600, " h")
  else if (seconds %% 60 == 0) paste0(seconds / 60, " min")
  else paste0(seconds, " s")
}
