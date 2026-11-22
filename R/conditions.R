# Classed conditions: the one place that decides what class a condition has.
#
# Every error has class `opengwasr_error`; a refusal (an option that can't be honoured exactly)
# also has `opengwasr_refusal`. Tests and callers match on class, never on message text.

#' Raise an opengwasr error
#'
#' @param message A character vector for [rlang::abort()] (names `i`, `x`, `!` make bullets).
#' @param class The condition's own classes, most specific first.
#' @param ... Fields carried on the condition (`e$ids`, `e$api_error`, ...).
#' @param refusal Add class `opengwasr_refusal`.
#' @noRd
og_abort <- function(message, class = NULL, ..., refusal = FALSE, call = rlang::caller_env(),
                     parent = NULL) {
  rlang::abort(
    message,
    class = c(class, if (refusal) "opengwasr_refusal", "opengwasr_error"),
    ...,
    call = call,
    parent = parent
  )
}

#' Raise a classed opengwasr warning. Warnings are not errors, so no `opengwasr_error` class.
#' @noRd
og_warn <- function(message, class, ..., call = rlang::caller_env()) {
  rlang::warn(message, class = class, ..., call = call)
}
