#' @details
#' opengwasr reads OpenGWAS data through a *Source*. The API Source queries
#' the OpenGWAS API; a Local Source, planned for a later release, will read
#' Store Releases downloaded to your own machine (see `vignette("local-source")`). Query functions take the
#' Source as their first argument, so the same call works against either.
#'
#' The terms used throughout the package (Analysis, Association, Source,
#' Store Release and others) are defined in the
#' [glossary](https://github.com/opengwas/opengwasr/blob/main/GLOSSARY.md).
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang %||%
## usethis namespace: end
NULL
