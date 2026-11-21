#' @details
#' opengwasr reads OpenGWAS data through a *Source*. The API Source queries
#' the OpenGWAS API; a Local Source, planned for a later release, will read
#' Store Releases and LD Stores downloaded to your own machine. Query functions take the
#' Source as their first argument, so the same call works against either.
#'
#' The terms used throughout the package (Analysis, Association, Source,
#' Store Release and others) are defined in the
#' [glossary](https://github.com/opengwas/opengwasr/blob/main/GLOSSARY.md).
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
# These hold the Imports until real code uses each package; remove each as it does.
#' @importFrom cli cli_abort
#' @importFrom httr2 request
#' @importFrom jsonlite fromJSON
#' @importFrom rlang abort
## usethis namespace: end
NULL
