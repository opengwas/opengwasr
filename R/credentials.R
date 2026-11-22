# Reading credentials and the base URL: the only code that reads OPENGWAS_URL, OPENGWAS_DEV_KEY,
# OPENGWAS_DEV_USER and the `opengwasr.url` option.
#
# Interim scheme (21 Nov 2026): talk to the Data API directly with a developer key, sent as the
# Bearer token, and name the user in X-OGDB-User. Gateway authentication (OPENGWAS_JWT) will be
# added here and nowhere else.

#' @param url The base URL, or `NULL` to read it from the environment.
#' @return A list: `url`, `key`, `user`, and `scheme`, the name of the setting that supplied the key.
#' @noRd
api_credentials <- function(url = NULL, call = rlang::caller_env()) {
  url <- url %||% Sys.getenv("OPENGWAS_URL")
  key <- Sys.getenv("OPENGWAS_DEV_KEY")
  if (!nzchar(url)) {
    og_abort(
      c("No OpenGWAS API URL found.",
        i = "Set `OPENGWAS_URL` in your .Renviron, or the `opengwasr.url` option, then restart R."),
      class = "opengwasr_auth", setting = "OPENGWAS_URL", call = call
    )
  }
  if (!nzchar(key)) {
    og_abort(
      c("No OpenGWAS key found.", i = auth_advice()),
      class = "opengwasr_auth", setting = auth_setting(), call = call
    )
  }
  user <- Sys.getenv("OPENGWAS_DEV_USER")
  list(
    url = sub("/+$", "", url),
    key = key,
    user = if (nzchar(user)) user else "opengwasr-dev",
    scheme = "OPENGWAS_DEV_KEY"
  )
}

# The setting that supplies the key, named in auth errors (`e$setting`) and advice.
auth_setting <- function() "OPENGWAS_DEV_KEY"

# What to tell a user whose key is missing or refused.
auth_advice <- function() {
  paste0("Set `", auth_setting(), "` in your .Renviron to your developer key, then restart R.")
}

#' Add this Source's authentication to a request: the Bearer key and the named user.
#' @noRd
api_authorise <- function(src, req) {
  creds <- src$credentials()
  req |>
    httr2::req_auth_bearer_token(creds$key) |>
    httr2::req_headers(`X-OGDB-User` = creds$user)
}

#' Replace this Source's key wherever it appears in `x` (a string, or a list of them, names included), so that a
#' server that echoes the key back can't put it into a condition.
#' @noRd
api_redact <- function(src, x) {
  key <- src$credentials()$key
  scrub <- function(text) gsub(key, "<redacted>", text, fixed = TRUE)
  if (is.character(x)) return(scrub(x))
  if (is.list(x)) {
    # Member names too: a server could echo the key as a JSON member name.
    nm <- names(x)
    x[] <- lapply(x, function(item) api_redact(src, item))
    if (!is.null(nm)) names(x) <- scrub(nm)
  }
  x
}
