# Vignettes replay recordings of the development Data API, so they build offline with no credentials
# (CRAN, CI). Each vignette's hidden setup chunk sources this file.
#
#   * Replaying (the default): `vignette_credentials()` sets placeholder credentials for the dev
#     Data API's URL. `start_recordings()` replays the responses saved in `vignettes/<name>/`.
#   * Recording: set OPENGWASR_RECORD=1, with the real OPENGWAS_URL and OPENGWAS_DEV_KEY in the
#     environment (for example from the gitignored `.Renviron`), delete `vignettes/<name>/`, and
#     render the vignette. Then grep the new files for the key before committing.
#
# Recordings hold response bodies only. The request's Authorization and X-OGDB-User headers are not
# saved, and `redact_response()` strips them if httptest2 ever does. Catalogue pages are cut down to
# the columns the vignettes use, and /api/batches loses its validation warnings, to keep the package
# small. The vignettes read one bounded GWAS Store, not the whole 38,152-row catalogue.

recording <- nzchar(Sys.getenv("OPENGWASR_RECORD"))

vignette_credentials <- function() {
  if (recording) return(invisible())
  old <- Sys.getenv(c("OPENGWAS_URL", "OPENGWAS_DEV_KEY", "OPENGWAS_DEV_USER"), unset = NA)
  options(opengwasr.vignette_env = old)
  Sys.setenv(OPENGWAS_URL = "http://localhost:6433", OPENGWAS_DEV_KEY = "replayed-from-recordings")
  Sys.unsetenv("OPENGWAS_DEV_USER")
  options(opengwasr.url = NULL)
  invisible()
}

catalogue_columns <- c("id", "store_id", "analysis_id", "analysis_label", "assigned_ancestry",
                       "sample_size", "outcome_type", "release_id")

# httptest2 passes both requests and responses through the redactor.
redact_response <- function(response) {
  response <- httptest2::redact_headers(response, c("Authorization", "X-OGDB-User", "Set-Cookie"))
  if (!inherits(response, "httr2_response")) return(response)
  if (!identical(httr2::resp_content_type(response), "application/json")) return(response)
  body <- jsonlite::fromJSON(httr2::resp_body_string(response), simplifyVector = FALSE)
  if (is.list(body) && all(c("offset", "limit", "analyses") %in% names(body))) {
    body$analyses <- lapply(body$analyses, function(a) a[intersect(names(a), catalogue_columns)])
    response$body <- charToRaw(as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")))
  }
  if (is.list(body) && "batches" %in% names(body)) {
    # The per-Analysis validation warnings are most of /api/batches, and nothing here reads them.
    body$batches <- lapply(body$batches, function(b) b[setdiff(names(b), c("validation_warnings", "smoke_request"))])
    response$body <- charToRaw(as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")))
  }
  response
}

start_recordings <- function(name) {
  vignette_credentials()
  httptest2::set_redactor(redact_response)
  httptest2::start_vignette(name)
}

end_recordings <- function() {
  httptest2::end_vignette()
  old <- getOption("opengwasr.vignette_env")
  if (!is.null(old)) {
    for (name in names(old)) {
      if (is.na(old[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, stats::setNames(list(old[[name]]), name))
    }
  }
  invisible()
}
