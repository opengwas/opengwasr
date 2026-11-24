# Test-only raw transport: an authenticated call to a Data API that doesn't go through opengwasr, so
# contract checks and fixture recording stay independent of the package's own decoding. Self-contained
# (httr2 only); sourced by helper-contract.R and fixtures/record-fixtures.R.

# The dev Data API's URL and key from the environment: the only reader the tests and the fixture
# script share.
live_credentials <- function() {
  list(url = Sys.getenv("OPENGWAS_URL"), key = Sys.getenv("OPENGWAS_DEV_KEY"))
}

# `parse = FALSE` returns the body text, for fixtures kept verbatim.
raw_api <- function(url, key, path, body = NULL, user = "opengwasr-dev", parse = TRUE) {
  req <- httr2::request(url) |>
    httr2::req_url_path_append(path) |>
    httr2::req_auth_bearer_token(key) |>
    httr2::req_headers(`X-OGDB-User` = user) |>
    httr2::req_error(is_error = function(resp) FALSE)
  if (!is.null(body)) req <- httr2::req_body_json(req, body)
  resp <- httr2::req_perform(req)
  if (parse) httr2::resp_body_json(resp) else httr2::resp_body_string(resp)
}
