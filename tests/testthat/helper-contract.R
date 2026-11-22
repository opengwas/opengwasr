# The OpenGWAS API contract, as the fake keeps it.
#
# `fixtures/openapi.json` is a snapshot of the Data API's `/openapi.json`, refreshed by
# `fixtures/record-fixtures.R`. Its request bodies are typed, so the fake rejects any request that
# breaks them. Its response schemas are open objects, so `fixtures/shapes.json` records the keys of
# real responses and the fake must keep them. When credentials are present, `contract_openapi()`
# fetches the live document instead of using the snapshot.

source(testthat::test_path("raw-api.R"), local = TRUE)

contract_openapi <- function() {
  if (live_available()) {
    live <- live_credentials()
    return(raw_api(live$url, live$key, "openapi.json"))
  }
  read_fixture("openapi.json")
}

# Problems with an error body against `ErrorBody`.
error_body_problems <- function(body, openapi) {
  schema_problems(body, openapi$components$schemas$ErrorBody, openapi)
}

# Keys a response must carry, from `fixtures/shapes.json`.
missing_keys <- function(value, shape) {
  setdiff(read_fixture("shapes.json")[[shape]] |> unlist(), names(value))
}
