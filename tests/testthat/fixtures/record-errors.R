# Regenerates fixtures/errors.json: the Data API's real error bodies, which the fake (and the tests'
# mocked responses) send back verbatim. Run by hand, with credentials in the environment:
#   R_ENVIRON_USER=/path/to/.Renviron Rscript tests/testthat/fixtures/record-errors.R
#
# `recorded` errors are provoked on the dev Data API. The others can't be provoked on demand (the dev
# API holds no ambiguous or retired Analysis, and refuses nothing as too large or busy), so their
# bodies are copied from the API's source, with example values; each says which file and commit.
# No body holds the caller's key.
source("tests/testthat/raw-api.R")
live <- live_credentials()
release <- function(store, id = paste0(store, "-completed")) list(list(store_id = store, release_id = id))
provoke <- function(path, body = NULL, key = live$key) {
  req <- httr2::request(live$url) |>
    httr2::req_url_path_append(path) |>
    httr2::req_auth_bearer_token(key) |>
    httr2::req_headers(`X-OGDB-User` = "opengwasr-dev") |>
    httr2::req_error(is_error = function(resp) FALSE)
  if (!is.null(body)) req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp)
  stopifnot(httr2::resp_status(resp) >= 400, !grepl(live$key, jsonlite::toJSON(body), fixed = TRUE))
  list(status = httr2::resp_status(resp), source = "dev Data API", body = body)
}
tophits <- function(...) {
  body <- list(analyses = list("OGS-00011:GCST003116"), threshold = 5e-8, limit = 1L, releases = release("OGS-00011"))
  body[names(list(...))] <- list(...)   # replaced whole (utils::modifyList would merge the lists)
  body
}
recorded <- list(
  unauthorized = provoke("api/user", key = "not-a-key"),
  invalid_request = provoke("api/tophits", tophits(limit = 20000L)),
  invalid_query = provoke("api/tophits", tophits(ids = list("OGS-00011:GCST90029007"))),
  analysis_not_found = provoke("api/tophits", tophits(analyses = list("OGS-00011:NOT-AN-ANALYSIS"))),
  release_not_found = provoke("api/tophits", tophits(releases = release("OGS-00011", "nonesuch"))),
  unsupported_threshold = provoke("api/tophits", tophits(threshold = 1e-3)),
  not_found = provoke("api/jobs/00000000-0000-0000-0000-000000000000"),
  ld_refusal = provoke("api/tophits", tophits(limit = 5L, clump = list(r2 = 0.001, kb = 10000L, ancestry = "EUR")))
)

# The ogdb-api checkout the copied bodies come from, and its commit.
ogdb_api <- Sys.getenv("OGDB_API_SOURCE", "/data/ogdb-apps/ogdb-api")
commit <- system2("git", c("-c", paste0("safe.directory=", ogdb_api), "-C", ogdb_api, "log", "-1", "--format=%h"),
                  stdout = TRUE)
from_source <- function(status, body, file) {
  list(status = status, source = paste0("ogdb-api ", file, " @ ", commit), body = body)
}
# HTTPException bodies repeat themselves under `detail` (errors.py `error_body()`); JSONResponse ones don't.
detailed <- function(body) c(body, list(detail = body))
copied <- list(
  ambiguous_analysis = from_source(409, detailed(list(error = "ambiguous_analysis",
    message = "analysis ID GCST90000001 is in several Stores; name one of: OGS-00011:GCST90000001, OGS-00012:GCST90000001")),
    "ogdb_api/registry.py, errors.py"),
  analysis_retired = from_source(410, detailed(list(error = "analysis_retired", message = "OGS-00013:OLD1",
    superseded_by = list("OGS-00014:NEW1"))), "ogdb_api/registry.py, errors.py"),
  user_required = from_source(400, detailed(list(error = "user_required",
    message = "Gateway requests to data routes must send X-OGDB-User")), "ogdb_api/auth.py"),
  release_selection_required = from_source(409, detailed(list(error = "release_selection_required",
    message = "an explicit release selection or analysis IDs are required")), "ogdb_api/planner.py, errors.py"),
  result_expired = from_source(410, detailed(list(error = "result_expired", message = "Job result has expired")),
    "ogdb_api/main.py"),
  query_too_broad = from_source(400, detailed(list(error = "query_too_broad",
    message = "the search needs more than 200,000 steps; narrow it with a longer q or a filter")),
    "ogdb_api/main.py, catalog.py"),
  request_too_large = from_source(413, list(error = "request_too_large",
    message = paste("The job is estimated at 3479 CPU-seconds, more than the 300 one job may use;",
                    "narrow it (fewer analyses, a shorter range) or split it"),
    cost_estimate = 3479, max_cost = 300), "ogdb_api/main.py"),
  user_queue_full = from_source(429, list(error = "user_queue_full",
    message = "You have 100 queued jobs, the most one user may queue; wait for some to run, or cancel some",
    queued = 100, max_queued = 100), "ogdb_api/main.py"),
  queue_wait_too_long = from_source(429, list(error = "queue_wait_too_long",
    message = "The job would wait about 1900 s to start, longer than the queue's maximum age of 1800 s; resubmit later",
    jobs_ahead = 40, expected_wait_seconds = 1900, max_wait_seconds = 1800, poll_after_seconds = 100),
    "ogdb_api/main.py"),
  server_busy = from_source(429, list(error = "server_busy", message = "Service capacity is temporarily full"),
    "ogdb_api/main.py"),
  results_quota_exceeded = from_source(429, list(error = "results_quota_exceeded",
    message = "Your stored results fill your share of the results cache: read them, or wait for them to expire",
    results_bytes = 1073741824, results = 12, max_bytes = 1073741824, poll_after_seconds = 60), "ogdb_api/main.py"),
  results_cache_full = from_source(503, list(error = "results_cache_full",
    message = "The results cache is full until stored results are retrieved or expire", poll_after_seconds = 60),
    "ogdb_api/main.py"),
  intake_paused = from_source(503, list(error = "intake_paused", retry_after_seconds = 30, mode = "paused",
    message = "The Data API is not accepting new jobs"), "ogdb_api/main.py"),
  job_store_unavailable = from_source(503, list(error = "job_store_unavailable",
    message = "The job store is unavailable; retry later", retry_after_seconds = 15), "ogdb_api/errors.py"),
  store_busy = from_source(503, list(error = "store_busy", message = "store backend capacity is full",
    retry_after_seconds = 15), "ogdb_api/errors.py, planner.py"),
  store_timeout = from_source(503, list(error = "store_timeout",
    message = "store query timed out: OGS-00011-completed", retry_after_seconds = 15), "ogdb_api/errors.py, planner.py"),
  search_busy = from_source(503, list(error = "search_busy", retry_after_seconds = 2,
    message = "the search could not finish in 5 s because the server is busy; retry after Retry-After"),
    "ogdb_api/main.py, catalog.py"),
  # Codes the class table doesn't list: they get `opengwasr_<code>` all the same.
  release_unavailable = from_source(503, detailed(list(error = "release_unavailable",
    message = "release OGS-00011/OGS-00011-completed failed to load")), "ogdb_api/main.py"),
  invalid_cursor = from_source(400, detailed(list(error = "invalid_cursor", message = "Invalid result cursor")),
    "ogdb_api/main.py")
)
jsonlite::write_json(c(recorded, copied), "tests/testthat/fixtures/errors.json",
                     auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
