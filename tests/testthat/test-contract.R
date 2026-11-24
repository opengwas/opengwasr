# The fake keeps the Data API's contract: the same request schemas and the same response keys.
# The OpenAPI response schemas are open objects, so response shape comes from fixtures/shapes.json,
# recorded from the dev API. With credentials, the same checks run against the live API too, so a
# change on the API's side fails here rather than in a user's script.

fake <- local_fake_api()

check_responses <- function(base_url, key) {
  page <- raw_api(base_url, key, "api/gwasinfo", list(limit = 2))
  lookup <- raw_api(base_url, key, "api/gwasinfo", list(id = list("finn-b-I9_CHD", "ieu-a-2")))
  list(
    gwasinfo_page = missing_keys(page, "gwasinfo_page"),
    analysis_row = missing_keys(page$analyses[[1]], "analysis_row"),
    gwasinfo_lookup = missing_keys(lookup, "gwasinfo_lookup"),
    opengwas_entry = missing_keys(lookup$opengwas[[1]], "opengwas_entry"),
    batches = missing_keys(raw_api(base_url, key, "api/batches"), "batches"),
    batch_row = missing_keys(raw_api(base_url, key, "api/batches")$batches[[1]], "batch_row"),
    user = missing_keys(raw_api(base_url, key, "api/user"), "user"),
    status = missing_keys(raw_api(base_url, key, "api/status"), "status"),
    status_limits = missing_keys(raw_api(base_url, key, "api/status")$execution$limits, "status_limits")
  )
}

test_that("the fake's responses carry every key the real API's do", {
  problems <- check_responses(fake$url(), fake_key)
  expect_equal(problems, lapply(problems, function(p) character()))
})

test_that("the fake's retired entries carry the keys the real API's do", {
  retired <- raw_api(fake$url(), fake_key, "api/gwasinfo", list(id = list("OGS-00013:OLD1")))$retired[[1]]
  expect_equal(missing_keys(retired, "retired_entry"), character())
})

test_that("the fake's error bodies validate against the API's ErrorBody", {
  openapi <- read_fixture("openapi.json")
  unauthorised <- raw_api(fake$url(), "wrong", "api/user")
  expect_equal(error_body_problems(unauthorised, openapi), character())
  malformed <- raw_api(fake$url(), fake_key, "api/gwasinfo", list(releases = list("OGS-00011")))
  expect_equal(malformed$error, "invalid_request")
  expect_equal(error_body_problems(malformed, openapi), character())
})

test_that("the request schema rejects what the API rejects", {
  openapi <- read_fixture("openapi.json")
  valid <- list(id = list("a"), releases = list(list(store_id = "OGS-00011", release_id = "OGS-00011-completed")))
  expect_equal(request_body_problems(valid, "/api/gwasinfo", "post", openapi), character())
  expect_gt(length(request_body_problems(list(releases = list("OGS-00011")), "/api/gwasinfo", "post", openapi)), 0)
  expect_gt(length(request_body_problems(list(limit = 5000), "/api/gwasinfo", "post", openapi)), 0)
  expect_gt(length(request_body_problems(list(limit = "ten"), "/api/gwasinfo", "post", openapi)), 0)
  expect_gt(length(request_body_problems(list(id = 7), "/api/gwasinfo", "post", openapi)), 0)
})

test_that("the committed OpenAPI snapshot describes the endpoints og_analyses() uses", {
  openapi <- read_fixture("openapi.json")
  expect_true(all(c("/api/status", "/api/user", "/api/batches", "/api/gwasinfo") %in% names(openapi$paths)))
  expect_true(all(c("id", "limit", "after", "releases") %in% names(openapi$components$schemas$GwasInfoBody$properties)))
})

test_that("the live API still matches the contract the fake keeps", {
  skip_on_cran()
  skip_if_not(live_available(), "no dev Data API credentials")
  live_doc <- contract_openapi()
  snapshot <- read_fixture("openapi.json")
  expect_equal(live_doc$components$schemas$GwasInfoBody, snapshot$components$schemas$GwasInfoBody)
  expect_equal(live_doc$components$schemas$ReleaseReference, snapshot$components$schemas$ReleaseReference)
  expect_equal(live_doc$components$schemas$ErrorBody, snapshot$components$schemas$ErrorBody)
  live <- live_credentials()
  problems <- check_responses(live$url, live$key)
  expect_equal(problems, lapply(problems, function(p) character()))
})
