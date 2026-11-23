shown_source <- function(src) paste(format(src), collapse = "\n")

fake <- local_fake_api()

test_that("og_api() builds an API Source from the environment", {
  src <- withr::with_envvar(
    c(OPENGWAS_URL = "http://example.test:1", OPENGWAS_DEV_KEY = "k-123456789012-secret",
      OPENGWAS_DEV_USER = NA),
    og_api()
  )
  expect_s3_class(src, "og_api_source")
})

test_that("og_api() takes the URL from the option, then the environment", {
  withr::local_envvar(OPENGWAS_URL = "http://from-env.test", OPENGWAS_DEV_KEY = "k", OPENGWAS_DEV_USER = NA)
  expect_match(shown_source(og_api()), "http://from-env.test", fixed = TRUE)
  withr::local_options(opengwasr.url = "http://from-option.test")
  expect_match(shown_source(og_api()), "http://from-option.test", fixed = TRUE)
  expect_match(shown_source(og_api(url = "http://from-arg.test")), "http://from-arg.test", fixed = TRUE)
})

test_that("a missing key raises opengwasr_auth, with advice", {
  withr::local_envvar(OPENGWAS_URL = "http://example.test", OPENGWAS_DEV_KEY = NA)
  err <- expect_error(og_api(), class = "opengwasr_auth")
  expect_s3_class(err, "opengwasr_error")
  expect_equal(err$setting, "OPENGWAS_DEV_KEY")
})

test_that("a missing URL raises opengwasr_auth, with advice", {
  withr::local_envvar(OPENGWAS_URL = NA, OPENGWAS_DEV_KEY = "k")
  withr::local_options(opengwasr.url = NULL)
  err <- expect_error(og_api(), class = "opengwasr_auth")
  expect_equal(err$setting, "OPENGWAS_URL")
})

test_that("printing a Source never shows the key", {
  key <- "k-123456789012-secret"
  src <- withr::with_envvar(
    c(OPENGWAS_URL = "http://example.test:1", OPENGWAS_DEV_KEY = key, OPENGWAS_DEV_USER = NA),
    og_api(timeout = 600, max_in_flight = 2)
  )
  shown <- paste(c(format(src), capture.output(print(src)), capture.output(str(src)), deparse(src)), collapse = "\n")
  expect_false(grepl(key, shown, fixed = TRUE))
  expect_match(shown, "http://example.test:1", fixed = TRUE)
  expect_match(shown, "600")
})

test_that("an options-set timeout and max_in_flight reach the Source", {
  withr::local_options(opengwasr.timeout = 77, opengwasr.max_in_flight = 3)
  withr::local_envvar(OPENGWAS_URL = "http://x.test", OPENGWAS_DEV_KEY = "k")
  shown <- shown_source(og_api())
  expect_match(shown, "77 s")
  expect_match(shown, "at most 3")
})

# Serialise a condition every way a user or a log might, and look for `key` in it.
condition_text <- function(cnd) {
  paste(c(capture.output(str(cnd)), capture.output(print(cnd)), conditionMessage(cnd),
          rawToChar(serialize(cnd, NULL, ascii = TRUE))), collapse = "\n")
}

test_that("a server that echoes the key in a 401's message, as a member name or in a reason leaves it nowhere", {
  fake_set(fake, "leak", "true", "false")
  key <- paste0("k-", "123456789012", "-secret-member")  # built, so the test source doesn't hold it
  err <- expect_error(og_user(fake_source(fake, key = key)), class = "opengwasr_unauthorized")
  expect_false(grepl(key, condition_text(err), fixed = TRUE))
  expect_match(err$api_message, "<redacted>", fixed = TRUE)
  expect_false(any(grepl(key, names(err$api_body), fixed = TRUE)))
  # A key the fake accepts, echoed in a successful lookup's reason (a plain 200).
  err <- expect_error(og_analyses(fake_source(fake), "ieu-a-2"), class = "opengwasr_analysis_not_found")
  expect_false(grepl(fake_key, condition_text(err), fixed = TRUE))
  expect_match(err$api_reasons[["ieu-a-2"]], "<redacted>", fixed = TRUE)
})

test_that("a 401 is opengwasr_unauthorized whatever its body", {
  fake_set(fake, "plain401", "true", "false")
  err <- expect_error(og_user(fake_source(fake, key = "wrong")), class = "opengwasr_unauthorized")
  expect_equal(c(err$status, err$api_error), c("401", "unauthorized"))
})

test_that("a connection failure leaves the key nowhere in the condition", {
  key <- paste0("k-", "123456789012", "-secret-unreachable")
  src <- withr::with_envvar(
    c(OPENGWAS_URL = "http://127.0.0.1:1", OPENGWAS_DEV_KEY = key, OPENGWAS_DEV_USER = NA),
    og_api(timeout = 5)
  )
  err <- expect_error(og_status(src), class = "opengwasr_unreachable")
  expect_s3_class(err, "opengwasr_error")
  expect_false(grepl(key, condition_text(err), fixed = TRUE))
})

test_that("a successful response that isn't JSON raises an opengwasr_api_error", {
  fake_set(fake, "malformed", "true", "false")
  err <- expect_error(og_user(fake_source(fake)), class = "opengwasr_api_error")
  expect_s3_class(err, "opengwasr_error")
  expect_equal(err$status, 200L)
})

test_that("og_api() refuses a timeout or max_in_flight that isn't positive, or a fractional count", {
  withr::local_envvar(OPENGWAS_URL = "http://x.test", OPENGWAS_DEV_KEY = "k")
  for (bad in list(1.9, 0, -1, NA_real_, "4", c(1, 2), Inf, 2^31)) {
    expect_error(og_api(max_in_flight = bad), class = "opengwasr_argument")
  }
  for (bad in list(0, -5, NA_real_, "60", c(1, 2), Inf)) {
    expect_error(og_api(timeout = bad), class = "opengwasr_argument")
  }
  expect_s3_class(og_api(timeout = 0.5, max_in_flight = 2), "og_api_source")
})

test_that("the dev user is sent in X-OGDB-User, defaulting to opengwasr-dev", {
  expect_equal(og_user(fake_source(fake))$user, "user:opengwasr-dev")
  src <- withr::with_envvar(
    c(OPENGWAS_URL = fake$url(), OPENGWAS_DEV_KEY = fake_key, OPENGWAS_DEV_USER = "someone-else"),
    og_api()
  )
  expect_equal(og_user(src)$user, "user:someone-else")
})

test_api("og_status() reports whether the API is ready, and its limits", {
  status <- og_status(src)
  expect_type(status, "list")
  expect_true(status$ready)
  expect_gt(status$execution$limits$result_rows, 0)
})

test_api("og_user() reports the account and allowance", {
  user <- og_user(src)
  expect_type(user, "list")
  expect_match(user$user, "^user:")
  expect_true(all(c("tier", "allowance", "concurrency_budget") %in% names(user)))
  expect_gt(user$allowance, 0)
})

# --- the error rule ---------------------------------------------------------------------------
#
# The error rule (ADR 0002, note of 22 Nov 2026): an API error reaches the user with the API's own
# message (key redacted) and a class from its code, `opengwasr_<code>`, under `opengwasr_refusal` or
# `opengwasr_busy` where they apply and always `opengwasr_error`. The fake answers with the API's
# real error bodies (fixtures/errors.json). Tests assert classes and fields, never wording: where a
# message is compared, it is compared with what the API itself sent (propagation, not wording).
cad <- "OGS-00011:GCST003116"
rel <- "OGS-00011/OGS-00011-completed"
errors <- read_fixture("errors.json")
withr::local_options(opengwasr.poll_cap = 0.05, opengwasr.progress = FALSE)

fake_bodies <- function() raw_api(fake$url(), fake_key, "_fake/bodies")
# What the API says to `body` at `path`, asked directly, outside opengwasr.
api_says <- function(src, path, body) raw_api(src$url, src$credentials()$key, path, body)

refusals <- c("unsupported_threshold", "query_too_broad", "request_too_large", "ld_refusal")

test_that("every code the class table names has a real body to test it with", {
  expect_true(all(names(api_error_parents) %in% names(errors)))
  expect_true(all(c("opengwasr_refusal", "opengwasr_busy") %in% unlist(api_error_parents)))
  expect_setequal(names(api_error_parents)[api_error_parents == "opengwasr_refusal"], refusals)
})

test_that("every API error surfaces with the API's message, a class from its code, and its fields", {
  src <- fake_source(fake)
  # The fake answers a look at Job `refuse-<code>` with that code's real body. Every other member of
  # the body rides on the condition as a field.
  seen <- function(code, e, body = errors[[code]]$body, fields = setdiff(names(body), c("error", "message", "detail"))) {
    list(class = intersect(class(e), c(paste0("opengwasr_", code), "opengwasr_refusal", "opengwasr_busy", "opengwasr_error")),
         message = conditionMessage(e), api_error = e$api_error, api_message = e$api_message, status = e$status,
         fields = e[fields])
  }
  got <- lapply(names(errors), function(code) {
    seen(code, tryCatch(og_job_status(src, paste0("refuse-", code)), error = identity))
  })
  want <- lapply(names(errors), function(code) {
    body <- errors[[code]]$body
    status <- errors[[code]]$status
    list(class = c(paste0("opengwasr_", code), if (code %in% refusals) "opengwasr_refusal",
                   if (status %in% c(429, 503)) "opengwasr_busy", "opengwasr_error"),
         message = body$message, api_error = code, api_message = body$message, status = status,
         fields = body[setdiff(names(body), c("error", "message", "detail"))])
  })
  expect_equal(stats::setNames(got, names(errors)), stats::setNames(want, names(errors)))
})

test_that("a retired Analysis is also one that names no Analysis; an ambiguous one lists its candidates", {
  src <- fake_source(fake)
  e <- expect_error(og_job_status(src, "refuse-analysis_retired"), class = "opengwasr_analysis_not_found")
  expect_equal(e$superseded_by, list("OGS-00014:NEW1"))
  e <- expect_error(og_job_status(src, "refuse-ambiguous_analysis"), class = "opengwasr_ambiguous_analysis")
  expect_equal(e$candidates, c("OGS-00011:GCST90000001", "OGS-00012:GCST90000001"))
})

test_that("a code the table doesn't list still gets its own class; only an answer with no code is opengwasr_api_error", {
  src <- fake_source(fake)
  httr2::local_mocked_responses(list(
    httr2::response_json(400, body = list(error = "brand_new", message = "Something new went wrong", hint = 1)),
    httr2::response_json(429, body = list(error = "allowance_used", message = "Allowance used")),
    httr2::response_json(500, body = list(message = "Internal error")),
    httr2::response(502, headers = list(`Content-Type` = "text/plain"), body = charToRaw("Bad gateway"))
  ))
  e <- expect_error(api_response(src, "api/tophits"), class = "opengwasr_brand_new")
  expect_false(inherits(e, "opengwasr_api_error"))
  expect_equal(c(conditionMessage(e), e$api_error, e$hint), c("Something new went wrong", "brand_new", "1"))
  e <- expect_error(api_response(src, "api/tophits"), class = "opengwasr_allowance_used")
  expect_s3_class(e, "opengwasr_busy")   # an unlisted 429 or 503 is busy
  e <- expect_error(api_response(src, "api/tophits"), class = "opengwasr_api_error")
  expect_equal(c(conditionMessage(e), e$api_error), c("Internal error", NA))
  e <- expect_error(api_response(src, "api/tophits"), class = "opengwasr_api_error")
  expect_equal(conditionMessage(e), "Bad gateway")
})

test_that("a body member named like og_abort()'s own arguments is never passed to it", {
  src <- fake_source(fake)
  httr2::local_mocked_responses(list(httr2::response_json(400, body = list(
    error = "invalid_query", message = "bad", refusal = TRUE, class = "x", call = "y", parent = "z", .subclass = "w"))))
  e <- expect_error(api_response(src, "api/tophits"), class = "opengwasr_invalid_query")
  expect_false(inherits(e, "opengwasr_refusal"))
  expect_false(inherits(e, "x"))
  expect_true(e$api_body$refusal)
})

test_that("a removed duplicate: a looser threshold reaches the API, and the API's refusal is what surfaces", {
  src <- fake_source(fake)
  before <- length(fake_bodies())
  e <- expect_error(og_tophits(src, cad, threshold = 1e-3, clump = FALSE, releases = rel),
                    class = "opengwasr_unsupported_threshold")
  sent <- fake_bodies()
  expect_length(sent, before + 1)
  expect_equal(sent[[before + 1]]$body$threshold, 1e-3)
  expect_equal(conditionMessage(e), errors$unsupported_threshold$body$message)
  expect_equal(unlist(e$indexed_thresholds), c(5e-8, 5e-6, 5e-4))
  # And a p-value threshold outside 0 to 1, which only the API checks now.
  e <- expect_error(og_tophits(src, cad, threshold = 2, clump = FALSE, releases = rel), class = "opengwasr_invalid_request")
  expect_equal(fake_bodies()[[before + 2]]$body$threshold, 2)
  expect_equal(conditionMessage(e), errors$invalid_request$body$message)
})

test_that("lookup-side errors carry the API's code in `api_error`, as API errors do", {
  src <- fake_source(fake)
  codes <- c(
    analysis_not_found = tryCatch(og_analyses(src, "nope"), error = function(e) e$api_error),
    analysis_retired = tryCatch(og_analyses(src, "OGS-00013:OLD1"), error = function(e) e$api_error),
    ambiguous_analysis = tryCatch(og_analyses(src, "GCST90000001"), error = function(e) e$api_error),
    release_not_found = tryCatch(og_analyses(src, cad, releases = "OGS-99999"), error = function(e) e$api_error)
  )
  expect_equal(unname(codes), names(codes))
})

test_that("removed messages: a refused key and a failed Job say what the API said", {
  e <- expect_error(og_user(fake_source(fake, key = "the-wrong-key")), class = "opengwasr_unauthorized")
  expect_equal(conditionMessage(e), errors$unauthorized$body$message)
  expect_equal(c(e$status, e$api_error), c("401", "unauthorized"))

  src <- fake_source(fake)
  fake_set(fake, "job", "failed_request", "normal")
  e <- expect_error(og_tophits(src, cad, clump = FALSE, releases = rel), class = "opengwasr_job_failed")
  expect_equal(conditionMessage(e), e$api_message)
})

test_that("each kept client check raises before any request", {
  src <- fake_source(fake)
  sent <- 0
  httr2::local_mocked_responses(function(req) {
    sent <<- sent + 1
    httr2::response_json(500, body = list(error = "unexpected", message = "no request was expected"))
  })
  kept <- list(
    argument = quote(og_tophits(src, cad, threshold = "5e-8", clump = FALSE)),
    argument = quote(og_tophits(src, cad, limit = 10001, clump = FALSE)),
    argument = quote(og_tophits(src, 1:3, clump = FALSE)),
    argument = quote(og_analyses(src, NA_character_)),
    argument = quote(og_analyses(src, "x", releases = 7)),
    argument = quote(og_associations(src, "16:53767042:T:C", cad)),   # non-canonical ALID (until ogdb-api#183)
    argument = quote(og_tophits(src, cad, r2 = "0.1")),
    auth = quote(withr::with_envvar(c(OPENGWAS_DEV_KEY = NA), og_api(url = "http://x.test")))
  )
  for (i in seq_along(kept)) {
    expect_error(eval(kept[[i]]), class = paste0("opengwasr_", names(kept)[[i]]), info = deparse(kept[[i]]))
  }
  expect_equal(sent, 0)
})

# --- against the dev Data API too ---------------------------------------------------------------

test_api("an unknown Analysis surfaces with the API's own reason, as opengwasr_analysis_not_found", {
  e <- expect_error(og_analyses(src, c(cad, "ieu-a-2")), class = "opengwasr_analysis_not_found")
  said <- api_says(src, "api/gwasinfo", list(id = list("ieu-a-2")))
  expect_equal(e$ids, "ieu-a-2")
  expect_equal(e$api_reasons[["ieu-a-2"]], said$opengwas[[1]]$reason)
  expect_match(conditionMessage(e), said$opengwas[[1]]$reason, fixed = TRUE)
})

test_api("an unsupported threshold surfaces with the API's message, as an opengwasr_unsupported_threshold refusal", {
  e <- expect_error(og_tophits(src, cad, threshold = 1e-3, clump = FALSE, releases = rel),
                    class = "opengwasr_unsupported_threshold")
  expect_s3_class(e, "opengwasr_refusal")
  releases <- list(list(store_id = "OGS-00011", release_id = "OGS-00011-completed"))
  said <- api_says(src, "api/tophits", list(analyses = list(cad), threshold = 1e-3, limit = 1L, releases = releases))
  expect_equal(conditionMessage(e), said$message)
  expect_equal(e$indexed_thresholds, said$indexed_thresholds)
  # A threshold outside 0 to 1 is the API's to refuse too.
  e <- expect_error(og_tophits(src, cad, threshold = 1.5, clump = FALSE, releases = rel), class = "opengwasr_invalid_request")
  said <- api_says(src, "api/tophits", list(analyses = list(cad), threshold = 1.5, limit = 1L, releases = releases))
  expect_equal(c(conditionMessage(e), e$api_error), c(said$message, said$error))
})

test_api("an unknown Store Release surfaces with the API's message, as opengwasr_release_not_found", {
  e <- expect_error(og_analyses(src, cad, releases = "OGS-00011/nonesuch"), class = "opengwasr_release_not_found")
  releases <- list(list(store_id = "OGS-00011", release_id = "nonesuch"))
  said <- api_says(src, "api/gwasinfo", list(id = list(cad), releases = releases))
  expect_equal(c(conditionMessage(e), e$api_error), c(said$message, said$error))
  expect_equal(e$status, 404L)
})

test_that("JSON bodies are the same text whatever the last ulp of a number", {
  src <- fake_source(fake)
  body_text <- function(x) {
    req <- api_request(src, "api/x", body = list(threshold = x))
    # httr2 serialises the stored data with the stored parameters (`digits`, `auto_unbox`, `null`).
    as.character(do.call(jsonlite::toJSON, c(list(req$body$data), req$body$params)))
  }
  low <- 1e-50 * (1 - 2^-52)
  expect_false(identical(low, 1e-50))
  expect_identical(body_text(low), body_text(1e-50))
  expect_match(body_text(1e-50), "1e-50", fixed = TRUE)
  expect_identical(body_text(0.001 * (1 + 2^-52)), body_text(0.001))
})
