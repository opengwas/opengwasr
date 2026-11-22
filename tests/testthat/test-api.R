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

test_that("a 401 from the API raises opengwasr_auth", {
  src <- fake_source(fake, key = "the-wrong-key")
  err <- expect_error(og_user(src), class = "opengwasr_auth")
  expect_s3_class(err, "opengwasr_error")
  expect_equal(err$api_error, "unauthorized")
  expect_equal(err$status, 401L)
  expect_equal(err$setting, "OPENGWAS_DEV_KEY")
})

# Serialise a condition every way a user or a log might, and look for `key` in it.
condition_text <- function(cnd) {
  paste(c(capture.output(str(cnd)), capture.output(print(cnd)), conditionMessage(cnd),
          rawToChar(serialize(cnd, NULL, ascii = TRUE))), collapse = "\n")
}

test_that("a 401 whose body echoes the key leaves the key nowhere in the condition", {
  key <- paste0("k-", "123456789012", "-secret-echoed")  # built, so the test source doesn't hold it
  src <- fake_source(fake, key = key)
  err <- expect_error(og_user(src), class = "opengwasr_auth")
  expect_false(grepl(key, condition_text(err), fixed = TRUE))
  expect_match(err$api_message, "<redacted>", fixed = TRUE)
})

test_that("a server that echoes the key as a JSON member name or in a reason leaves it nowhere", {
  fake_set(fake, "leak", "true", "false")
  key <- paste0("k-", "123456789012", "-secret-member")
  err <- expect_error(og_user(fake_source(fake, key = key)), class = "opengwasr_auth")
  expect_false(grepl(key, condition_text(err), fixed = TRUE))
  expect_false(any(grepl(key, names(err$api_body), fixed = TRUE)))
  # A key the fake accepts, echoed in a successful lookup's reason (a plain 200).
  err <- expect_error(og_analyses(fake_source(fake), "ieu-a-2"), class = "opengwasr_unknown_analysis")
  expect_false(grepl(fake_key, condition_text(err), fixed = TRUE))
  expect_match(err$api_reasons[["ieu-a-2"]], "<redacted>", fixed = TRUE)
})

test_that("a 401 is an opengwasr_auth whatever its body, and the setting comes from credentials", {
  fake_set(fake, "plain401", "true", "false")
  err <- expect_error(og_user(fake_source(fake, key = "wrong")), class = "opengwasr_auth")
  expect_equal(err$status, 401L)
  expect_equal(err$setting, "OPENGWAS_DEV_KEY")
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
