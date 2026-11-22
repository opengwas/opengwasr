# A fake OpenGWAS Data API (webfakes), for unit tests. Later tickets extend it: add routes to
# `fake_api()` and rows or scenarios to `fake_world()` in fake-server.R. Routes read their data from
# the world, so an extension is additive.
#
# Start it once per test file (`fake <- local_fake_api()` at the top), not once per test.
# Requests are checked against the OpenAPI snapshot (see helper-contract.R) and answered the way
# the real API answers, including its error bodies.

fixture_dir <- testthat::test_path("fixtures")
fixture_file <- function(...) file.path(fixture_dir, ...)
server_file <- testthat::test_path("fake-server.R")

# fake-server.R is sourced twice: into this session (for `fake_world()`, `schema_problems()`, ...)
# and into `server_env`, whose parent is the global environment. webfakes serialises the app's
# handlers into a subprocess, and an environment chain through the package namespace can't be
# loaded there; a plain environment can.
source(server_file, local = TRUE)
server_env <- new.env(parent = globalenv())
server_env$fixture_dir <- normalizePath(fixture_dir)
sys.source(normalizePath(server_file), server_env)

# Start the fake once and stop it when `env` finishes (the test file, by default). `...` go to
# `fake_world()`.
local_fake_api <- function(..., env = testthat::teardown_env()) {
  app <- server_env$fake_api(server_env$fake_world(...))
  webfakes::local_app_process(app, .local_envir = env)
}

# A Source for the fake, signed in with its key.
fake_source <- function(fake, key = fake_key, ...) {
  withr::with_envvar(
    c(OPENGWAS_URL = fake$url(), OPENGWAS_DEV_KEY = key, OPENGWAS_DEV_USER = NA),
    og_api(...)
  )
}

# Switch a fake scenario on for the rest of the calling test: `paging` ("cursor", "offset" or
# "truncating") or `malformed` ("true"). Undone when the test ends.
fake_set <- function(fake, setting, value, default, env = parent.frame()) {
  post <- function(v) raw_api_post(fake$url(), paste0("_fake/", setting, "/", v))
  post(value)
  withr::defer(post(default), envir = env)
}
raw_api_post <- function(url, path) {
  httr2::req_perform(httr2::req_method(httr2::request(paste0(url, path)), "POST"))
}
