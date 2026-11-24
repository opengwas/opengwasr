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
# `fake_world()`. Two modes, one definition of the fake (`fake_api()` in fake-server.R):
#  * In-process (the default): the app is built in this session and requests made by httr2 to the
#    fake's URL are answered by the app's own handlers, with no socket and no second process
#    (`fake_dispatch()` below).
#  * `socket = TRUE`: the same app served by webfakes in a second R process, for the tests that are
#    about the transport (timeouts, connection failures, interrupts). See CONTRIBUTING.md.
# Either way the result has `$url()`, the base URL a Source or `raw_api()` talks to.
local_fake_api <- function(..., socket = FALSE, env = testthat::teardown_env()) {
  app <- server_env$fake_api(server_env$fake_world(...))
  if (isTRUE(socket)) return(webfakes::local_app_process(app, .local_envir = env))
  fake_in_process(app, env)
}

fake_hosts <- new.env()

# Answer httr2 requests to a fresh host name from `app`'s handlers, until `env` finishes. Requests to
# any other host go to whatever mock (or the network) was in place before.
fake_in_process <- function(app, env) {
  host <- paste0("fake-api-", length(ls(fake_hosts)) + 1, ".test")
  assign(host, TRUE, envir = fake_hosts)
  before <- getOption("httr2_mock")
  httr2::local_mocked_responses(function(req) {
    if (!identical(httr2::url_parse(httr2::req_get_url(req))$hostname, host)) {
      return(if (is.function(before)) before(req))
    }
    fake_dispatch(app, req)
  }, env = env)
  list(url = function() paste0("http://", host, "/"), app = app)
}

# Run one httr2 request through `app`'s routes the way webfakes' server does (the loop of its
# `app$.process_request()`, which can't be called without a socket), and return an httr2 response.
# `res` implements the response methods the fake's handlers use; a handler that needs another one
# fails here, loudly, and the method is added below.
fake_dispatch <- function(app, request) {
  url <- httr2::url_parse(httr2::req_get_url(request))
  body <- if (httr2::req_get_body_type(request) == "json") {
    # Serialised as httr2 does it: the body data with the arguments given to `req_body_json()`.
    charToRaw(unclass(rlang::exec(jsonlite::toJSON, request$body$data, !!!request$body$params)))
  }
  headers <- httr2::req_get_headers(request, "reveal")
  if (!is.null(body) && is.null(headers[["Content-Type"]])) headers[["Content-Type"]] <- "application/json"
  req <- new.env()
  res <- new.env()
  req$method <- tolower(httr2::req_get_method(request))
  req$path <- sub("//+", "/", paste0("/", url$path))
  req$query <- url$query %||% list()
  req$headers <- headers
  req$.body <- body
  req$app <- app
  req$res <- res
  req$get_header <- function(field) {
    h <- headers
    names(h) <- tolower(names(h))
    h[[tolower(field)]]
  }
  res$status <- 200L
  res$headers <- list()
  res$body <- ""
  res$sent <- FALSE
  res$set_status <- function(status) { res$status <- as.integer(status); res }
  res$set_header <- function(field, value) { res$headers[[field]] <- as.character(value); res }
  res$set_type <- function(type) res$set_header("Content-Type", type)
  res$send <- function(body) { res$body <- body; res$sent <- TRUE; res }
  res$send_json <- function(object = NULL, text = NULL, ...) {
    res$set_header("Content-Type", "application/json")$send(text %||% jsonlite::toJSON(object, ...))
  }
  res$send_status <- function(status) res$set_status(status)$send("")
  # Which handlers answer this method and path (with their parameters) is fixed by the app, and a
  # test asks about the same paths again and again, so it is looked up once.
  key <- paste(req$method, req$path)
  matches <- app$locals$.routes[[key]]
  if (is.null(matches)) {
    matches <- Filter(Negate(is.null), lapply(app$.stack, function(handler) {
      m <- webfakes:::path_match(req$method, req$path, handler)
      if (!isFALSE(m)) list(handler = handler$handler, params = if (is.list(m)) m$params)
    }))
    if (is.null(app$locals$.routes)) app$locals$.routes <- new.env()
    assign(key, matches, envir = app$locals$.routes)
  }
  for (m in matches) {
    if (!is.null(m$params)) req$params <- m$params
    if (!identical(m$handler(req, res), "next")) break
  }
  if (!res$sent) res$send_status(404)
  httr2::response(res$status, url = httr2::req_get_url(request), method = toupper(req$method),
                  headers = res$headers, body = charToRaw(paste(res$body, collapse = "\n")))
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
