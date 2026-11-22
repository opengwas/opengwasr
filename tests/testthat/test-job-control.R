fake <- local_fake_api(n_synthetic = 1)
withr::local_options(opengwasr.poll_cap = 0.05, opengwasr.progress = FALSE)

cad <- "OGS-00011:GCST003116"
bmi <- "OGS-00011:GCST90029007"
rel <- "OGS-00011/OGS-00011-completed"
snv <- "16:53767042:C:T"
quietly <- function(x) suppressWarnings(x, classes = "opengwasr_truncated")
without_time <- function(x, existing = NULL) {   # provenance minus the moment it was read
  prov <- attr(x, "og_provenance")
  prov$retrieved <- NULL
  if (!is.null(existing)) prov$existing <- existing
  attr(x, "og_provenance") <- prov
  x
}

# A query function written the way the plan contract says (R/query-plan.R), as #7, #9 and later
# tickets will write theirs: prepare (optionally through another query), build a plan of several
# Jobs, finish from what was pinned at plan time.
fake_query <- function(src, ids, nested = FALSE) {
  deadline <- local_query(src)
  if (nested) quietly(og_tophits(src, cad, clump = FALSE, limit = 1, releases = rel))
  release <- list(list(store_id = "OGS-00011", release_id = "OGS-00011-completed"))
  requests <- lapply(ids, function(id) {
    list(path = "api/tophits", body = list(analyses = list(id), threshold = 5e-8, limit = 2L,
                                           observed_only = FALSE, releases = release))
  })
  pinned <- ids
  plan <- query_plan(requests, finish = function(results) {
    list(ids = pinned, rows = vapply(results, function(r) nrow(r$rows), 1L),
         jobs = vapply(results, function(r) r$job$job_id, ""))
  })
  run_query(src, plan, deadline)
}

test_api("og_submit() returns without waiting; status, result and cancel match the synchronous call", {
  clock <- local_virtual_clock()
  jobs <- og_submit(src, og_tophits, cad, clump = FALSE, limit = 3, releases = rel)
  expect_s3_class(jobs, "og_jobs")
  expect_length(clock$waits, 0)
  expect_length(job_requests(clock), 0)
  expect_match(jobs$job_ids, "^[0-9a-f-]{36}$")

  status <- og_job_status(src, jobs)
  expect_equal(status$job_id, jobs$job_ids)
  expect_true(status$status %in% c("queued", "running", "succeeded"))
  expect_equal(og_job_status(src, jobs$job_ids)$job_id, jobs$job_ids)   # a bare id is fine here

  top <- quietly(og_job_result(src, jobs))
  direct <- quietly(og_tophits(src, cad, clump = FALSE, limit = 3, releases = rel))
  # The same rows and provenance, cost_estimate included. The synchronous call is the second
  # submission of this query, so it reattaches (`existing`); the handle keeps what its own submission
  # saw (a live API may already hold the query from an earlier run).
  expect_equal(og_provenance(top)$existing, isTRUE(jobs$jobs[[1]]$existing))
  expect_true(og_provenance(direct)$existing)
  expect_equal(without_time(top, existing = TRUE), without_time(direct, existing = TRUE))
  expect_equal(og_provenance(top)$job_ids, jobs$job_ids)
  expect_false(is.na(og_provenance(top)$cost_estimate))

  other <- og_submit(src, og_tophits, cad, clump = FALSE, limit = 2, releases = rel)
  cancelled <- og_job_cancel(src, other)
  expect_true(cancelled$status %in% c("cancelled", "succeeded"))
  expect_equal(cancelled$job_id, other$job_ids)
})

test_that("a Job still running times out naming it, cancels, then names the cancellation; bare ids refuse unless raw", {
  fake_set(fake, "job", "never", "normal")
  src <- fake_source(fake)
  jobs <- og_submit(src, og_tophits, cad, clump = FALSE, limit = 4, releases = rel)
  expect_equal(og_job_status(src, jobs)$status, "running")

  # The same call, with a short budget, reattaches and times out: the error names the Job.
  clock <- local_virtual_clock()
  withr::local_options(opengwasr.poll_cap = 1000)   # a few long polls, not a hundred short ones
  e <- expect_error(og_tophits(fake_source(fake, timeout = 5), cad, clump = FALSE, limit = 4, releases = rel),
                    class = "opengwasr_timeout")
  expect_equal(e$job_ids, jobs$job_ids)
  expect_match(conditionMessage(e), "og_job_cancel", fixed = TRUE)
  expect_match(conditionMessage(e), "e$jobs", fixed = TRUE)   # it has a handle to collect with
  expect_s3_class(e$jobs, "og_jobs")

  expect_equal(og_job_cancel(src, jobs$job_ids)$status, "cancelled")
  expect_error(og_job_result(src, jobs), class = "opengwasr_job_cancelled")

  e <- expect_error(og_job_result(src, jobs$job_ids), class = "opengwasr_argument")
  expect_match(conditionMessage(e), "og_submit", fixed = TRUE)
  fake_set(fake, "job", "normal", "normal")
  ok <- og_submit(src, og_tophits, cad, clump = FALSE, limit = 5, releases = rel)
  raw <- og_job_result(src, ok$job_ids, raw = TRUE)
  expect_s3_class(raw, "og_job_raw")
  expect_true(nrow(raw) > 0)
  expect_false("beta" %in% names(raw))   # unshaped, and not pretending otherwise
})

test_that("every Job function checks its Source and its job argument", {
  for (f in list(og_job_status, og_job_result, og_job_cancel)) {
    expect_error(f(list(url = "x", timeout = 5), "abc"), class = "opengwasr_argument")
  }
  src <- fake_source(fake)
  expect_error(og_job_status(src, 1), class = "opengwasr_argument")
  expect_error(og_submit(src, function(src) 1), class = "opengwasr_argument")
  expect_error(og_submit(src, "og_tophits"), class = "opengwasr_argument")
})

test_that("a plan of several Jobs is submitted together; a query inside its preparation stays synchronous", {
  fake_set(fake, "job", "queued_running", "normal")
  src <- fake_source(fake)
  clock <- local_virtual_clock()
  # `nested = TRUE`: the preparation runs og_tophits() (to completion, waiting) before the plan is
  # submitted, so submit mode is for the outermost query only.
  jobs <- og_submit(src, fake_query, c(cad, bmi), nested = TRUE)
  expect_length(jobs$job_ids, 2)
  expect_equal(og_job_status(src, jobs)$status, c("queued", "queued"))
  out <- og_job_result(src, jobs)
  expect_equal(out$ids, c(cad, bmi))
  expect_equal(out$jobs, jobs$job_ids)
  expect_equal(og_job_cancel(src, jobs)$job_id, jobs$job_ids)

  # An interrupt at the second submission keeps the first Job: the error carries it, as a partial
  # handle that can be cancelled but not collected as if it were the whole answer.
  real_submit <- getFromNamespace("job_submit", "opengwasr")
  calls <- 0
  local_mocked_bindings(
    job_submit = function(...) {
      calls <<- calls + 1
      if (calls == 2) rlang::interrupt() else real_submit(...)
    },
    .package = "opengwasr"
  )
  e <- expect_error(fake_query(src, c(cad, bmi, cad)), class = "opengwasr_interrupted")
  expect_length(e$job_ids, 1)
  expect_true(e$jobs$partial)
  expect_error(og_job_result(src, e$jobs), class = "opengwasr_argument")
  expect_equal(og_job_cancel(src, e$jobs)$job_id, e$job_ids)
})

test_that("og_submit() needs no marked function: lambdas and partial application submit; two queries refuse", {
  src <- fake_source(fake)
  bodies <- function() length(raw_api(fake$url(), fake_key, "_fake/jobs")$times)
  one <- function(src, id) og_tophits(src, id, clump = FALSE, limit = 17, releases = rel)
  wrapped <- og_submit(src, one, cad)
  expect_s3_class(wrapped, "og_jobs")
  expect_length(wrapped$job_ids, 1)
  # Partial application, the way purrr::partial() builds it (purrr isn't a dependency here).
  make_partial <- function(...) {
    args <- list(...)
    function(src, ...) do.call(og_tophits, c(list(src, ...), args))
  }
  partial <- make_partial(clump = FALSE, limit = 18, releases = rel)
  expect_length(og_submit(src, partial, cad)$job_ids, 1)
  # A lambda that runs two queries can't be submitted as one: say so, don't drop the second.
  two <- function(src) {
    list(og_tophits(src, cad, clump = FALSE, limit = 19, releases = rel),
         og_tophits(src, bmi, clump = FALSE, limit = 19, releases = rel))
  }
  expect_error(og_submit(src, two), class = "opengwasr_argument")
  # And one that does something with the answer, rather than returning it.
  post <- function(src) nrow(og_tophits(src, cad, clump = FALSE, limit = 20, releases = rel))
  expect_error(og_submit(src, post), class = "opengwasr_argument")
})

test_that("og_associations() submits and collects like the synchronous call, rsID resolution included", {
  src <- fake_source(fake)
  clock <- local_virtual_clock()
  # An rsID (resolved by a Job in preparation) and an ALID: the Associations Job is the one submitted.
  jobs <- og_submit(src, og_associations, c("rs1421085", snv), cad, releases = rel)
  expect_length(jobs$job_ids, 1)
  expect_false(is.null(jobs$jobs[[1]]$cost_estimate))
  got <- og_job_result(src, jobs)
  direct <- og_associations(src, c("rs1421085", snv), cad, releases = rel)
  expect_equal(without_time(got, existing = TRUE), without_time(direct, existing = TRUE))
  expect_equal(og_provenance(got)$job_ids, og_provenance(direct)$job_ids)
  expect_equal(og_provenance(got)$cost_estimate, og_provenance(direct)$cost_estimate)
})

test_that("an interrupt while waiting leaves the Job running; the error carries the handle", {
  fake_set(fake, "job", "queued_running", "normal")
  src <- fake_source(fake)
  clock <- local_virtual_clock()
  local_mocked_bindings(job_sleep = function(seconds) rlang::interrupt(), .package = "opengwasr")
  e <- expect_error(og_tophits(src, cad, clump = FALSE, limit = 5, releases = rel),
                    class = "opengwasr_interrupted")
  expect_s3_class(e$jobs, "og_jobs")
  expect_equal(e$job_ids, e$jobs$job_ids)
  expect_match(conditionMessage(e), e$job_ids, fixed = TRUE)
  expect_match(conditionMessage(e), "og_job_result(src, e$jobs)", fixed = TRUE)
  expect_true(og_job_status(src, e$job_ids)$status %in% c("queued", "running"))


  # Collect it from the handle: the Job's own provenance, not a new submission.
  local_mocked_bindings(job_sleep = function(seconds) clock$t <- clock$t + seconds, .package = "opengwasr")
  top <- quietly(og_job_result(src, e$jobs))
  expect_equal(og_provenance(top)$job_ids, e$job_ids)
  # Or run the same call again: reattached.
  again <- quietly(og_tophits(src, cad, clump = FALSE, limit = 5, releases = rel))
  expect_true(og_provenance(again)$existing)
  expect_equal(og_provenance(again)$job_ids, e$job_ids)

  # An interrupt in collect's first status read, or on the bare-id path, names the Job as well.
  local_mocked_bindings(job_status = function(...) rlang::interrupt(), .package = "opengwasr")
  again <- expect_error(og_job_result(src, e$jobs), class = "opengwasr_interrupted")
  expect_equal(again$job_ids, e$job_ids)
  expect_s3_class(again$jobs, "og_jobs")
  again <- expect_error(og_job_result(src, e$job_ids, raw = TRUE), class = "opengwasr_interrupted")
  expect_equal(again$job_ids, e$job_ids)
})

set_busy <- function(code, times, after, route = "submit", env = parent.frame()) {
  fake_set(fake, "busy_route", route, "submit", env)
  fake_set(fake, "busy", code, "none", env)
  fake_set(fake, "busy_times", as.character(times), "0", env)
  fake_set(fake, "busy_after", as.character(after), "3", env)
}

test_that("end to end: a refused submission is retried after the stated wait", {
  src <- fake_source(fake, timeout = 100)
  set_busy("user_queue_full", 2, 7)
  clock <- local_virtual_clock()
  expect_s3_class(quietly(og_tophits(src, cad, clump = FALSE, limit = 8, releases = rel)), "data.frame")
  expect_equal(clock$waits[1:2], c(7, 7))
  expect_length(Filter(function(r) r$path == "api/tophits", clock$requests), 1)   # retries live below
})

# The retry policy, on api_response() alone: scripted answers, a hand-run clock, no server.
mock_clock <- function(env = parent.frame()) {
  clock <- new.env()
  clock$t <- 0
  clock$waits <- numeric()
  local_mocked_bindings(
    job_now = function() clock$t,
    job_sleep = function(seconds) {
      clock$waits <- c(clock$waits, seconds)
      clock$t <- clock$t + seconds
    },
    .package = "opengwasr", .env = env
  )
  clock
}
# The API's real busy answer for `code` (fixtures/errors.json); `body` replaces it for a careless server.
busy <- function(code, after = NULL, body = read_fixture("errors.json")[[code]]$body, status = 429) {
  status <- read_fixture("errors.json")[[code]]$status %||% status
  httr2::response_json(status, body = body,
                       headers = if (!is.null(after)) list("Retry-After" = as.character(after)) else list())
}
ok <- function() httr2::response_json(200, body = list(ok = TRUE))

test_that("429 and 503 are retried after Retry-After on any route: submit, status, result page", {
  src <- fake_source(fake, timeout = 100)
  for (path in c("api/tophits", "api/jobs/abc", "api/jobs/abc/result")) {
    clock <- mock_clock()
    httr2::local_mocked_responses(list(busy("user_queue_full", 7), busy("intake_paused", 5), ok()))
    expect_true(api_response(src, path) |> httr2::resp_body_json() |> _$ok)
    expect_equal(clock$waits, c(7, 5), info = path)
  }
})

test_that("a Retry-After below a second waits a second; the code is redacted from progress output", {
  withr::local_options(opengwasr.progress = TRUE)
  src <- fake_source(fake, timeout = 100)
  clock <- mock_clock()
  httr2::local_mocked_responses(list(busy(fake_key, 0, body = list(error = fake_key, message = "busy")), ok()))   # the API echoes the key as its code
  msgs <- testthat::capture_messages(api_response(src, "api/jobs/abc"))
  expect_equal(clock$waits, 1)
  expect_true(any(grepl("busy", msgs)))
  expect_false(any(grepl(fake_key, msgs, fixed = TRUE)))
})

test_that("a wait that would pass the deadline, or no Retry-After, is opengwasr_busy at once", {
  src <- fake_source(fake, timeout = 40)
  clock <- mock_clock()
  httr2::local_mocked_responses(list(busy("server_busy", 30), busy("server_busy", 30), ok()))
  e <- expect_error(api_response(src, "api/jobs/abc"), class = "opengwasr_busy")
  expect_equal(c(e$api_error, e$retry_after), c("server_busy", "30"))
  expect_equal(clock$waits, 30)   # one wait fits in 40 s; a second would not

  clock <- mock_clock()
  httr2::local_mocked_responses(list(busy("job_store_unavailable"), ok()))
  e <- expect_error(api_response(src, "api/jobs/abc"), class = "opengwasr_busy")
  expect_length(clock$waits, 0)
  httr2::local_mocked_responses(list(busy(fake_key, 500, body = list(error = fake_key, message = "busy"))))
  e <- expect_error(api_response(src, "api/jobs/abc"), class = "opengwasr_busy")
  expect_false(grepl(fake_key, paste(conditionMessage(e), e$api_error), fixed = TRUE))
})

test_that("one deadline covers every request: lookups and transport time are capped by what is left", {
  # The Store Release and Analysis lookups of a call share its budget (virtual clock: 3 s a lookup).
  src <- fake_source(fake, timeout = 2)
  local_virtual_clock(latency = list("api/gwasinfo" = 3))
  expect_error(og_tophits(src, cad, clump = FALSE, limit = 14, releases = rel), class = "opengwasr_timeout")
})

test_that("each request's transport timeout is what is left of the call, with or without an explicit one", {
  src <- fake_source(fake, timeout = 2)
  clock <- mock_clock()
  clock$t <- 1.5
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- c(seen, req$options$timeout_ms)
    ok()
  })
  api_response(src, "api/status", deadline = 2)
  api_response(src, "api/status", deadline = 2, timeout = 60)
  expect_true(all(seen <= 500 + 1))
  # Nothing is sent once the budget is spent.
  clock$t <- 3
  seen <- NULL
  expect_error(api_response(src, "api/status", deadline = 2), class = "opengwasr_timeout")
  expect_null(seen)
})

test_that("410 result_expired names its Job; analysis_retired stays distinct", {
  fake_set(fake, "job", "result_gone", "normal")
  src <- fake_source(fake)
  jobs <- og_submit(src, og_tophits, cad, clump = FALSE, limit = 16, releases = rel)
  e <- expect_error(og_job_result(src, jobs), class = "opengwasr_result_expired")
  expect_equal(e$job_id, jobs$job_ids)
  expect_equal(conditionMessage(e), read_fixture("errors.json")$result_expired$body$message)
  expect_false(inherits(e, "opengwasr_analysis_retired"))
  expect_error(og_analyses(fake_source(fake), "OGS-00013:GONE"), class = "opengwasr_analysis_retired")
})
