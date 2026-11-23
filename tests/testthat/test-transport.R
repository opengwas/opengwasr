# The transport itself: what curl does over a real socket, which an in-process fake can't show.
# Everything else is tested against the in-process fake (see CONTRIBUTING.md); these use
# `local_fake_api(socket = TRUE)`, the same fake served by webfakes in a second R process, or no fake
# at all where the point is that nothing answers.
#
# Also socket tests, kept where their subject lives: "a request that runs out of time is the call's
# timeout, not an unreachable API" (test-tophits.R).

test_that("an API nothing is listening to is unreachable, and the message names it but not the key", {
  src <- withr::with_envvar(
    c(OPENGWAS_URL = "http://127.0.0.1:1", OPENGWAS_DEV_KEY = fake_key, OPENGWAS_DEV_USER = NA),
    og_api(timeout = 5)
  )
  e <- expect_error(api_response(src, "api/status"), class = "opengwasr_unreachable")
  expect_match(conditionMessage(e), "http://127.0.0.1:1", fixed = TRUE)
  expect_false(grepl(fake_key, paste(conditionMessage(e), collapse = " "), fixed = TRUE))
})

test_that("a request over its own timeout is the call's timeout when the call is over, and unreachable when it isn't", {
  fake <- local_fake_api(n_synthetic = 1, socket = TRUE, env = environment())
  fake_set(fake, "delay_ms", "400", "0")
  src <- fake_source(fake, timeout = 60)
  # The call's whole budget is the request's: running out of it is the call's timeout.
  expect_error(api_response(src, "api/status", timeout = 0.1, deadline = job_now() + 0.1),
               class = "opengwasr_timeout")
  # The call has time left, but the request's own timeout passed: the API didn't answer.
  expect_error(api_response(src, "api/status", timeout = 0.1), class = "opengwasr_unreachable")
  # Within its timeout the same request is answered.
  fake_set(fake, "delay_ms", "0", "0")
  expect_equal(httr2::resp_status(api_response(src, "api/status", timeout = 5)), 200L)
})
