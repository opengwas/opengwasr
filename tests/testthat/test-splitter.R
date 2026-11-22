# The splitter's arithmetic and the executor's failure handling, with no fake server: pure functions
# and httr2's mocked responses.

test_that("the splitter cuts each dimension to its limit, in order", {
  one <- split_pieces(list(a = split_dim(5, 2)))
  expect_equal(lapply(one, `[[`, "a"), list(1:2, 3:4, 5L))
  # The first dimension varies slowest; every combination appears once.
  two <- split_pieces(list(a = split_dim(3, 2), b = split_dim(5, 3)))
  expect_equal(lapply(two, function(p) c(p$a[[1]], p$b[[1]])), list(c(1, 1), c(1, 4), c(3, 1), c(3, 4)))
  expect_length(split_pieces(list(a = split_dim(3, 100), b = split_dim(3, 1000))), 1)
  expect_equal(split_pieces(list(a = split_dim(0, 5)))[[1]]$a, integer())
  expect_error(split_dim(3, 0), class = "opengwasr_argument")
  expect_error(split_dim(c(1, 2), 5), class = "opengwasr_argument")
})

test_that("exact multiples of a limit and one over", {
  chunks <- function(n, limit) lengths(lapply(split_pieces(list(a = split_dim(n, limit))), `[[`, "a"))
  expect_equal(chunks(1000, 1000), 1000)
  expect_equal(chunks(1001, 1000), c(1000, 1))
  expect_equal(chunks(2000, 1000), c(1000, 1000))
  expect_equal(chunks(100, 100), 100)
  expect_equal(chunks(101, 100), c(100, 1))
})

test_that("the splitter packs Analyses by GWAS Store: at most `group_limit` Stores and `limit` Analyses a piece", {
  store <- c("A", "B", "A", "C", "D", "D")
  pieces <- split_pieces(list(x = split_dim(6, 10, group = store, group_limit = 2)))
  expect_equal(lapply(pieces, `[[`, "x"), list(c(1L, 3L, 2L), c(4L, 5L, 6L)))
  big <- split_pieces(list(x = split_dim(7, 3, group = c(rep("A", 5), "B", "B"), group_limit = 10)))
  expect_equal(lapply(big, `[[`, "x"), list(1:3, 4:5, 6:7))
  set.seed(1)
  store <- sample(letters[1:8], 60, TRUE)
  for (p in split_pieces(list(x = split_dim(60, 9, group = store, group_limit = 3)))) {
    expect_lte(length(p$x), 9)
    expect_lte(length(unique(store[p$x])), 3)
  }
  # A piece reads only the Store Releases of its own Analyses.
  releases <- lapply(c("A", "B", "C", "D"), function(s) list(store_id = s, release_id = paste0(s, "-r")))
  got <- piece_releases(releases, c("A", "B", "A", "C"), c(3, 4))
  expect_equal(vapply(got, `[[`, "", "store_id"), c("A", "C"))
})

test_that("pairs: every pair is covered, one chunk is one piece, and a limit under 2 is refused", {
  key <- function(i, j) paste(pmin(i, j), pmax(i, j))
  covered <- function(pieces) unique(unlist(lapply(pieces, function(p) {
    u <- union(p$a, p$b)
    outer(u, u, key)
  })))
  pairs <- split_pairs(10, 4)
  expect_true(all(outer(1:10, 1:10, key) %in% covered(pairs)))
  expect_true(all(vapply(pairs, function(p) length(union(p$a, p$b)) <= 4, NA)))
  # Five chunks of two: the ten chunk pairs i < j, and no diagonal pieces.
  expect_length(pairs, 10)
  expect_false(any(vapply(pairs, function(p) identical(p$a, p$b), NA)))
  expect_length(split_pairs(4, 4), 1)
  expect_equal(split_pairs(4, 4)[[1]]$a, 1:4)
  expect_length(split_pairs(8, 4), 6)   # exact multiple: 4 chunks of 2
  expect_error(split_pairs(10, 1), class = "opengwasr_argument")
  expect_error(split_pairs(10, 0), class = "opengwasr_argument")
})

test_that("an interval is a dimension: it crosses other dimensions, and is checked", {
  cut <- split_pieces(list(range = split_interval(1, 25e6, 10e6)))
  expect_equal(lapply(cut, `[[`, "range"), list(c(1, 10e6), c(10e6 + 1, 20e6), c(20e6 + 1, 25e6)))
  expect_length(split_pieces(list(range = split_interval(1, 20e6, 10e6))), 2)   # exact multiple
  expect_equal(split_pieces(list(range = split_interval(5, 5, 10e6)))[[1]]$range, c(5, 5))
  crossed <- split_pieces(list(analyses = split_dim(3, 2), range = split_interval(1, 25e6, 10e6)))
  expect_length(crossed, 6)
  expect_equal(crossed[[2]]$range, c(10e6 + 1, 20e6))
  expect_equal(crossed[[2]]$analyses, 1:2)
  expect_error(split_interval(5, 1, 10), class = "opengwasr_argument")
})

test_that("a top-hits Job takes `limit` x Analyses within the 100,000-row cap", {
  per_job <- function(limit) tophits_analyses_per_job(limit, job_limits())
  expect_equal(per_job(1), 100)
  expect_equal(per_job(1000), 100)
  expect_equal(per_job(1001), 99)
  expect_equal(per_job(10000), 10)
  expect_equal(per_job(9090), 11)
})

test_that("max_in_flight is at most the 100 Jobs a user may queue", {
  withr::local_envvar(OPENGWAS_URL = "http://x.test", OPENGWAS_DEV_KEY = "k", OPENGWAS_DEV_USER = NA)
  expect_error(og_api(max_in_flight = 101), class = "opengwasr_argument")
  expect_equal(og_api(max_in_flight = 100)$max_in_flight, 100L)
})

# A Data API that answers the Job protocol from httr2's mocked responses. `on_submit(n)` may return a
# response to give instead of accepting the n-th submission; `on_status(id)` runs before a status
# answer (it may interrupt); `running` keeps every Job running. `state` records submits and DELETEs.
mock_jobs <- function(on_submit = function(n) NULL, on_status = function(id) NULL, running = FALSE,
                      env = parent.frame()) {
  state <- new.env()
  state$submits <- 0L
  state$deleted <- character()
  httr2::local_mocked_responses(function(req) {
    path <- httr2::url_parse(req$url)$path
    method <- req$method %||% if (is.null(req$body)) "GET" else "POST"
    id <- sub("^.*/api/jobs/([^/]+).*$", "\\1", path)
    if (method == "POST") {
      state$submits <- state$submits + 1L
      refusal <- on_submit(state$submits)
      if (!is.null(refusal)) return(refusal)
      return(httr2::response_json(202, headers = list(`X-OGDB-Cost-Estimate` = "1"), body = list(
        job_id = paste0("job-", state$submits), status = "queued", poll_after_seconds = 0,
        existing = FALSE, ignored_fields = list())))
    }
    if (method == "DELETE") {
      state$deleted <- c(state$deleted, id)
      return(httr2::response_json(body = list(job_id = id, status = "cancelled")))
    }
    if (grepl("/result$", path)) return(httr2::response_json(body = list(ndjson = "", final = TRUE)))
    on_status(id)
    if (running) return(httr2::response_json(body = list(job_id = id, status = "running", poll_after_seconds = 0)))
    httr2::response_json(body = list(
      job_id = id, status = "succeeded", poll_after_seconds = 0,
      result = list(manifest = list(row_count = 0L, releases = list()),
                    metadata = list(total = 0L, truncated = FALSE))))
  }, env = env)
  state
}
mock_source <- function(...) {
  withr::with_envvar(c(OPENGWAS_URL = "http://mock.test", OPENGWAS_DEV_KEY = "mock-key", OPENGWAS_DEV_USER = NA),
                     og_api(...))
}
refusal_413 <- function(...) {
  httr2::response_json(413, body = list(error = "request_too_large", message = "Too costly: download the Store",
                                        cost_estimate = 3479, max_cost = 300))
}
four <- lapply(1:4, function(i) list(path = "api/associations", body = list(i = i)))
window <- function(src, deadline = job_now() + 60) run_window(src, four, deadline, "rows")
withr::local_options(opengwasr.poll_cap = 0.02, opengwasr.progress = FALSE)

test_that("a 413 is classed from the API's error code, with the API's message and its fields", {
  mock_jobs(on_submit = refusal_413)
  e <- expect_error(window(mock_source()), class = "opengwasr_request_too_large")
  expect_s3_class(e, "opengwasr_refusal")
  expect_equal(e$status, 413L)
  expect_equal(e$api_message, "Too costly: download the Store")
  expect_equal(conditionMessage(e)[[1]], e$api_message)   # the API's words, not ours
  expect_equal(c(e$cost_estimate, e$max_cost), c(3479, 300))
  expect_length(e$cancelled, 0)
  # A 413 that isn't the API's (a proxy's body limit) is a plain API error.
  mock_jobs(on_submit = function(n) httr2::response(413, headers = list(`Content-Type` = "text/plain"),
                                                    body = charToRaw("Payload too large")))
  e <- expect_error(window(mock_source()), class = "opengwasr_api_error")
  expect_false(inherits(e, "opengwasr_request_too_large"))
})

test_that("a 413 part-way through a window is not retried or split, and cancels the Jobs not yet collected", {
  state <- mock_jobs(on_submit = function(n) if (n == 3) refusal_413())
  e <- expect_error(window(mock_source(max_in_flight = 2)), class = "opengwasr_request_too_large")
  # Jobs 1 and 2 were in flight; 1 was collected before 3 was tried.
  expect_equal(e$cancelled, "job-2")
  expect_equal(state$deleted, "job-2")
  expect_equal(state$submits, 3L)
})

test_that("any other error part-way through a window carries every Job submitted", {
  mock_jobs(on_submit = function(n) if (n == 3) {
    httr2::response_json(400, body = list(error = "invalid_request", message = "bad"))
  })
  e <- expect_error(window(mock_source(max_in_flight = 2)), class = "opengwasr_api_error")
  expect_equal(e$job_ids, c("job-1", "job-2"))
  expect_equal(e$jobs$job_ids, c("job-1", "job-2"))
  expect_true(e$jobs$partial)
})

test_that("an interrupt part-way through a window reports every Job submitted", {
  mock_jobs(on_status = function(id) if (id == "job-2") rlang::interrupt())
  e <- expect_error(window(mock_source(max_in_flight = 2)), class = "opengwasr_interrupted")
  expect_equal(e$job_ids, c("job-1", "job-2", "job-3"))
  expect_equal(e$jobs$job_ids, e$job_ids)
  expect_true(e$jobs$partial)   # a fourth request was never submitted
})

test_that("a timeout part-way through a window carries every Job submitted, as `job_ids` too", {
  mock_jobs(running = TRUE)
  e <- expect_error(window(mock_source(max_in_flight = 2), deadline = job_now() + 0.1), class = "opengwasr_timeout")
  expect_equal(e$job_ids, c("job-1", "job-2"))
  expect_equal(e$jobs$job_ids, c("job-1", "job-2"))
})

test_that("og_submit() of a plan: a 413 cancels the pieces already submitted; another error carries them", {
  query <- function(src) {
    deadline <- local_query(src)
    run_query(src, query_plan(four, finish = identity), deadline)
  }
  state <- mock_jobs(on_submit = function(n) if (n == 3) refusal_413())
  e <- expect_error(og_submit(mock_source(), query), class = "opengwasr_request_too_large")
  expect_equal(e$cancelled, c("job-1", "job-2"))
  expect_equal(state$deleted, c("job-1", "job-2"))
  expect_equal(state$submits, 3L)

  mock_jobs(on_submit = function(n) if (n == 3) httr2::response_json(400, body = list(error = "invalid_request", message = "bad")))
  e <- expect_error(og_submit(mock_source(), query), class = "opengwasr_api_error")
  expect_equal(e$job_ids, c("job-1", "job-2"))
  expect_true(e$jobs$partial)
})
