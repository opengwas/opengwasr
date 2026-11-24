# A virtual clock for the Job client: `job_now()` reads it, `job_sleep()` advances it (recording the
# wait), and every request advances it by `latency` (seconds, a named list of path patterns) before
# the real fake answers. No test sleeps: waits are exact and requests are observed against the clock.
local_virtual_clock <- function(latency = list(), env = parent.frame()) {
  clock <- new.env()
  clock$t <- 0
  clock$waits <- numeric()
  clock$requests <- list()
  real_api_response <- getFromNamespace("api_response", "opengwasr")
  testthat::local_mocked_bindings(
    job_now = function() clock$t,
    job_sleep = function(seconds) {
      clock$waits <- c(clock$waits, seconds)
      clock$t <- clock$t + seconds
    },
    api_response = function(src, path, query = NULL, body = NULL, call = rlang::caller_env(),
                            timeout = NULL, method = NULL, deadline = NULL) {
      clock$requests[[length(clock$requests) + 1]] <- list(path = path, at = clock$t,
                                                           timeout = timeout %||% NA_real_)
      for (pattern in names(latency)) {
        if (grepl(pattern, path)) clock$t <- clock$t + latency[[pattern]]
      }
      real_api_response(src, path, query, body, call = call, timeout = NULL, method = method,
                        deadline = deadline)
    },
    .package = "opengwasr", .env = env
  )
  clock
}

# The paths of the Job requests (status and result), with the times they were made.
job_requests <- function(clock) {
  Filter(function(r) grepl("^api/jobs/", r$path), clock$requests)
}
