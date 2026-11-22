fake <- local_fake_api(n_synthetic = 1)

# Hand-written expectations (see `fake_tophits()`): beta = z * se and pval = 10^log10_p worked by
# hand from the literal rows, never recomputed the way the code does.
cad <- "OGS-00011:GCST003116"
bmi <- "OGS-00011:GCST90029007"
# Pinned, so the tests that aren't about discovering Store Releases skip the /api/batches call.
rel <- "OGS-00011/OGS-00011-completed"

# The columns every Association-returning function shares (vignette: What the columns mean).
expected_columns <- c("id", "store_id", "analysis_id", "analysis_label", "alid", "rsid", "chromosome",
                      "position", "effect_allele", "other_allele", "eaf", "beta", "se", "z", "pval",
                      "effect_scale", "association_status", "release_id")

# Waiting costs nothing here: the fake asks for a poll every 10 ms.
withr::local_options(opengwasr.poll_cap = 0.05, opengwasr.progress = FALSE)

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
                            timeout = NULL) {
      clock$requests[[length(clock$requests) + 1]] <- list(path = path, at = clock$t,
                                                           timeout = timeout %||% NA_real_)
      for (pattern in names(latency)) {
        if (grepl(pattern, path)) clock$t <- clock$t + latency[[pattern]]
      }
      real_api_response(src, path, query, body, call = call, timeout = NULL)
    },
    .package = "opengwasr", .env = env
  )
  clock
}

# The paths of the Job requests (status and result), with the times they were made.
job_requests <- function(clock) {
  Filter(function(r) grepl("^api/jobs/", r$path), clock$requests)
}

test_api("og_tophits() returns top hits: columns, mixed scales, underflow, truncation, provenance", {
  w <- expect_warning(
    top <- og_tophits(src, c(cad, bmi), clump = FALSE, limit = 3, releases = rel),
    class = "opengwasr_truncated"
  )
  expect_equal(w$n_analyses, 2L)
  expect_match(conditionMessage(w), "At least one", fixed = TRUE)
  expect_s3_class(top, "data.frame")
  expect_equal(names(top)[seq_along(expected_columns)], expected_columns)
  expect_false("log10_p" %in% names(top))
  expect_equal(unique(top$id), c(cad, bmi))
  expect_true(all(top$association_status %in% c("observed", "imputed")))
  expect_true(all(startsWith(top$release_id, "OGS-00011")))
  expect_true(all(top$store_id == "OGS-00011"))
  expect_equal(unique(top$effect_scale[top$id == cad]), "log_or")
  expect_equal(unique(top$effect_scale[top$id == bmi]), "sd")
  expect_equal(top$beta, top$z * top$se)
  expect_true(all(top$pval >= 0 & top$pval <= 1))
  expect_false(is.unsorted(rev(abs(top$z[top$id == cad]))))
  if (backend == "fake") {
    in_cad <- top[top$id == cad, ]
    in_bmi <- top[top$id == bmi, ]
    expect_equal(in_cad$alid, c("9:22098620:A:G", "9:22098575:A:G", "9:22123767:A:C"))
    expect_equal(in_cad$association_status, c("observed", "observed", "imputed"))
    expect_equal(in_cad$z, c(-20, 16, 10))
    expect_equal(in_cad$beta, c(-0.25, 0.2, 0.2))
    # On the log scale and non-zero, so a p-value that underflowed to 0 can't pass.
    expect_true(all(in_cad$pval > 0))
    expect_equal(log10(in_cad$pval), c(-88, -57, -23))
    # |z| of 40: the p-value underflows to 0 while z and beta stay exact.
    expect_equal(in_bmi$z, c(-40, 6))
    expect_equal(in_bmi$beta, c(-0.04, 1.5))
    expect_equal(in_bmi$pval[[1]], 0)
    expect_equal(log10(in_bmi$pval[[2]]), -8)
  }

  prov <- og_provenance(top)
  expect_s3_class(prov, "og_provenance")
  expect_equal(prov$total, nrow(top))
  expect_true(prov$truncated)
  if (backend == "fake") expect_false(prov$existing)
  expect_length(prov$job_ids, 1)
  expect_match(prov$job_ids, "^[0-9a-f-]{36}$")
  expect_true("OGS-00011/OGS-00011-completed" %in% prov$store_releases)
  expect_true(is.numeric(prov$cost_estimate) && prov$cost_estimate > 0)
  expect_match(prov$query, "og_tophits(", fixed = TRUE)
  expect_match(prov$query, "limit = 3", fixed = TRUE)
  expect_s3_class(prov$retrieved, "POSIXct")
  expect_output(print(prov), "Store Releases")

  # Combining or cutting the rows makes the attached provenance stale: it is refused, not reused.
  expect_warning(stale <- og_provenance(rbind(top, top)), class = "opengwasr_provenance_stale")
  expect_null(stale)
  expect_warning(og_provenance(top[1:2, ]), class = "opengwasr_provenance_stale")
  expect_s3_class(og_provenance(top), "og_provenance")
})

test_that("pages are all read, a queued then running Job is waited for, and progress shows each state", {
  src <- fake_source(fake)
  fake_set(fake, "page_rows", "2", "1000")
  fake_set(fake, "job", "queued_running", "normal")
  withr::local_options(opengwasr.progress = TRUE, cli.progress_show_after = 0)
  shown <- character()
  top <- withCallingHandlers(
    og_tophits(src, cad, clump = FALSE, limit = 10, releases = rel),
    message = function(m) {
      shown <<- c(shown, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  shown <- paste(shown, collapse = "\n")
  expect_match(shown, "queued (position 3)", fixed = TRUE)
  expect_match(shown, "running", fixed = TRUE)
  expect_match(shown, "succeeded", fixed = TRUE)
  expect_equal(nrow(top), 5)
  expect_equal(top$z, c(-20, 16, 10, -8, 5.6))
  expect_equal(top$beta, c(-0.25, 0.2, 0.2, -0.4, 0.28))
  expect_equal(log10(top$pval), c(-88, -57, -23, -15, -7.5))
  expect_equal(og_provenance(top)$total, 5)
  expect_false(og_provenance(top)$truncated)
})

test_that("observed_only drops imputed Associations", {
  src <- fake_source(fake)
  top <- og_tophits(src, cad, clump = FALSE, observed_only = TRUE, releases = rel)
  expect_true(all(top$association_status == "observed"))
  expect_equal(nrow(top), 4)
})

test_that("an Analysis with no top hits gives an empty data frame with the same columns", {
  src <- fake_source(fake)
  top <- og_tophits(src, "OGS-00017:finngen-r13-I9_CHD", clump = FALSE,
                    releases = "OGS-00017/OGS-00017-completed")
  expect_equal(nrow(top), 0)
  expect_true(all(expected_columns %in% names(top)))
  expect_equal(og_provenance(top)$total, 0)
})

test_that("resubmitting a query reattaches to the Job the API already has", {
  src <- fake_source(fake)
  first <- suppressWarnings(og_tophits(src, cad, clump = FALSE, limit = 4, releases = rel))
  second <- suppressWarnings(og_tophits(src, cad, clump = FALSE, limit = 4, releases = rel))
  expect_false(og_provenance(first)$existing)
  expect_true(og_provenance(second)$existing)
  expect_equal(og_provenance(second)$job_ids, og_provenance(first)$job_ids)
  expect_equal(og_provenance(second)$total, 4)
  expect_equal(second$z, first$z)
})

# --- Waiting for a Job -----------------------------------------------------------------------

test_that("waits start at the server's interval, grow by half, saturate at the cap, and end at the timeout", {
  fake_set(fake, "job", "never", "normal")
  fake_set(fake, "poll_after_ms", "1000", "10")
  withr::local_options(opengwasr.poll_cap = 4)
  clock <- local_virtual_clock()
  src <- fake_source(fake, timeout = 30)
  e <- expect_error(og_tophits(src, cad, clump = FALSE, limit = 6, releases = rel),
                    class = "opengwasr_timeout")
  expect_s3_class(e, "opengwasr_error")
  expect_length(e$job_ids, 1)
  expect_match(conditionMessage(e), e$job_ids, fixed = TRUE)
  expect_match(conditionMessage(e), "30", fixed = TRUE)
  # 1 s, then 1.5, 2.25, 3.375 (growing by half), then the 4 s cap five times, then what is left.
  expect_equal(clock$waits, c(1, 1.5, 2.25, 3.375, rep(4, 5), 1.875))
  expect_equal(clock$t, 30)
  # No Job request at or after the deadline, and each one's own timeout is what is left of the budget.
  jobs <- job_requests(clock)
  expect_true(all(vapply(jobs, function(r) r$at < 30, NA)))
  expect_equal(vapply(jobs, function(r) r$timeout, 0), vapply(jobs, function(r) 30 - r$at, 0))
})

test_that("a cap below the server's interval never shortens it", {
  fake_set(fake, "job", "never", "normal")
  fake_set(fake, "poll_after_ms", "2000", "10")
  withr::local_options(opengwasr.poll_cap = 0.5)
  clock <- local_virtual_clock()
  src <- fake_source(fake, timeout = 10)
  expect_error(og_tophits(src, cad, clump = FALSE, limit = 7, releases = rel), class = "opengwasr_timeout")
  expect_equal(clock$waits, rep(2, 5))
})

test_that("the deadline is hard: a late answer is refused, whichever response it is", {
  src <- fake_source(fake, timeout = 30)
  # A slow status: the Job has succeeded, but too late.
  clock <- local_virtual_clock(latency = list("^api/jobs/[^/]+$" = 40))
  expect_error(og_tophits(src, cad, clump = FALSE, limit = 8, releases = rel), class = "opengwasr_timeout")
  # A slow submission.
  clock <- local_virtual_clock(latency = list("api/tophits" = 40))
  expect_error(og_tophits(src, cad, clump = FALSE, limit = 12, releases = rel), class = "opengwasr_timeout")
  # A slow final result page, after a status that was in time.
  clock <- local_virtual_clock(latency = list("/result$" = 40))
  expect_error(og_tophits(src, cad, clump = FALSE, limit = 13, releases = rel), class = "opengwasr_timeout")
  # A slow re-read of a Job that had already finished (reattaching).
  clock <- local_virtual_clock()
  suppressWarnings(og_tophits(src, cad, clump = FALSE, limit = 9, releases = rel))
  clock <- local_virtual_clock(latency = list("^api/jobs/[^/]+$" = 40))
  expect_error(og_tophits(src, cad, clump = FALSE, limit = 9, releases = rel), class = "opengwasr_timeout")
  # Everything in time is fine.
  clock <- local_virtual_clock(latency = list("api/" = 5))
  expect_no_error(suppressWarnings(og_tophits(src, cad, clump = FALSE, limit = 10, releases = rel)))
})

test_that("the deadline starts with the call: looking up the Analyses counts", {
  src <- fake_source(fake, timeout = 30)
  clock <- local_virtual_clock(latency = list("api/gwasinfo" = 31))
  e <- expect_error(og_tophits(src, cad, clump = FALSE, limit = 11, releases = rel),
                    class = "opengwasr_timeout")
  # The query never went out.
  expect_false(any(vapply(clock$requests, function(r) grepl("tophits", r$path), NA)))
})

test_that("a request that runs out of time is the call's timeout, not an unreachable API", {
  fake_set(fake, "delay_ms", "100", "0")
  src <- fake_source(fake, timeout = 0.15)
  started <- proc.time()[["elapsed"]]
  expect_error(og_tophits(src, cad, clump = FALSE, limit = 8, releases = rel), class = "opengwasr_timeout")
  expect_lt(proc.time()[["elapsed"]] - started, 0.15 + 0.5)
})

test_that("a failed Job raises opengwasr_job_failed with error_kind and the server's message", {
  src <- fake_source(fake)
  fake_set(fake, "job", "failed_request", "normal")
  e <- expect_error(og_tophits(src, cad, clump = FALSE, releases = rel), class = "opengwasr_job_failed")
  expect_s3_class(e, "opengwasr_error")
  expect_equal(e$error_kind, "request")
  expect_false(e$retryable)
  expect_match(e$api_message, "worker rejected the query", fixed = TRUE)
  expect_length(e$job_id, 1)

  fake_set(fake, "job", "failed_temporary", "normal")
  e <- expect_error(og_tophits(src, cad, clump = FALSE, releases = rel), class = "opengwasr_job_failed")
  expect_equal(e$error_kind, "temporary")
  expect_true(e$retryable)
})

test_that("server text never carries the key: failure reason, error_kind, ignored fields", {
  src <- fake_source(fake)
  fake_set(fake, "job", "failed_leak", "normal")
  e <- expect_error(og_tophits(src, cad, clump = FALSE, releases = rel), class = "opengwasr_job_failed")
  expect_false(grepl(fake_key, paste(conditionMessage(e), e$api_message, e$error_kind), fixed = TRUE))
  expect_match(e$api_message, "worker rejected", fixed = TRUE)

  fake_set(fake, "job", "ignored", "normal")
  e <- expect_error(og_tophits(src, cad, clump = FALSE, releases = rel), class = "opengwasr_ignored_fields")
  expect_s3_class(e, "opengwasr_error")
  expect_true("observed_onyl" %in% e$ignored_fields)
  expect_false(grepl(fake_key, paste(conditionMessage(e), paste(e$ignored_fields, collapse = " ")), fixed = TRUE))
})

test_that("a cancelled or expired Job, or a result that is gone, is classed", {
  src <- fake_source(fake)
  fake_set(fake, "job", "cancelled", "normal")
  e <- expect_error(og_tophits(src, cad, clump = FALSE, releases = rel), class = "opengwasr_job_cancelled")
  expect_length(e$job_id, 1)
  fake_set(fake, "job", "expired", "normal")
  e <- expect_error(og_tophits(src, cad, clump = FALSE, releases = rel), class = "opengwasr_job_expired")
  expect_length(e$job_id, 1)
  fake_set(fake, "job", "result_gone", "normal")
  expect_error(og_tophits(src, cad, clump = FALSE, releases = rel), class = "opengwasr_result_expired")
})

test_that("an answer the client can't trust raises opengwasr_job_invalid, never a partial result", {
  src <- fake_source(fake)
  for (defect in c("bad_status", "no_result", "no_ndjson", "bad_ndjson", "short_rows", "no_metadata",
                   "total_too_big", "total_too_small")) {
    fake_set(fake, "job", defect, "normal")
    e <- expect_error(og_tophits(src, cad, clump = FALSE, releases = rel),
                      class = "opengwasr_job_invalid", info = defect)
    expect_s3_class(e, "opengwasr_error")
    # Nothing the server sent, such as the key it echoed, appears in the message.
    expect_false(grepl(fake_key, conditionMessage(e), fixed = TRUE), info = defect)
  }
})

# --- what the API or the arguments refuse ---------------------------------------------------

test_that("a threshold looser than the GWAS Store indexes is an opengwasr_unsupported_threshold refusal", {
  src <- fake_source(fake)
  e <- expect_error(og_tophits(src, cad, threshold = 0.001, clump = FALSE, releases = rel),
                    class = "opengwasr_unsupported_threshold")
  expect_s3_class(e, "opengwasr_refusal")
  expect_s3_class(e, "opengwasr_error")
})

test_that("clumping and original units say which ticket brings them", {
  src <- fake_source(fake)
  e <- expect_error(og_tophits(src, cad), class = "opengwasr_not_implemented")
  expect_s3_class(e, "opengwasr_error")
  expect_match(conditionMessage(e), "#13", fixed = TRUE)
  expect_error(og_tophits(src, cad, clump = TRUE), class = "opengwasr_not_implemented")
  e <- expect_error(og_tophits(src, cad, clump = FALSE, original_units = TRUE),
                    class = "opengwasr_not_implemented")
  expect_match(conditionMessage(e), "#8", fixed = TRUE)
})

test_that("more Store Releases than one Job holds is refused, naming the splitting ticket", {
  fake_set(fake, "many_stores", "true", "false")
  src <- fake_source(fake)
  ids <- sprintf("OGS-%05d:MANY%02d", 2000 + 1:11, 1:11)
  pins <- sprintf("OGS-%05d/OGS-%05d-r", 2000 + 1:11, 2000 + 1:11)
  # Eleven pinned Store Releases, ten Analyses: only the Analyses' own Store Releases go to the Job.
  top <- og_tophits(src, ids[1:10], clump = FALSE, releases = pins)
  expect_equal(nrow(top), 0)
  e <- expect_error(og_tophits(src, ids, clump = FALSE, releases = pins), class = "opengwasr_not_implemented")
  expect_equal(e$ticket, "#9")
})

test_that("og_tophits() is a Source generic, and checks its arguments", {
  src <- fake_source(fake)
  expect_true(is.function(getS3method("og_tophits", "og_api_source")))
  expect_error(og_tophits(src, cad, clump = FALSE, r2 = 0.01), class = "opengwasr_argument")
  expect_error(og_tophits(src, cad, clump = FALSE, kb = 500), class = "opengwasr_argument")
  expect_error(og_tophits(src, cad, clump = FALSE, ld = "EUR"), class = "opengwasr_argument")
  expect_error(og_tophits(src, 1:3, clump = FALSE), class = "opengwasr_argument")
  expect_error(og_tophits(src, character(), clump = FALSE), class = "opengwasr_argument")
  expect_error(og_tophits(src, cad, threshold = 2, clump = FALSE), class = "opengwasr_argument")
  expect_error(og_tophits(src, cad, threshold = c(1e-8, 1e-6), clump = FALSE), class = "opengwasr_argument")
  expect_error(og_tophits(src, cad, limit = 0, clump = FALSE), class = "opengwasr_argument")
  expect_error(og_tophits(src, cad, limit = 10001, clump = FALSE), class = "opengwasr_argument")
  expect_error(og_tophits(src, cad, observed_only = NA, clump = FALSE), class = "opengwasr_argument")
  expect_error(og_tophits(src, cad, clump = NA), class = "opengwasr_argument")
})

test_that("og_provenance() says so when a data frame has none", {
  expect_error(og_provenance(data.frame(a = 1)), class = "opengwasr_no_provenance")
})
