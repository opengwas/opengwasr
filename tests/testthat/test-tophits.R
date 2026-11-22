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
                      "effect_scale", "n", "sample_size_kind", "ncase", "ncontrol", "association_status",
                      "release_id")

# Waiting costs nothing here: the fake asks for a poll every 10 ms.
withr::local_options(opengwasr.poll_cap = 0.05, opengwasr.progress = FALSE)

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
})

# Sample size and original units (#8).
ukb <- "OGS-00009:ukb-b-19953"
ukb_rel <- c(rel, "OGS-00009/OGS-00009-completed")
# The dev catalogue has more top hits than these tests ask for; the cut-off isn't what they test.
top_hits <- function(...) {
  withCallingHandlers(og_tophits(..., clump = FALSE), opengwasr_truncated = function(w) invokeRestart("muffleWarning"))
}

test_api("every top hit carries n, sample_size_kind, ncase and ncontrol from the Analysis metadata", {
  top <- top_hits(src, c(cad, bmi), limit = 5, releases = rel)
  expect_true(all(c("n", "sample_size_kind", "ncase", "ncontrol") %in% names(top)))
  expect_false(anyNA(top$n))
  expect_equal(unique(top$n[top$id == cad]), 187599)
  expect_equal(unique(top$sample_size_kind[top$id == cad]), "case_control")
  expect_equal(unique(top$ncase[top$id == cad]), 61289)
  expect_equal(unique(top$ncontrol[top$id == cad]), 126310)
  expect_equal(unique(top$n[top$id == bmi]), 532396)
  expect_equal(unique(top$sample_size_kind[top$id == bmi]), "total")
  expect_true(all(is.na(top$ncase[top$id == bmi])) && all(is.na(top$ncontrol[top$id == bmi])))
})

ensg <- "OGS-00027:ENSG00000000419"
ensg_rel <- "OGS-00027/OGS-00027"

test_api("a per-Association N from the rows is kept, and only a missing one falls back to the metadata", {
  top <- top_hits(src, ensg, limit = 3, releases = ensg_rel)
  expect_false(anyNA(top$n))
  expect_equal(unique(top$sample_size_kind), "variant_level")
  if (backend == "fake") {
    # Rows carry N 30694 and 30508; the third has none, so the Analysis's sample_size (30895) fills in.
    expect_equal(top$n, c(30694, 30508, 30895))
  } else {
    # Live: variant-level N differs from the Analysis's 30,895 for some top hits.
    expect_true(any(top$n != 30895))
  }
})

test_api("provenance records original_units, so rescaled and default answers differ", {
  plain <- top_hits(src, bmi, limit = 6, releases = rel)
  scaled <- top_hits(src, bmi, limit = 6, releases = rel, original_units = TRUE)
  expect_false(og_provenance(plain)$args$original_units)
  expect_true(og_provenance(scaled)$args$original_units)
  expect_match(og_provenance(scaled)$query, "original_units = TRUE", fixed = TRUE)
  expect_false(identical(og_provenance(plain)$query, og_provenance(scaled)$query))
})

test_that("a per-Association N in the rows wins over the Analysis's sample_size", {
  rows <- data.frame(store_id = "OGS-00011", analysis_id = c("GCST003116", "GCST003116"), z = c(5, 4),
                     se = 0.1, log10_p = c(-7, -5), n = c(1000, NA))
  info <- og_analyses(fake_source(fake), cad, releases = rel)
  shaped <- getFromNamespace("shape_associations", "opengwasr")(rows, info)
  expect_equal(shaped$n, c(1000, 187599))
})

test_api("original_units = TRUE rescales a per-SD continuous Analysis by original_sd", {
  sd_top <- top_hits(src, bmi, limit = 6, releases = rel)
  top <- top_hits(src, bmi, limit = 6, releases = rel, original_units = TRUE)
  sd <- og_analyses(src, bmi, releases = rel)$original_sd
  expect_equal(unique(top$effect_scale), "original")
  expect_equal(top$beta, sd_top$beta * sd)
  expect_equal(top$se, sd_top$se * sd)
  expect_equal(top$z, sd_top$z)
  expect_equal(top$pval, sd_top$pval)
  if (backend == "fake") {
    # original_sd 0.9931; beta = z * se = -0.04 and 1.5 per SD.
    expect_equal(top$beta, c(-0.039724, 1.48965))
    expect_equal(top$se, c(0.0009931, 0.248275))
  }
  expect_equal(og_provenance(top)$total, nrow(top))
})

test_api("original_units = TRUE leaves log-odds effects unchanged", {
  plain <- top_hits(src, cad, limit = 5, releases = rel)
  top <- top_hits(src, cad, limit = 5, releases = rel, original_units = TRUE)
  expect_equal(top$beta, plain$beta)
  expect_equal(top$se, plain$se)
  expect_equal(unique(top$effect_scale), "log_or")
})

test_api("original_units = TRUE mixes a rescaled and an unchanged Analysis", {
  top <- top_hits(src, c(cad, bmi), limit = 5, releases = rel, original_units = TRUE)
  expect_equal(unique(top$effect_scale[top$id == cad]), "log_or")
  expect_equal(unique(top$effect_scale[top$id == bmi]), "original")
})

test_api("original_units = TRUE refuses Analyses with no original_sd, naming them", {
  e <- expect_error(
    top_hits(src, c(bmi, ukb), limit = 5, releases = ukb_rel, original_units = TRUE),
    class = "opengwasr_original_units_unavailable"
  )
  expect_s3_class(e, "opengwasr_refusal")
  expect_equal(e$ids, ukb)
  expect_equal(e$reason, "no_original_sd")
})

test_that("a refused original_units call submits no Job", {
  src <- fake_source(fake)
  clock <- local_virtual_clock()
  expect_error(og_tophits(src, c(bmi, ukb), clump = FALSE, releases = ukb_rel, original_units = TRUE),
               class = "opengwasr_original_units_unavailable")
  expect_false(any(grepl("tophits|jobs", vapply(clock$requests, function(r) r$path, ""))))
})

test_that("original_units = TRUE refuses a binary Analysis stored per SD, even when original_sd is set", {
  src <- fake_source(fake)
  fake_set(fake, "binary_sd", "true", "false")
  bin <- "OGS-00009:ukb-b-13745"
  e <- expect_error(
    top_hits(src, bin, releases = ukb_rel, original_units = TRUE),
    class = "opengwasr_original_units_unavailable"
  )
  expect_equal(e$ids, bin)
  expect_equal(e$reason, "binary_per_sd")
  # The rule needs no original_sd: a binary per-SD Analysis that has one is refused too.
  info <- og_analyses(src, bin, releases = ukb_rel)
  info$original_sd <- 2
  rows <- data.frame(store_id = "OGS-00009", analysis_id = "ukb-b-13745", z = 7, se = 0.1, log10_p = -11)
  e2 <- expect_error(getFromNamespace("shape_associations", "opengwasr")(rows, info, original_units = TRUE),
                     class = "opengwasr_original_units_unavailable")
  expect_equal(e2$ids, bin)
  expect_equal(e2$reason, "binary_per_sd")
})

test_that("an Analysis stored per SD is rescaled only when its trait is known to be continuous with an original_sd", {
  shape <- getFromNamespace("shape_associations", "opengwasr")
  rows <- data.frame(store_id = "OGS-00011", analysis_id = "GCST90029007", z = 4, se = 0.5, log10_p = -5)
  info <- og_analyses(fake_source(fake), bmi, releases = rel)
  ok <- shape(rows, info, original_units = TRUE)
  expect_equal(c(ok$beta, ok$se), c(4 * 0.5 * 0.9931, 0.5 * 0.9931))
  expect_equal(ok$effect_scale, "original")
  refused <- function(info, reason) {
    e <- expect_error(shape(rows, info, original_units = TRUE), class = "opengwasr_original_units_unavailable")
    expect_equal(e$ids, bmi)
    expect_equal(e$reason, reason)
  }
  no_sd <- info
  no_sd$original_sd <- NA_real_
  refused(no_sd, "no_original_sd")
  # With the trait type absent or unknown, an Analysis stored per SD is refused, not rescaled by NA.
  no_type <- info
  no_type$outcome_type <- NULL
  refused(no_type, "unknown_trait_type")
  no_type_no_sd <- no_type
  no_type_no_sd$original_sd <- NA_real_
  refused(no_type_no_sd, "no_original_sd")
  na_type <- info
  na_type$outcome_type <- NA_character_
  refused(na_type, "unknown_trait_type")
  # Log-odds Analyses never need either field.
  log_or <- og_analyses(fake_source(fake), cad, releases = rel)
  log_or$outcome_type <- NULL
  log_or$original_sd <- NULL
  lo <- shape(data.frame(store_id = "OGS-00011", analysis_id = "GCST003116", z = 4, se = 0.5, log10_p = -5),
              log_or, original_units = TRUE)
  expect_equal(c(lo$beta, lo$se), c(2, 0.5))
  expect_equal(lo$effect_scale, "log_or")
})

test_api("a binary Analysis stored per SD is refused live", {
  skip_if(backend == "fake")
  e <- expect_error(
    top_hits(src, "OGS-00009:ukb-b-13745", limit = 5, releases = ukb_rel, original_units = TRUE),
    class = "opengwasr_original_units_unavailable"
  )
  expect_equal(e$ids, "OGS-00009:ukb-b-13745")
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

