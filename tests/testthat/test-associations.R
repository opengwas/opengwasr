fake <- local_fake_api(n_synthetic = 1)

cad <- "OGS-00011:GCST003116"
bmi <- "OGS-00011:GCST90029007"
ukb <- "OGS-00009:ukb-b-19953"
# Pinned, so the tests that aren't about discovering Store Releases skip the /api/batches call.
rel <- "OGS-00011/OGS-00011-completed"
snv <- "16:53767042:C:T"
other_snv <- "16:53767042:A:G"
insertion <- "5:75701728:A:AAAAT"

expected_columns <- c("id", "store_id", "analysis_id", "analysis_label", "alid", "rsid", "chromosome",
                      "position", "effect_allele", "other_allele", "eaf", "beta", "se", "z", "pval",
                      "effect_scale", "n", "sample_size_kind", "ncase", "ncontrol", "association_status",
                      "release_id")

withr::local_options(opengwasr.poll_cap = 0.05, opengwasr.progress = FALSE)

# The bodies the fake was sent (oldest first), for tests of the request itself.
fake_bodies <- function(endpoint = NULL) {
  all <- raw_api(fake$url(), fake_key, "_fake/bodies")
  if (is.null(endpoint)) all else Filter(function(b) identical(b$endpoint, endpoint), all)
}

# The expectations for CAD's rows below are literal values the dev Data API gave for these variants
# (z, se, log10_p), with beta = z * se worked by hand. The fake serves the same numbers.
test_api("ALIDs in Analyses: columns, Imputed included by default, observed_only drops them", {
  assoc <- og_associations(src, c(snv, insertion), c(cad, bmi), releases = rel)
  expect_s3_class(assoc, "data.frame")
  expect_equal(names(assoc)[seq_along(expected_columns)], expected_columns)
  expect_false("log10_p" %in% names(assoc))
  expect_equal(nrow(assoc), 4)
  expect_setequal(assoc$id, c(cad, bmi))
  expect_false(anyNA(assoc$rsid))
  expect_false(anyNA(assoc$alid))
  expect_setequal(assoc$alid, c(snv, insertion))
  expect_equal(unique(assoc$effect_scale[assoc$id == cad]), "log_or")
  expect_equal(unique(assoc$effect_scale[assoc$id == bmi]), "sd")
  expect_equal(assoc$association_status[assoc$alid == insertion], c("imputed", "imputed"))
  expect_equal(assoc$association_status[assoc$alid == snv], c("observed", "observed"))

  in_cad <- assoc[assoc$id == cad, ]
  in_cad <- in_cad[match(c(insertion, snv), in_cad$alid), ]
  expect_equal(in_cad$rsid, c("rs34159589", "rs1421085"))
  expect_equal(in_cad$z, c(-0.3066406, 3.0810547), tolerance = 1e-6)
  expect_equal(in_cad$se, c(0.009841919, 0.009628296), tolerance = 1e-6)
  expect_equal(in_cad$beta, c(-0.003017932, 0.029665306), tolerance = 1e-6)
  expect_equal(log10(in_cad$pval), c(-0.1196913, -2.6855665), tolerance = 1e-6)
  if (backend == "fake") {
    in_bmi <- assoc[assoc$id == bmi, ]
    in_bmi <- in_bmi[match(c(insertion, snv), in_bmi$alid), ]
    # z of -12 and 38, se of 0.002: beta = -0.024 and 0.076.
    expect_equal(in_bmi$beta, c(-0.024, 0.076))
    expect_equal(in_bmi$pval[[2]], 0)
  }

  observed <- og_associations(src, c(snv, insertion), c(cad, bmi), observed_only = TRUE, releases = rel)
  expect_equal(nrow(observed), 2)
  expect_equal(unique(observed$association_status), "observed")
  expect_equal(unique(observed$alid), snv)

  # ALIDs need no rsID lookup: one Job. Provenance says what was asked.
  prov <- og_provenance(assoc)
  expect_length(prov$job_ids, 1)
  expect_equal(prov$total, 4)
  expect_false(prov$truncated)
  expect_match(prov$query, "og_associations(", fixed = TRUE)
  expect_match(prov$query, "observed_only = FALSE", fixed = TRUE)
  expect_true(rel %in% prov$store_releases)
  expect_warning(og_provenance(assoc[1:2, ]), class = "opengwasr_provenance_stale")
})

test_api("rsIDs and ALIDs mix; an rsID's every ALID is asked for; unresolvable rsIDs are reported", {
  w <- expect_warning(
    assoc <- og_associations(src, c("rs1421085", snv, "rs34159589", "rs999999999"), cad, releases = rel),
    class = "opengwasr_unresolved_variants"
  )
  expect_equal(w$count, 1L)
  expect_equal(w$ids, "rs999999999")
  expect_match(conditionMessage(w), "rs999999999", fixed = TRUE)
  # The ALID given twice (once by its rsID) is one row per Analysis.
  expect_equal(assoc$alid, c(insertion, snv))
  expect_equal(assoc$rsid, c("rs34159589", "rs1421085"))
  # An rsID lookup and an Associations query: two Jobs, but only the second one's rows are
  # Associations.
  prov <- og_provenance(assoc)
  expect_length(prov$job_ids, 2)
  expect_equal(prov$total, nrow(assoc))

  # The Associations Job is asked for ALIDs only, every ALID each rsID names (the API itself would
  # expand an rsID; opengwasr resolves them, so what was asked is on the record).
  asked <- unlist(utils::tail(fake_bodies("/api/associations"), 1)[[1]]$body$variants)
  if (backend == "fake") {
    expect_setequal(asked, c(other_snv, snv, insertion, "5:75701728:AAAAT:AAAATAAAT"))
  }
  expect_false(any(grepl("^rs", asked)))
  expect_false(any(duplicated(asked)))

  # An rsID that names two ALIDs gives every one an Analysis holds. (Only the fake's BMI holds both.)
  if (backend == "fake") {
    both <- og_associations(src, "rs1421085", bmi, releases = rel)
    expect_setequal(both$alid, c(other_snv, snv))
    # The API sends rsid "." where a variant has no rsID; opengwasr returns NA, not ".".
    expect_equal(both$rsid[both$alid == other_snv], NA_character_)
    expect_equal(both$rsid[both$alid == snv], "rs1421085")
  }

  # Resolved but absent from the Analysis is not "unresolved": no warning, no rows for it.
  none <- expect_no_warning(og_associations(src, "rs6567160", cad, releases = rel))
  expect_equal(none$alid, "18:60161902:C:T")
})

test_api("an rsID resolves to every ALID it names, with the lookup's rows", {
  src_releases <- list(list(store_id = "OGS-00011", release_id = "OGS-00011-completed"))
  got <- resolve_variants(src, c("rs1421085", "rs34159589", "rs999999999", snv), src_releases)
  expect_setequal(got$alids, c(other_snv, snv, insertion, "5:75701728:AAAAT:AAAATAAAT"))
  expect_equal(got$unresolved, "rs999999999")
  expect_setequal(got$map$alid[got$map$rsid == "rs1421085"], c(other_snv, snv))
  expect_setequal(got$map$alid[got$map$rsid == "rs34159589"], c(insertion, "5:75701728:AAAAT:AAAATAAAT"))
  # The whole variant rows, for og_variants_rsid().
  expect_true(all(c("alid", "chromosome", "position", "effect_allele", "other_allele", "rsid",
                    "store_id", "release_id") %in% names(got$lookup)))
  expect_equal(got$lookup$position[got$lookup$alid == snv], 53767042)
  expect_equal(got$lookup$other_allele[got$lookup$alid == "5:75701728:AAAAT:AAAATAAAT"], "AAAATAAAT")
  expect_length(got$job$job_id, 1)
  # No rsIDs, no Job.
  expect_null(resolve_variants(src, snv, src_releases)$job)
})

test_api("the vignette workflow: BMI top hits looked up in CAD, Imputed included and excluded", {
  muffle_truncated <- function(expr) {
    withCallingHandlers(expr, opengwasr_truncated = function(w) invokeRestart("muffleWarning"))
  }
  bmi_top <- muffle_truncated(og_tophits(src, bmi, clump = FALSE, releases = rel))
  outcome <- og_associations(src, bmi_top$alid, cad, releases = rel)
  observed <- og_associations(src, bmi_top$alid, cad, observed_only = TRUE, releases = rel)
  expect_true(all(outcome$alid %in% bmi_top$alid))
  expect_equal(unique(outcome$id), cad)
  expect_true(all(outcome$association_status %in% c("observed", "imputed")))
  expect_equal(nrow(observed), sum(outcome$association_status == "observed"))
  expect_true(any(outcome$association_status == "imputed"))
  expect_lt(nrow(observed), nrow(outcome))
  if (backend == "fake") {
    # BMI's two hits; CAD holds an Imputed Association for the first and an Observed one for the second.
    expect_equal(table(outcome$association_status)[["imputed"]], 1L)
    expect_equal(observed$alid, "16:53800000:A:G")
    expect_equal(observed$beta, 0.04)
  }
})

test_api("original_units is passed to the shaper: per-SD Analyses are rescaled, log-odds ones are not", {
  plain <- og_associations(src, snv, c(cad, bmi), releases = rel)
  scaled <- og_associations(src, snv, c(cad, bmi), original_units = TRUE, releases = rel)
  expect_equal(scaled$effect_scale[scaled$id == cad], "log_or")
  expect_equal(scaled$effect_scale[scaled$id == bmi], "original")
  sd <- og_analyses(src, bmi, releases = rel)$original_sd
  expect_equal(scaled$beta[scaled$id == bmi], plain$beta[plain$id == bmi] * sd)
  expect_equal(scaled$beta[scaled$id == cad], plain$beta[plain$id == cad])
  expect_true(og_provenance(scaled)$args$original_units)
  expect_match(og_provenance(scaled)$query, "original_units = TRUE", fixed = TRUE)
})

test_that("original_units = TRUE refuses an Analysis it can't honour before any Job runs", {
  src <- fake_source(fake)
  before <- length(fake_bodies())
  rels <- c(rel, "OGS-00009/OGS-00009-completed")
  e <- expect_error(og_associations(src, snv, c(bmi, ukb), original_units = TRUE, releases = rels),
                    class = "opengwasr_original_units_unavailable")
  expect_equal(e$ids, ukb)
  expect_length(fake_bodies(), before)
})

test_that("every rsID unresolvable gives no rows, the warning, and the columns", {
  src <- fake_source(fake)
  w <- expect_warning(
    none <- og_associations(src, c("rs999999999", "rs999999998"), cad, releases = rel),
    class = "opengwasr_unresolved_variants"
  )
  expect_equal(w$count, 2L)
  expect_equal(nrow(none), 0)
  expect_equal(names(none)[seq_along(expected_columns)], expected_columns)
  prov <- og_provenance(none)
  expect_length(prov$job_ids, 1)
  expect_equal(prov$total, 0)
})

test_that("variants that can't be ALIDs or rsIDs, and bad arguments, are argument errors", {
  src <- fake_source(fake)
  before <- length(fake_bodies())
  bad <- function(...) {
    e <- expect_error(og_associations(src, ..., releases = rel), class = "opengwasr_argument")
    e
  }
  bad(c(snv, "banana"), cad)
  bad("16:53767042:C", cad)
  bad("16:53767042:C:T:", cad)
  bad("16:53767042:C:T:G", cad)
  bad("chr16:53767042:C:T", cad)
  bad("16:53767042:c:t", cad)
  bad("16:0:C:T", cad)
  bad("16:-5:C:T", cad)
  bad("16:5376.5:C:T", cad)
  bad("16:53767042:C:C", cad)
  bad("16:53767042:C:N", cad)
  bad("16:53767042:T:C", cad)
  bad("rs", cad)
  bad(c(snv, NA), cad)
  bad(character(), cad)
  bad(1234, cad)
  bad(snv, character())
  bad(snv, NA_character_)
  e <- bad("16:53767042:T:C", cad)
  expect_match(paste(conditionMessage(e), collapse = " "), "og_alid", fixed = TRUE)
  expect_error(og_associations(src, snv, cad, observed_only = NA, releases = rel), class = "opengwasr_argument")
  expect_error(og_associations(src, snv, cad, original_units = "yes", releases = rel),
               class = "opengwasr_argument")
  # None of them submitted a Job.
  expect_length(fake_bodies(), before)
  # What og_alid() produces is accepted: an indel (reference first), a substitution, an alt contig, X.
  ok <- getFromNamespace("classify_variants", "opengwasr")
  expect_equal(ok(c("1:827587:GGGGCC:G", "5:75701728:A:AAAAT", "1_KI270706v1_random:1000:A:C", "X:100:AC:GT"))$alids,
               c("1:827587:GGGGCC:G", "5:75701728:A:AAAAT", "1_KI270706v1_random:1000:A:C", "X:100:AC:GT"))
})
