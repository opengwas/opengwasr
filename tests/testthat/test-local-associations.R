# A Local Source answers og_associations(), og_tophits(clump = FALSE), og_phewas() and og_range()
# (#33): the same calls, columns, order, refusals and provenance as the API Source.
#
# Reads use the example Store Releases bundled in inst/stores (helper-local.R) and skip without a
# Python that has ogquery. Every number is a literal from ogquery's contract answers for the same
# release (tests/contract/expected/{dense_ranges,ragged_completed,hybrid_observed}.json), read by
# eye and converted by hand: beta = z * se, and n is the Analysis's sample_size (analyses.tsv). The
# one value those answers never print is 1:5000:C:T's z in s:bmi_irn (-9; its heads stop at two
# rows), read from the release with ogquery's own facade.
#
# Store Releases (helper-local.R): dense is s/r (one Analysis, bmi_irn, n 500,000; 8 variants on 1,
# 2 and X), ragged is residual-rag/comp (Reference-Completed: a and b, n 50,000) and hyb is hyb/v1
# (trait_a and trait_b, n 1,000).

# The column layout every Association-returning function shares (vignette: What the columns mean).
local_columns <- c("id", "store_id", "analysis_id", "analysis_label", "alid", "rsid", "chromosome",
                   "position", "effect_allele", "other_allele", "eaf", "beta", "se", "z", "pval",
                   "effect_scale", "n", "sample_size_kind", "ncase", "ncontrol", "association_status",
                   "release_id")

# Associations -----------------------------------------------------------------------------------

test_that("og_associations() on a Local Source returns the contract's Associations, API columns first", {
  skip_if_no_local()
  w <- expect_warning(
    x <- og_associations(three(), c("1:5000:C:G", "rs1003", "rs424242"), c("s:bmi_irn", "residual-rag:a")),
    class = "opengwasr_unresolved_variants"
  )
  expect_equal(w$ids, "rs424242")
  expect_equal(names(x), local_columns)
  expect_equal(x$id, c("s:bmi_irn", "residual-rag:a"))
  expect_equal(x$store_id, c("s", "residual-rag"))
  expect_equal(x$release_id, c("r", "comp"))
  expect_equal(x$analysis_id, c("bmi_irn", "a"))
  expect_equal(x$analysis_label, c("BMI_IRN", ""))
  expect_equal(x$alid, c("1:5000:C:G", "1:1001500:A:G"))
  expect_equal(x$rsid, c("rs5000", "rs1003"))
  expect_equal(x$chromosome, c("1", "1"))
  expect_identical(x$position, c(5000L, 1001500L))
  expect_equal(x$effect_allele, c("C", "A"))
  expect_equal(x$other_allele, c("G", "G"))
  # dense_ranges phewas/1:5000:C:G; ragged_completed top_hits/limit's imputed row (variant 3, a).
  expect_equal(x$z, c(8, 1.326171875))
  expect_equal(x$se, c(0.01000213623046875, 0.17525450885295868))
  expect_equal(x$eaf, c(0.75, 0.05150249972939491))
  expect_equal(x$beta, c(8 * 0.01000213623046875, 1.326171875 * 0.17525450885295868))
  expect_equal(x$pval, c(1.2441921148543639e-15, 0.1847827729761612), tolerance = 1e-12)
  expect_equal(x$association_status, c("observed", "imputed"))
  expect_equal(x$effect_scale, c("sd", "sd"))
  expect_equal(x$n, c(500000, 50000))
  expect_equal(x$ncase, c(NA_real_, NA_real_))
})

test_that("a local rsID stands for every ALID it names, and Associations come in position order", {
  skip_if_no_local()
  # rs5000 names 1:5000:A:C and 1:5000:C:G in s (dense_ranges' multiallelic rsID).
  x <- og_associations(three(), c("X:2500:G:T", "rs5000"), "bmi_irn")
  expect_equal(x$alid, c("1:5000:A:C", "1:5000:C:G", "X:2500:G:T"))
  expect_equal(x$z, c(7, 8, -8))
  expect_equal(x$eaf, c(0.30000001192092896, 0.75, 0.550000011920929))
})

test_that("observed_only = TRUE leaves out a local Imputed Association", {
  skip_if_no_local()
  all <- og_associations(three(), "rs1003", c("residual-rag:a", "residual-rag:b"))
  expect_equal(all$association_status, c("imputed", "imputed"))
  observed <- og_associations(three(), "rs1003", c("residual-rag:a", "residual-rag:b"), observed_only = TRUE)
  expect_equal(nrow(observed), 0)
  expect_equal(names(observed), local_columns)
})

test_that("a local Association the release holds as missing comes back as the API sends it", {
  skip_if_no_local()
  # ragged_completed holds 1:1003500:A:G (rs1007) as missing in both Analyses: no z, se or EAF
  # ("." from ogquery), which the API passes on as it reads them.
  x <- og_associations(three(), "1:1003500:A:G", "residual-rag:a")
  expect_equal(x$association_status, "missing")
  expect_true(is.nan(x$z))
  expect_identical(x$eaf, NA_real_)
})

test_that("original_units = TRUE refuses a local per-SD Analysis without original_sd, as on the API", {
  skip_if_no_local()
  e <- expect_error(og_associations(three(), "rs1003", "residual-rag:a", original_units = TRUE),
                    class = "opengwasr_original_units_unavailable")
  expect_equal(e$ids, "residual-rag:a")
  expect_equal(e$reason, "no_original_sd")
})

test_that("provenance of a local result names the Store Releases read and records the call", {
  skip_if_no_local()
  x <- og_associations(three(), c("1:5000:C:G", "rs1003"), c("s:bmi_irn", "residual-rag:a"))
  prov <- og_provenance(x)
  expect_setequal(prov$store_releases, c("s/r", "residual-rag/comp"))
  expect_equal(prov$job_ids, character())
  expect_equal(prov$query, "og_associations(observed_only = FALSE, original_units = FALSE)")
  expect_equal(prov$args$variants, c("1:5000:C:G", "rs1003"))
  expect_equal(prov$args$analyses, c("s:bmi_irn", "residual-rag:a"))
  expect_equal(prov$returned, 2)
  expect_false(prov$truncated)
})

# Top hits ---------------------------------------------------------------------------------------

test_that("og_tophits(clump = FALSE) on a Local Source gives the contract's top hits, strongest first", {
  skip_if_no_local()
  x <- og_tophits(three(), "s:bmi_irn", clump = FALSE)
  expect_equal(names(x), local_columns)
  # By |z|, then genome order (chromosome, position, ALID), as the API orders them.
  expect_equal(x$alid, c("1:5000:C:T", "1:5000:C:G", "X:2500:G:T", "1:5000:A:C", "2:300:C:T", "2:7000:A:C",
                         "1:1000:A:G", "1:9000:A:G"))
  expect_equal(x$z, c(-9, 8, -8, 7, 7, 7, -6, -6))
  expect_equal(x$rsid, c(NA, "rs5000", "rs1007", "rs5000", "rs1005", "rs1006", "rs1000", "rs1004"))
  # 1:1000:A:G's SE is infinite (dense_ranges' planted legacy state): its beta is NaN, as ogquery
  # derives it, not z * Inf. Its pval is what ogquery's row carries (10^log10_p): NaN, as the
  # contract gives it ("NaN").
  expect_identical(x$se[[7]], Inf)
  expect_true(is.nan(x$beta[[7]]))
  expect_true(is.nan(x$pval[[7]]))
  prov <- og_provenance(x)
  expect_equal(prov$total, 8)
  expect_false(prov$truncated)
  expect_equal(prov$store_releases, "s/r")
})

test_that("a local limit cuts each Analysis's top hits off, with the truncation warning", {
  skip_if_no_local()
  w <- expect_warning(x <- og_tophits(three(), c("hyb:trait_b", "s:bmi_irn"), limit = 3, clump = FALSE),
                      class = "opengwasr_truncated")
  expect_equal(w$n_analyses, 2L)
  expect_equal(x$id, c(rep("hyb:trait_b", 3), rep("s:bmi_irn", 3)))
  # hybrid_observed top_hits/5e-08/trait_b: four hits, all z = -8, so genome order decides.
  expect_equal(x$alid, c("1:1000:A:G", "1:51000:A:G", "1:101000:A:AACGTACGTACGT",
                         "1:5000:C:T", "1:5000:C:G", "X:2500:G:T"))
  expect_true(og_provenance(x)$truncated)
  # Raising the limit returns them all, uncut.
  expect_no_warning(all <- og_tophits(three(), "hyb:trait_b", limit = 4, clump = FALSE))
  expect_equal(nrow(all), 4)
})

test_that("a local threshold reads the indexed tier at or above it, and refuses a looser one as the API does", {
  skip_if_no_local()
  # 1e-7 reads the 5e-6 tier and keeps |z| >= z_critical(1e-7), about 5.33: all eight.
  expect_equal(nrow(og_tophits(three(), "s:bmi_irn", threshold = 1e-7, clump = FALSE)), 8)
  # 1e-9 (|z| >= about 6.11) leaves out the two at |z| = 6.
  expect_equal(nrow(og_tophits(three(), "s:bmi_irn", threshold = 1e-9, clump = FALSE)), 6)
  e <- expect_error(og_tophits(three(), "s:bmi_irn", threshold = 1e-3, clump = FALSE),
                    class = "opengwasr_unsupported_threshold")
  expect_s3_class(e, "opengwasr_refusal")
  # A list, as the API Source carries the API's body field.
  expect_identical(e$indexed_thresholds, list(5e-8, 5e-6, 5e-4))
  # A threshold outside (0, 1) is refused as the API's body model refuses it (ogdb-api schemas.py:225,
  # errors.py:56-64): `invalid_request`.
  e <- expect_error(og_tophits(three(), "s:bmi_irn", threshold = 1, clump = FALSE), class = "opengwasr_invalid_request")
  expect_identical(e$api_error, "invalid_request")
  # The API's message, never reworded (ADR 0002); what was wrong is in `reason`.
  expect_identical(conditionMessage(e), "Malformed request")
  expect_match(e$reason, "threshold")
  # A Hybrid release's own tiers (its overflow's, ogquery facade.py:785) are found too.
  e <- expect_error(og_tophits(three(), "hyb:trait_a", threshold = 1e-3, clump = FALSE),
                    class = "opengwasr_unsupported_threshold")
  # A list, as the API Source carries the API's body field.
  expect_identical(e$indexed_thresholds, list(5e-8, 5e-6, 5e-4))
})

test_that("observed_only = TRUE leaves out a local Imputed top hit", {
  skip_if_no_local()
  # ragged_completed top_hits/limit (threshold 0.2 isn't indexed here, so use the 5e-4 tier: a's
  # imputed rows are all weaker than that, and every one of its 60 hits is observed).
  x <- og_tophits(three(), "residual-rag:a", threshold = 5e-4, limit = 10000, clump = FALSE, observed_only = TRUE)
  expect_equal(nrow(x), 60)
  expect_true(all(x$association_status == "observed"))
})

# Clumping on a Local Source needs its LD resource (#34): test-local-ld.R.
test_that("og_tophits(clump = TRUE) on a Local Source with no LD resource is refused", {
  skip_if_no_local()
  expect_error(og_tophits(three(), "s:bmi_irn"), class = "opengwasr_ld_resource_required")
})

# PheWAS -----------------------------------------------------------------------------------------

test_that("og_phewas() on a Local Source searches every Analysis held, in the API's order", {
  skip_if_no_local()
  x <- og_phewas(three(), "1:1000:A:G", threshold = 1e-9)
  expect_equal(names(x), local_columns)
  # hybrid_observed phewas/1:1000:A:G: z = -8 in both Analyses; s's is z = -6, above 1e-9.
  expect_equal(x$id, c("hyb:trait_a", "hyb:trait_b"))
  expect_equal(x$z, c(-8, -8))
  expect_equal(x$se[[1]], 0.16141442954540253)
  expect_equal(x$eaf, c(0.949999988079071, 0.949999988079071))
  expect_equal(x$n, c(1000, 1000))
  expect_equal(og_provenance(x)$total, 2)
  # At 1e-8 s's z = -6 (p about 2e-9) passes too.
  expect_equal(og_phewas(three(), "1:1000:A:G")$id, c("hyb:trait_a", "hyb:trait_b", "s:bmi_irn"))
})

test_that("a local PheWAS takes rsIDs, named Analyses and observed_only, and warns when cut off", {
  skip_if_no_local()
  w <- expect_warning(x <- og_phewas(three(), c("rs5000", "rs424242")), class = "opengwasr_unresolved_variants")
  expect_equal(w$ids, "rs424242")
  expect_equal(x$alid, c("1:5000:A:C", "1:5000:C:G"))
  expect_equal(x$z, c(7, 8))
  expect_equal(og_phewas(three(), "1:1000:A:G", analyses = "hyb:trait_b")$id, "hyb:trait_b")
  expect_equal(og_phewas(three(), "1:1001500:A:G", threshold = 1)$association_status, c("imputed", "imputed"))
  expect_equal(nrow(og_phewas(three(), "1:1001500:A:G", threshold = 1, observed_only = TRUE)), 0)
  w <- expect_warning(cut <- og_phewas(three(), "1:1000:A:G", threshold = 1e-9, limit = 1),
                      class = "opengwasr_truncated")
  expect_equal(cut$id, "hyb:trait_a")
  prov <- og_provenance(cut)
  expect_equal(prov$total, 2)
  expect_true(prov$truncated)
})

# Ranges -----------------------------------------------------------------------------------------

test_that("og_range() on a Local Source returns the contract's range, in position order", {
  skip_if_no_local()
  # dense_ranges range_phewas/shared: the three variants at 1:5000.
  x <- og_range(three(), "s:bmi_irn", chromosome = "1", start = 4000, end = 6000)
  expect_equal(names(x), local_columns)
  expect_equal(x$alid, c("1:5000:A:C", "1:5000:C:G", "1:5000:C:T"))
  expect_equal(x$z[1:2], c(7, 8))
  # from_before: 1:1000 to 1:5000.
  expect_equal(nrow(og_range(three(), "s:bmi_irn", chromosome = "1", start = 1000, end = 5000)), 4)
  # Every Analysis asked, in the order asked; a range none holds has no rows.
  both <- og_range(three(), c("hyb:trait_b", "s:bmi_irn"), chromosome = "1", start = 1000, end = 1000)
  expect_equal(both$id, c("hyb:trait_b", "s:bmi_irn"))
  expect_equal(nrow(og_range(three(), "s:bmi_irn", chromosome = "2", start = 400, end = 6999)), 0)
  expect_setequal(og_provenance(both)$store_releases, c("hyb/v1", "s/r"))
})

test_that("a local range takes observed_only, and refuses a chromosome the API refuses", {
  skip_if_no_local()
  all <- og_range(three(), "residual-rag:a", chromosome = "1", start = 1001500, end = 1001500)
  expect_equal(all$association_status, "imputed")
  expect_equal(nrow(og_range(three(), "residual-rag:a", chromosome = "1", start = 1001500, end = 1001500,
                             observed_only = TRUE)), 0)
  # A chromosome other than 1-22, X, Y or MT is refused as the API's body model refuses it
  # (ogdb-api schemas.py:35-44 and 166, errors.py:56-64): `invalid_request`.
  e <- expect_error(og_range(three(), "s:bmi_irn", chromosome = "27", start = 1, end = 10), class = "opengwasr_invalid_request")
  expect_identical(e$api_error, "invalid_request")
  expect_identical(conditionMessage(e), "Malformed request")
  expect_match(e$reason, "chromosome '27'", fixed = TRUE)
  expect_error(og_range(three(), "s:bmi_irn", chromosome = "Z", start = 1, end = 10), class = "opengwasr_invalid_request")
})

test_that("a local PheWAS threshold or limit out of range is refused as the API's body model refuses it", {
  skip_if_no_local()
  # ogdb-api schemas.py:192-194 (threshold in (0, 1], limit 1-10,000), refused before anything else
  # with 400 invalid_request "Malformed request" (main.py:773-784, errors.py:56-64).
  e <- expect_error(og_phewas(three(), "1:9000:A:G", limit = 0), class = "opengwasr_invalid_request")
  expect_identical(e$api_error, "invalid_request")
  expect_identical(conditionMessage(e), "Malformed request")
  expect_match(e$reason, "limit")
  e <- expect_error(og_phewas(three(), "1:9000:A:G", threshold = 0), class = "opengwasr_invalid_request")
  expect_identical(e$api_error, "invalid_request")
  expect_identical(conditionMessage(e), "Malformed request")
  expect_match(e$reason, "threshold")
})

# What the API refuses, refused the same way ------------------------------------------------------

# ogquery's resolved rows for s:bmi_irn at 1:5000:C:G (dense_ranges phewas/1:5000:C:G), one per
# `z`: what local_resolve() returns, for the tests that mock it.
resolved_rows <- function(z) {
  n <- length(z)
  list(analysis_id = rep("bmi_irn", n), analysis_label = rep("BMI_IRN", n), chromosome = rep("1", n),
       position = rep(5000L, n), alid = rep("1:5000:C:G", n), effect_allele = rep("C", n),
       other_allele = rep("G", n), z = z, se = rep(0.01000213623046875, n), beta = z * 0.01000213623046875,
       pval = rep(1.2441921148543639e-15, n), log10_p = rep(-14.905112555353176, n), eaf = rep(0.75, n),
       association_status = rep("observed", n), rsid = rep("rs5000", n), source_alid = rep("1:5000:C:G", n))
}

test_that("two local rows for one Association collapse when they agree, and are refused when they don't", {
  skip_if_no_local()
  local_mocked_bindings(local_resolve = function(ogquery, facade, record, keep = NULL) resolved_rows(c(8, 8)))
  expect_equal(og_associations(three(), "1:5000:C:G", "s:bmi_irn")$z, 8)
  local_mocked_bindings(local_resolve = function(ogquery, facade, record, keep = NULL) resolved_rows(c(8, 7)))
  e <- expect_error(og_associations(three(), "1:5000:C:G", "s:bmi_irn"), class = "opengwasr_conflicting_rows")
  expect_equal(e$api_error, "conflicting_rows")
  expect_equal(e$alid, "1:5000:C:G")
  expect_equal(e$analysis_id, "bmi_irn")
  expect_equal(e$store_id, "s")
  expect_equal(e$release_id, "r")
})

test_that("a local PheWAS refuses a Store Release without a by-variant index, as the API does", {
  skip_if_no_local()
  # ogquery 0.1.0 opens no Ragged release without its index (below), so the index is mocked away at
  # the boundary for the API's own check.
  local_mocked_bindings(local_variant_indexed = function(ogquery, release) FALSE)
  expect_error(og_phewas(three(), "1:1000:A:G"), class = "opengwasr_local_unsupported")
  expect_equal(nrow(og_associations(three(), "1:1000:A:G", "s:bmi_irn")), 1)
})

test_that("a Ragged Store Release without its by-variant index is refused, not ogquery's exception", {
  skip_if_no_local()
  src <- og_local(release_without(file.path("data.zarr", "ragged", "by_variant")))
  expect_error(og_phewas(src, "1:1000000:A:G"), class = "opengwasr_local_unsupported")
  expect_error(og_associations(src, "1:1000000:A:G", "residual-rag:a"), class = "opengwasr_local_unsupported")
})

test_that("a local query of Associations refuses a Store Release that holds only top hits, as the API does", {
  skip_if_no_local()
  # ogquery reads no such manifest (its AssociationCoverage has no `top_hits`), so the coverage is
  # mocked at the boundary.
  local_mocked_bindings(local_association_coverage = function(release) "top_hits")
  expect_error(og_associations(three(), "1:5000:C:G", "s:bmi_irn"), class = "opengwasr_local_unsupported")
  expect_error(og_range(three(), "s:bmi_irn", chromosome = "1", start = 1, end = 10), class = "opengwasr_local_unsupported")
  expect_error(og_phewas(three(), "1:5000:C:G"), class = "opengwasr_local_unsupported")
})

# Original units ---------------------------------------------------------------------------------

test_that("original_units = TRUE rescales a local per-SD continuous Analysis by its original_sd", {
  skip_if_no_local()
  # s:bmi_irn made a continuous trait (sample_size_kind "total") with a phenotype SD of 2.5.
  path <- release_with_analyses(function(table) {
    table$sample_size_kind[table$analysis_id == "bmi_irn"] <- "total"
    table$original_sd[table$analysis_id == "bmi_irn"] <- "2.5"
    table
  })
  src <- og_local(path)
  x <- og_associations(src, "1:5000:C:G", "s:bmi_irn", original_units = TRUE)
  # dense_ranges phewas/1:5000:C:G: z = 8, se = 0.01000213623046875.
  expect_equal(x$se, 0.01000213623046875 * 2.5)
  expect_equal(x$beta, 8 * 0.01000213623046875 * 2.5)
  expect_equal(x$z, 8)
  expect_equal(x$effect_scale, "original")
  expect_equal(og_associations(src, "1:5000:C:G", "s:bmi_irn")$effect_scale, "sd")
})
