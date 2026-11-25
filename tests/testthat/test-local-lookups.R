# A Local Source answers og_analyses() and the variant lookups (#32), and refuses the verbs that mean
# nothing for it.
#
# Reads use the example Store Releases bundled in inst/releases (helper-local.R) and skip without a
# Python that has ogquery. Expected values are literals: Analyses from each release's analyses.tsv
# and manifest.json, read by eye; variants from ogquery's contract answers for the same releases
# (tests/contract/expected/{dense_ranges,ragged_completed,hybrid_observed}.json, read by eye). The
# refusals need no Python: a Local Source object is enough to dispatch on.

# One Local Source over three GWAS Stores, opened once and shared: the tests only read it.
three <- local({
  src <- NULL
  function() {
    if (is.null(src)) src <<- og_local(example_releases("dense", "ragged", "hyb"))
    src
  }
})

# A Local Source object for the refusals, which never read it.
unopened <- function() {
  structure(list(releases = data.frame(store_id = "s", release_id = "r", format_version = "0.3.0", path = "/nowhere"),
                 handles = new.env()), class = "og_local_source")
}

# Analyses ---------------------------------------------------------------------------------------

test_that("og_analyses() on a Local Source returns the Analyses named, with their Store Release", {
  skip_if_no_local()
  x <- og_analyses(three(), c("hyb:trait_b", "s:bmi_irn", "residual-rag:a"))
  expect_equal(x$requested_id, c("hyb:trait_b", "s:bmi_irn", "residual-rag:a"))
  expect_equal(x$id, c("hyb:trait_b", "s:bmi_irn", "residual-rag:a"))
  expect_equal(x$store_id, c("hyb", "s", "residual-rag"))
  expect_equal(x$analysis_id, c("trait_b", "bmi_irn", "a"))
  expect_equal(x$release_id, c("v1", "r", "comp"))
  expect_equal(x$analysis_label, c("Trait B", "BMI_IRN", NA))
  expect_identical(x$sample_size, c(1000L, 500000L, 50000L))
  expect_identical(x$stored_effect_scale, c("sd", "sd", "sd"))
  expect_identical(x$trait_chr, c(NA, NA, "1"))
  expect_identical(x$trait_bp, c(NA, NA, 1020000L))
  expect_identical(x$original_sd, rep(NA_real_, 3))
  expect_identical(x$original_sd_method, rep("declared_standardised", 3))
  expect_identical(x$completed_against, c(NA, NA, "EUR"))
  expect_identical(x$n_hits_5e8, c(4L, 8L, 60L))
  # No sample_size_kind in these releases, so no outcome_type (policy.py: "None ... rather than a guess").
  expect_identical(x$outcome_type, rep(NA_character_, 3))
})

test_that("a local Analysis row has the API Source's columns in its order, then the release's own", {
  skip_if_no_local()
  x <- og_analyses(three(), "s:bmi_irn")
  expect_equal(names(x), c(
    "requested_id", "id", "store_id", "analysis_id",
    # The API Source's, in its order (fixtures/shapes.json `analysis_row`), where analyses.tsv has them.
    "analysis_label", "trait_ontology_id", "trait_ontology_label", "tissue", "context", "trait_chr",
    "trait_bp", "assigned_ancestry", "ancestry_assignment_method", "sample_size_kind",
    "sample_size_scope", "sample_size", "n_cases", "n_controls", "stored_effect_scale",
    "original_effect_scale", "original_sd", "original_sd_method", "license", "publication_doi",
    "publication_pmid", "consortium", "first_author", "outcome_type", "release_id",
    # analyses.tsv's columns the API Source doesn't return, in the file's order.
    "eaf_scope", "original_sd_dispersion", "completed_against", "completion_median_pearson_r",
    "completion_n_imputed_total", "completion_n_missing_total", "n_missing_se_out_of_range",
    "eaf_orientation", "eaf_orientation_r", "eaf_orientation_n", "n_hits_5e8", "n_hits_5e6",
    "n_hits_5e4"))
})

test_that("og_analyses(ids = NULL) on a Local Source lists every Analysis of its Store Releases", {
  skip_if_no_local()
  x <- og_analyses(three())
  expect_equal(x$id, c("s:bmi_irn", "residual-rag:a", "residual-rag:b", "hyb:trait_a", "hyb:trait_b"))
  expect_equal(x$requested_id, x$id)
  expect_equal(x$release_id, c("r", "comp", "comp", "v1", "v1"))
})

test_that("a bare analysis id names the one Analysis it names in the Local Source", {
  skip_if_no_local()
  x <- og_analyses(three(), c("bmi_irn", "hyb:trait_a"))
  expect_equal(x$requested_id, c("bmi_irn", "hyb:trait_a"))
  expect_equal(x$id, c("s:bmi_irn", "hyb:trait_a"))
})

test_that("an Analysis a Local Source doesn't hold gets the API Source's class", {
  skip_if_no_local()
  e <- expect_error(og_analyses(three(), c("s:bmi_irn", "s:absent", "OGS-00011:GCST003116")),
                    class = "opengwasr_analysis_not_found")
  expect_equal(e$ids, c("s:absent", "OGS-00011:GCST003116"))
  expect_equal(e$api_error, "analysis_not_found")
})

test_that("a Local Source refuses a Legacy OpenGWAS id and says to translate it on the API Source", {
  skip_if_no_local()
  e <- expect_error(og_analyses(three(), c("s:bmi_irn", "ieu-a-2", "ukb-b-19953")),
                    class = "opengwasr_legacy_id_local")
  expect_s3_class(e, "opengwasr_refusal")
  expect_equal(e$ids, c("ieu-a-2", "ukb-b-19953"))
})

test_that("`releases` on a Local Source takes a GWAS Store id or a pin it holds", {
  skip_if_no_local()
  src <- three()
  expect_equal(og_analyses(src, releases = "s")$id, "s:bmi_irn")
  expect_equal(og_analyses(src, releases = c("residual-rag/comp", "s"))$id,
               c("residual-rag:a", "residual-rag:b", "s:bmi_irn"))
  expect_equal(og_analyses(src, "hyb:trait_a", releases = "hyb/v1")$release_id, "v1")
  # An Analysis outside the Store Releases asked for names nothing there.
  expect_error(og_analyses(src, "hyb:trait_a", releases = "s"), class = "opengwasr_analysis_not_found")
})

test_that("a Store Release a Local Source doesn't hold is refused with the API Source's class", {
  skip_if_no_local()
  src <- three()
  e <- expect_error(og_analyses(src, releases = "OGS-00011"), class = "opengwasr_release_not_found")
  expect_equal(e$api_error, "release_not_found")
  expect_error(og_analyses(src, "s:bmi_irn", releases = "s/other"), class = "opengwasr_release_not_found")
  # hyb-c is a Store Release of GWAS Store hyb, which this Source holds at v1.
  expect_error(og_analyses(src, releases = "hyb/v1-completed"), class = "opengwasr_release_not_found")
  expect_error(og_variants_rsid(src, "rs5000", releases = "s/other"), class = "opengwasr_release_not_found")
  expect_error(og_variants_chrpos(src, "1:5000", releases = "OGS-00011"), class = "opengwasr_release_not_found")
})

# outcome_type: opengwasdb-stores policy.py outcome_type(), its docstring's cases ------------------

test_that("outcome_type follows the catalogue's rule", {
  # "Binary when sample_size_kind is case_control or stored_effect_scale is log_or": "an sd effect
  # with case counts is a binary trait stored in SD units".
  expect_identical(analysis_outcome_type("sd", "case_control"), "binary")
  expect_identical(analysis_outcome_type("log_or", "total"), "binary")
  expect_identical(analysis_outcome_type("log_or", NA), "binary")
  # "Continuous for sd with any other sample size kind: a total sample size or a variant_level one".
  expect_identical(analysis_outcome_type("sd", "total"), "continuous")
  expect_identical(analysis_outcome_type("sd", "variant_level"), "continuous")
  expect_identical(analysis_outcome_type("sd", "effective"), "continuous")
  # "None for every other pair, including a blank, rather than a guess."
  expect_identical(analysis_outcome_type("sd", NA), NA_character_)
  expect_identical(analysis_outcome_type(NA, "total"), NA_character_)
  expect_identical(analysis_outcome_type("log_hazard", "total"), NA_character_)
  expect_identical(analysis_outcome_type(c("sd", "log_or", NA), c("total", NA, NA)),
                   c("continuous", "binary", NA))
})

# Variant lookups ---------------------------------------------------------------------------------

test_that("og_variants_rsid() on a Local Source returns the API Source's rows", {
  skip_if_no_local()
  x <- og_variants_rsid(three(), c("rs5000", "RS1100", "rs99999999"))
  # The API's fields, in the order it sends them (fake-server.R fake_variants()).
  expect_equal(names(x), c("alid", "chromosome", "effect_allele", "other_allele", "position", "release_id",
                           "rsid", "store_id"))
  expect_equal(x$alid, c("1:5000:A:C", "1:5000:C:G", "1:1096000:A:G", "1:101000:A:AACGTACGTACGT"))
  expect_equal(x$chromosome, c("1", "1", "1", "1"))
  expect_identical(x$position, c(5000L, 5000L, 1096000L, 101000L))
  expect_equal(x$effect_allele, c("A", "C", "A", "A"))
  expect_equal(x$other_allele, c("C", "G", "G", "AACGTACGTACGT"))
  expect_equal(x$rsid, c("rs5000", "rs5000", "rs1100", "rs1100"))
  expect_equal(x$store_id, c("s", "s", "residual-rag", "hyb"))
  expect_equal(x$release_id, c("r", "r", "comp", "v1"))

  prov <- og_provenance(x)
  expect_equal(prov$store_releases, c("s/r", "residual-rag/comp", "hyb/v1"))
  expect_equal(prov$total, 4)
  expect_false(prov$truncated)
})

test_that("an rsID is matched whole, as the API matches it", {
  skip_if_no_local()
  # The third variant of each digest release names `rs77;rs78`, one name, whole (contract README).
  x <- og_variants_rsid(three(), c("rs77;rs78", "rs77"), releases = "residual-rag")
  expect_equal(x$alid, "1:1001000:A:G")
  expect_equal(x$rsid, "rs77;rs78")
})

test_that("og_variants_chrpos() on a Local Source returns the API Source's rows", {
  skip_if_no_local()
  x <- og_variants_chrpos(three(), c("chr1:1000", "2:300"))
  expect_equal(names(x), c("alid", "chromosome", "effect_allele", "other_allele", "position", "release_id",
                           "rsid", "store_id"))
  expect_equal(x$alid, c("1:1000:A:G", "2:300:C:T", "1:1000:A:G"))
  expect_identical(x$position, c(1000L, 300L, 1000L))
  expect_identical(x$rsid, c("rs1000", "rs1005", NA))
  expect_equal(x$store_id, c("s", "s", "hyb"))
  expect_equal(x$release_id, c("r", "r", "v1"))

  # A position with several variants gives each: rs5000's two, and one with no rsID.
  y <- og_variants_chrpos(three(), "1:5000", releases = "s")
  expect_equal(nrow(y), 3)
  expect_equal(y$alid[1:2], c("1:5000:A:C", "1:5000:C:G"))
  expect_identical(y$rsid, c("rs5000", "rs5000", NA))

  # A position nothing holds, or that no chromosome spells, has no row.
  z <- og_variants_chrpos(three(), c("1:4999", "27:5000"), releases = "s")
  expect_equal(nrow(z), 0)
  expect_equal(names(z), names(empty_lookup()))
})

test_that("rsIDs resolve on a Local Source as on the API, the ones held by none unresolved", {
  skip_if_no_local()
  src <- three()
  releases <- resolve_releases(src, NULL)
  resolved <- resolve_variants(src, c("rs5000", "1:1000:A:G", "rs99999999"), releases)
  expect_equal(resolved$alids, c("1:1000:A:G", "1:5000:A:C", "1:5000:C:G"))
  expect_equal(resolved$unresolved, "rs99999999")
  expect_warning(warn_unresolved(resolved$unresolved), class = "opengwasr_unresolved_variants")
})

test_that("an endpoint the local Job doesn't answer is an internal error naming it", {
  e <- expect_error(local_request(unopened(), "api/nothing", list()), class = "opengwasr_internal")
  expect_equal(e$path, "api/nothing")
})

# Refusals: no Python needed --------------------------------------------------------------------

test_that("og_status() and og_user() refuse a Local Source", {
  for (f in list(og_status, og_user)) {
    e <- expect_error(f(unopened()), class = "opengwasr_local_unsupported")
    expect_s3_class(e, "opengwasr_refusal")
  }
})

test_that("og_submit() and the og_job_* functions refuse a Local Source", {
  src <- unopened()
  expect_error(og_submit(src, og_variants_rsid, "rs5000"), class = "opengwasr_local_unsupported")
  expect_error(og_job_status(src, "abc"), class = "opengwasr_local_unsupported")
  expect_error(og_job_result(src, "abc", raw = TRUE), class = "opengwasr_local_unsupported")
  expect_error(og_job_cancel(src, "abc"), class = "opengwasr_local_unsupported")
})

test_that("LD operations refuse a Local Source as `src`", {
  src <- unopened()
  for (call in list(quote(og_ld_stores(src)), quote(og_ld_matrix(src, "1:1000:A:G")),
                    quote(og_ld_proxies(src, "rs5000")), quote(og_ld_reflookup(src, "rs5000")))) {
    e <- expect_error(eval(call), class = "opengwasr_local_unsupported")
    expect_s3_class(e, "opengwasr_refusal")
  }
})
