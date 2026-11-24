# PheWAS, ranges and variant lookups: thin specs over the Association-query engine
# (R/association-query.R) and the lookup helpers (R/variants.R). Through the public calls, against
# the fake (live too, small, where credentials are present).
association_columns <- getFromNamespace("association_columns", "opengwasr")
fake <- local_fake_api(n_synthetic = 11)
withr::local_options(opengwasr.poll_cap = 0.05, opengwasr.progress = FALSE)

cad <- "OGS-00011:GCST003116"
bmi <- "OGS-00011:GCST90029007"
rel <- "OGS-00011/OGS-00011-completed"
snv <- "16:53767042:C:T"
other_snv <- "16:53767042:A:G"
syn <- sprintf("OGS-00006:SYN%05d", 1:11)
syn_rel <- "OGS-00006/OGS-00006-completed"

fake_bodies <- function(endpoint) {
  Filter(function(b) identical(b$endpoint, endpoint), raw_api(fake$url(), fake_key, "_fake/bodies"))
}
bare <- function(x) {
  attr(x, "og_provenance") <- NULL
  x
}
lifted <- function(expr) {   # no limits, on the fake or in opengwasr: the unsplit reference
  raw_api_post(fake$url(), "_fake/lift_limits/true")
  on.exit(raw_api_post(fake$url(), "_fake/lift_limits/false"))
  withr::local_options(opengwasr.job_limits = list(releases = 1e9, variants = 1e9, analyses = 1e9, rsids = 1e9,
                                                   positions = 1e9, range_analyses = 1e9, range_window = 1e9))
  force(expr)
}

# ---- PheWAS ----

test_api("og_phewas() of an ALID: columns match og_associations(), threshold and observed_only apply", {
  both <- c(cad, bmi)
  ph <- og_phewas(src, snv, threshold = 1, analyses = both, releases = rel)
  expect_equal(names(ph)[seq_along(association_columns)], association_columns)
  expect_false("log10_p" %in% names(ph))
  expect_equal(nrow(ph), 2)
  expect_setequal(ph$id, both)
  expect_equal(unique(ph$alid), snv)
  expect_equal(unique(ph$effect_scale[ph$id == bmi]), "sd")
  expect_equal(og_provenance(ph)$truncated, FALSE)
  # observed_only leaves out the Imputed ones (on the fake, the insertion is Imputed in both Analyses).
  insertion <- "5:75701728:A:AAAAT"
  observed <- og_phewas(src, insertion, threshold = 1, analyses = both, observed_only = TRUE, releases = rel)
  expect_observed_subset(observed, og_phewas(src, insertion, threshold = 1, analyses = both, releases = rel))
  if (backend == "fake") expect_equal(nrow(observed), 0)
})

test_api("og_phewas() with `analyses = NULL` searches every Analysis of the Store Releases", {
  all <- og_phewas(src, snv, releases = rel)
  expect_true(bmi %in% all$id)
  expect_true(all(all$pval <= 1e-8))
  expect_false(anyNA(all$effect_scale))   # metadata came with the Analyses in the result
  expect_false(anyNA(all$n))
  if (backend == "fake") {
    expect_equal(all$id, bmi)
    expect_equal(unlist(tail(raw_api(fake$url(), fake_key, "_fake/gwasinfo"), 1)[[1]]$id), bmi)  # only that one
    sent <- tail(fake_bodies("/api/phewas"), 1)[[1]]$body
    expect_null(sent$analyses)
    expect_identical(sent$observed_only, FALSE)
  }
})

test_api("og_phewas() of a multiallelic rsID runs one PheWAS per ALID and combines them", {
  both <- c(cad, bmi)
  ph <- og_phewas(src, "rs1421085", threshold = 1, analyses = both, releases = rel)
  # rs1421085 names two ALIDs; (on the fake) only BMI holds the second.
  expect_true(all(ph$alid %in% c(snv, other_snv)))
  expect_true(snv %in% ph$alid)
  if (backend == "fake") {
    expect_equal(nrow(ph), 3)
    expect_equal(sort(ph$alid), sort(c(snv, snv, other_snv)))
    expect_equal(ph$id[ph$alid == other_snv], bmi)
  }
  # One ALID's rows are together, in the order the ALIDs were resolved.
  expect_equal(ph$alid, ph$alid[order(match(ph$alid, unique(ph$alid)))])
  if (backend == "fake") {
    expect_equal(length(og_provenance(ph)$job_ids), 3)   # the rsID lookup and one Job per ALID
    sent <- fake_bodies("/api/phewas")
    expect_setequal(vapply(tail(sent, 2), function(b) b$body$variant, ""), c(other_snv, snv))
    # With no `analyses`, the same search, every Analysis.
    every <- og_phewas(src, "rs1421085", threshold = 1, releases = rel)
    expect_equal(sort(paste(every$id, every$alid)), sort(paste(ph$id, ph$alid)))
  }
})

test_that("og_phewas() of several variants runs one PheWAS per ALID, combined in the order given", {
  src <- fake_source(fake)
  both <- c(cad, bmi)
  insertion <- "5:75701728:A:AAAAT"
  expect_warning(
    ph <- og_phewas(src, c(insertion, "rs1421085", "rs0000001"), threshold = 1, analyses = both, releases = rel),
    class = "opengwasr_unresolved_variants")
  # The ALID first, then the rsID's two ALIDs in the order it resolved; one ALID's rows together.
  expect_equal(unique(ph$alid)[1], insertion)
  expect_setequal(ph$alid, c(insertion, snv, other_snv))
  expect_equal(ph$alid, ph$alid[order(match(ph$alid, unique(ph$alid)))])
  expect_equal(nrow(ph), 5)    # 2 for the insertion, 1 (BMI only) for A:G, 2 for C:T
  # Jobs: the rsID lookup, then one PheWAS per ALID; every one is in provenance.
  expect_equal(length(og_provenance(ph)$job_ids), 4)
  back <- suppressWarnings(og_phewas(src, c("rs1421085", insertion), threshold = 1, analyses = both, releases = rel))
  expect_equal(unique(back$alid)[3], insertion)
  expect_setequal(paste(back$id, back$alid), paste(ph$id, ph$alid))
  # The same variant twice is one PheWAS.
  expect_equal(nrow(og_phewas(src, c(snv, snv), threshold = 1, analyses = both, releases = rel)), 2)
})

test_api("og_phewas() of an ALID and an rsID, combined in the order given [small]", {
  both <- c(cad, bmi)
  ph <- og_phewas(src, c(snv, "rs6567160"), threshold = 1, analyses = both, releases = rel)
  expect_equal(unique(ph$alid)[1], snv)
  expect_true("18:60161902:C:T" %in% ph$alid)
  expect_equal(ph$alid, ph$alid[order(match(ph$alid, unique(ph$alid)))])
})

test_api("og_phewas() warns when `limit` cuts the PheWAS off", {
  expect_warning(ph <- og_phewas(src, snv, threshold = 1, releases = rel, limit = 1), class = "opengwasr_truncated")
  expect_equal(nrow(ph), 1)
  prov <- og_provenance(ph)
  expect_true(prov$truncated)
  expect_gte(prov$total, 2)
})

test_api("og_phewas() checks its own arguments", {
  expect_error(og_phewas(src, character()), class = "opengwasr_argument")
  expect_error(og_phewas(src, snv, limit = 1.5), class = "opengwasr_argument")
  expect_error(og_phewas(src, snv, threshold = "1"), class = "opengwasr_argument")
  expect_error(og_phewas(src, "not-a-variant", releases = rel), class = "opengwasr_argument")
  expect_warning(none <- og_phewas(src, "rs0000001", releases = rel), class = "opengwasr_unresolved_variants")
  expect_equal(nrow(none), 0)
  expect_equal(names(none)[1:5], c("id", "store_id", "analysis_id", "analysis_label", "alid"))
})

# ---- Ranges ----

test_api("og_range() returns the window's Associations, columns as og_associations()", {
  rg <- og_range(src, cad, chromosome = "9", start = 22090000, end = 22130000, releases = rel)
  expect_s3_class(rg, "data.frame")
  expect_equal(names(rg)[seq_along(association_columns)], association_columns)
  expect_true(all(rg$chromosome == "9" & rg$position >= 22090000 & rg$position <= 22130000))
  expect_equal(rg$position, sort(rg$position))
  expect_gt(nrow(rg), 0)
  observed <- og_range(src, cad, "9", 22090000, 22130000, observed_only = TRUE, releases = rel)
  expect_observed_subset(observed, rg)
  if (backend == "fake") expect_equal(nrow(rg), 5)
  if (backend == "fake") expect_lt(nrow(observed), nrow(rg))
  if (backend == "fake") {
    sent <- tail(fake_bodies("/api/associations/range"), 3)[[1]]$body
    expect_identical(sent$start, 22090000L)
    expect_identical(sent$observed_only, FALSE)
  }
})

test_that("a value the API checks itself is the API's error", {
  src <- fake_source(fake)
  expect_error(og_range(src, cad, "9", 0, 10, releases = rel), class = "opengwasr_invalid_request")
  expect_error(og_phewas(src, snv, limit = 0, releases = rel), class = "opengwasr_invalid_request")
})

test_that("an all-Analyses PheWAS looks metadata up for every Analysis in the result", {
  src <- fake_source(fake)
  # 3:1000:A:C is held by all 11 synthetic Analyses (p <= 1e-9 in each).
  ph <- og_phewas(src, "3:1000:A:C", releases = syn_rel)
  expect_setequal(ph$id, syn)
  expect_equal(length(unique(ph$id)), 11)
  expect_false(anyNA(ph$effect_scale))
  expect_false(anyNA(ph$n))
  asked <- raw_api(fake$url(), fake_key, "_fake/gwasinfo")
  expect_setequal(unlist(asked[[length(asked)]]$id), syn)
  # original_units: refused after the rows, naming the result's Analyses (none has an original SD).
  err <- expect_error(og_phewas(src, "3:1000:A:C", releases = syn_rel, original_units = TRUE),
                      class = "opengwasr_original_units_unavailable")
  expect_setequal(err$ids, syn)
  # Named Analyses are refused before any Job runs.
  before <- length(fake_bodies("/api/phewas"))
  expect_error(og_phewas(src, "3:1000:A:C", analyses = syn[1:2], releases = syn_rel, original_units = TRUE),
               class = "opengwasr_original_units_unavailable")
  expect_length(fake_bodies("/api/phewas"), before)
})

test_that("provenance leaves out long arguments, counts neutrally, and the warning speaks of `limit`", {
  src <- fake_source(fake)
  v <- og_variants_rsid(src, "rs1421085", releases = "OGS-00011")
  expect_false(grepl("rs1421085", og_provenance(v)$query))
  p <- og_variants_chrpos(src, "16:53767042", releases = "OGS-00011")
  expect_false(grepl("53767042", og_provenance(p)$query))
  expect_warning(ph <- og_phewas(src, snv, threshold = 1, analyses = c(cad, bmi), releases = rel, limit = 1),
                 "ALID")
  text <- paste(format(og_provenance(ph)), collapse = "\n")
  expect_false(grepl("Top hits", text))
  expect_match(text, "1 returned of 2 matching \\(truncated\\)")
  w <- tryCatch(og_phewas(src, snv, threshold = 1, analyses = c(cad, bmi), releases = rel, limit = 1),
                warning = function(w) conditionMessage(w))
  expect_false(grepl("Job", w))
})

test_that("a lookup's own total and truncated are checked, not overwritten", {
  src <- fake_source(fake)
  fake_set(fake, "job", "total_too_big", "normal")
  expect_error(og_variants_rsid(src, "rs1421085", releases = "OGS-00011"), class = "opengwasr_job_invalid")
})

test_api("og_range() checks its own arguments", {
  expect_error(og_range(src, cad, "9", 20, 10), class = "opengwasr_argument")
  expect_error(og_range(src, cad, c("9", "10"), 1, 10), class = "opengwasr_argument")
  expect_error(og_range(src, cad, "9", "1", 10), class = "opengwasr_argument")
  expect_error(og_range(src, character(), "9", 1, 10), class = "opengwasr_argument")
})

test_that("og_range() over more than 10 Mb and 10 Analyses equals the windows fetched unsplit", {
  src <- fake_source(fake)
  ids <- rev(syn)
  before <- length(fake_bodies("/api/associations/range"))
  split <- og_range(src, ids, "3", 1, 30000000, releases = syn_rel)
  sent <- fake_bodies("/api/associations/range")[-seq_len(before)]
  # Three windows (1-10 Mb, 10-20 Mb, 20-30 Mb) times two groups of Analyses (10, then 1).
  expect_equal(length(og_provenance(split)$job_ids), 6)
  # Every Analysis holds 3:1000 and 3:2000+i, and 3:12e6+i and 3:25e6+i: four rows each.
  expect_equal(nrow(split), 11 * 4)
  expect_equal(split$id, rep(ids, each = 4))   # Analyses in the order asked, not sorted
  expect_equal(split$position[1:4], c(1000, 2011, 12000011, 25000011))
  whole <- lifted(og_range(src, ids, "3", 1, 30000000, releases = syn_rel))
  expect_equal(length(og_provenance(whole)$job_ids), 1)
  expect_equal(bare(split), bare(whole))
  # Windows are at most 10 Mb and inclusive; the last is cut short.
  windows <- lapply(sent, function(b) c(b$body$start, b$body$end))
  expect_equal(unique(windows), list(c(1, 1e7), c(1e7 + 1, 2e7), c(2e7 + 1, 3e7)))
})

test_that("og_range() is not split round a refusal", {
  src <- fake_source(fake)
  withr::local_options(opengwasr.job_limits = list(range_window = 1e9))
  expect_error(og_range(src, syn[1], "3", 1, 30000000, releases = syn_rel), class = "opengwasr_request_too_large")
})

# ---- Variant lookups ----

test_api("og_variants_rsid() returns every ALID an rsID names, one row each", {
  # (matched in lower case; the unknown rsID has no row)
  v <- og_variants_rsid(src, c("rs34159589", "RS1421085", "rs6567160", "rs0000001"), releases = "OGS-00011")
  expect_setequal(names(v), c("alid", "chromosome", "position", "effect_allele", "other_allele", "rsid",
                              "store_id", "release_id"))
  expect_true(all(c(other_snv, snv, "18:60161902:C:T") %in% v$alid))
  expect_true(all(c("rs1421085", "rs6567160") %in% v$rsid))
  expect_false("rs0000001" %in% v$rsid)
  if (backend == "fake") {
    expect_setequal(v$alid, c("5:75701728:A:AAAAT", "5:75701728:AAAAT:AAAATAAAT", other_snv, snv, "18:60161902:C:T"))
    expect_equal(nrow(v), 5)
    expect_setequal(v$rsid, c("rs34159589", "rs1421085", "rs6567160"))
  }
  expect_s3_class(og_provenance(v), "og_provenance")
})

test_api("og_variants_chrpos() returns every ALID at a position", {
  v <- og_variants_chrpos(src, c("16:53767042", "chr5:75701728", "1:1"), releases = "OGS-00011")
  expect_true(all(c("5:75701728:A:AAAAT", other_snv, snv) %in% v$alid))
  expect_true(all(paste0(v$chromosome, ":", v$position) %in% c("16:53767042", "5:75701728")))
  if (backend == "fake") {
    expect_equal(nrow(v), 4)
    expect_setequal(v$alid, c("5:75701728:A:AAAAT", "5:75701728:AAAAT:AAAATAAAT", other_snv, snv))
  }
  expect_equal(unique(v$store_id), "OGS-00011")
  expect_error(og_variants_chrpos(src, 5), class = "opengwasr_argument")
  expect_error(og_variants_rsid(src, character()), class = "opengwasr_argument")
})

test_that("variant lookups split over the Job limits and put the rows back together", {
  src <- fake_source(fake)
  # By positions (the lookups share their splitting); the arithmetic itself is in test-splitter.R.
  withr::local_options(opengwasr.job_limits = list(positions = 1))
  p <- og_variants_chrpos(src, c("18:60161902", "16:53767042"), releases = "OGS-00011")
  expect_equal(length(og_provenance(p)$job_ids), 2)
  expect_equal(p$alid, c("18:60161902:C:T", other_snv, snv))
})

# ---- og_submit() ----

test_that("og_submit() works for og_phewas(), og_range() and the lookups", {
  src <- fake_source(fake)
  for (case in list(
    list(og_phewas, list(c(snv, other_snv), threshold = 1, releases = rel)),
    list(og_range, list(cad, "9", 22090000, 22130000, releases = rel)),
    list(og_variants_rsid, list("rs1421085", releases = "OGS-00011")),
    list(og_variants_chrpos, list("16:53767042", releases = "OGS-00011"))
  )) {
    job <- do.call(og_submit, c(list(src, case[[1]]), case[[2]]))
    expect_s3_class(job, "og_jobs")
    got <- og_job_result(src, job)
    expect_gt(nrow(got), 0)
    if (identical(case[[1]], og_phewas)) expect_equal(bare(got), bare(do.call(case[[1]], c(list(src), case[[2]]))))
  }
  # An every-Analysis PheWAS looks its metadata up after the rows, when collected.
  job <- og_submit(src, og_phewas, snv, releases = rel)
  expect_equal(og_job_result(src, job)$id, bmi)
})
