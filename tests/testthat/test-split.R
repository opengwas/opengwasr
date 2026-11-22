# Splitting: a request over one Job's shape limits becomes several Jobs, run a few at a time, and
# put back together (R/split.R, R/query-plan.R). The splitter's arithmetic is tested on its own;
# the rest runs through the public calls against the fake, whose Job routes enforce the contract's
# limits (100 Analyses, 1,000 variants, 10 Store Releases) unless a test lifts them for the
# reference run.
fake <- local_fake_api(n_synthetic = 120)
withr::local_options(opengwasr.poll_cap = 0.05, opengwasr.progress = FALSE)

shared <- "3:1000:A:C"
syn <- sprintf("OGS-00006:SYN%05d", 1:101)
syn_rel <- "OGS-00006/OGS-00006-completed"
many <- sprintf("OGS-%05d:MANY%02d", 2000 + 1:11, 1:11)
many_rel <- sprintf("OGS-%05d/OGS-%05d-r", 2000 + 1:11, 2000 + 1:11)

bare <- function(x) {
  attr(x, "og_provenance") <- NULL
  x
}
lifted <- function(expr) {   # no limits, on the fake or in opengwasr: the unsplit reference
  raw_api_post(fake$url(), "_fake/lift_limits/true")
  on.exit(raw_api_post(fake$url(), "_fake/lift_limits/false"))
  withr::local_options(opengwasr.job_limits = list(releases = 1e9, variants = 1e9, analyses = 1e9, rsids = 1e9))
  force(expr)
}
events <- function() raw_api(fake$url(), fake_key, "_fake/events")
count <- function(what, from = 0) sum(vapply(events()[-seq_len(from)], function(e) e$event == what, NA))
# The most Jobs the API held submitted and not yet completed at once, after event number `from`.
peak_in_flight <- function(from = 0) {
  open <- character()
  peak <- 0
  for (e in events()[-seq_len(from)]) {
    if (e$event == "submit") open <- union(open, e$job_id)
    if (e$event == "done") open <- setdiff(open, e$job_id)
    peak <- max(peak, length(open))
  }
  peak
}

# Variants over 1,000 split the same way, by the same code at the real limit (test-splitter.R checks
# the arithmetic at 1,000/1,001; the all-dimension test below runs it end to end at lowered limits).
test_api("Analyses over a Job's 100 are split, 2 Jobs", {
  alids <- c(shared, "3:2001:A:G", "3:2101:A:G")
  got <- og_associations(src, alids, syn, releases = syn_rel)
  expect_equal(nrow(got), 103)
  # Analyses as asked, then position.
  expect_equal(got$alid[1:2], c(shared, "3:2001:A:G"))
  expect_equal(unique(got$id), syn)
  expect_equal(got$alid[got$id == syn[[101]]], c(shared, "3:2101:A:G"))
  expect_equal(sum(got$alid == shared), 101)
  prov <- og_provenance(got)
  expect_length(prov$job_ids, 2)
  expect_equal(prov$total, nrow(got))
  expect_false(prov$truncated)
  # (The unsplit reference of the same rows is the all-dimension test's, at lowered limits.)
}, live = FALSE)

test_api("Analyses from more than 10 GWAS Stores are split by GWAS Store", {
  fake_set(fake, "many_stores", "true", "false")
  got <- og_associations(src, c(shared, "4:101:A:G", "4:111:A:G"), many, releases = many_rel)
  expect_equal(nrow(got), 13)
  expect_equal(unique(got$id), many)
  prov <- og_provenance(got)
  expect_length(prov$job_ids, 2)
  expect_equal(prov$total, 13)
  # Each Job holds only the Store Releases of its own Analyses, at most 10.
  sent <- Filter(function(b) identical(b$endpoint, "/api/associations"), raw_api(fake$url(), fake_key, "_fake/bodies"))
  last <- utils::tail(sent, 2)
  expect_equal(vapply(last, function(b) length(b$body$releases), 1L), c(10L, 1L))
}, live = FALSE)

test_that("top hits split so that `limit` x Analyses stays within a Job's 100,000 rows", {
  src <- fake_source(fake)
  releases <- "OGS-00006/OGS-00006-completed"
  # 100,000 %/% 10,000 = 10 Analyses a Job: 11 Analyses are 2 Jobs. (The boundary arithmetic is in test-splitter.R.)
  big <- og_tophits(src, syn[1:11], clump = FALSE, limit = 10000, releases = releases)
  expect_length(og_provenance(big)$job_ids, 2)
})

test_api("rsIDs over a Job's limit are looked up in several Jobs, and every Job is in provenance", {
  cad <- "OGS-00011:GCST003116"
  rel <- "OGS-00011/OGS-00011-completed"
  rsids <- c("rs1421085", "rs34159589", "rs999999999")
  reference <- suppressWarnings(og_associations(src, rsids, cad, releases = rel), classes = "opengwasr_unresolved_variants")
  withr::local_options(opengwasr.job_limits = list(rsids = 1))
  w <- expect_warning(got <- og_associations(src, rsids, cad, releases = rel), class = "opengwasr_unresolved_variants")
  expect_equal(w$ids, "rs999999999")
  expect_equal(bare(got), bare(reference))
  # Three lookups of one rsID each, and the Associations Job.
  expect_length(og_provenance(got)$job_ids, 4)
  expect_equal(og_provenance(got)$total, nrow(got))
  # Variants at exactly a Job's limit, where an rsID names ALIDs already among them, are one Job (after
  # the lookup's own): the limit is on the resolved ALIDs.
  withr::local_options(opengwasr.job_limits = list(variants = 2))
  one <- og_associations(src, c("16:53767042:A:G", "16:53767042:C:T", "rs1421085"), cad, releases = rel)
  expect_length(og_provenance(one)$job_ids, 2)
})

test_api("top hits split by GWAS Store keep `limit` per Analysis, warn once, and add up", {
  fake_set(fake, "many_stores", "true", "false")
  w <- expect_warning(
    got <- og_tophits(src, many, clump = FALSE, limit = 1, releases = many_rel),
    class = "opengwasr_truncated"
  )
  expect_equal(w$n_analyses, 11L)
  expect_equal(nrow(got), 11)
  expect_equal(got$id, many)
  prov <- og_provenance(got)
  expect_length(prov$job_ids, 2)
  expect_equal(prov$total, 11)
  expect_true(prov$truncated)
  ref <- suppressWarnings(lifted(og_tophits(src, many, clump = FALSE, limit = 1, releases = many_rel)),
                          classes = "opengwasr_truncated")
  expect_equal(bare(got), bare(ref))
}, live = FALSE)

test_that("every dimension splits, at most `max_in_flight` Jobs are in flight, and the rows are the unsplit rows", {
  fake_set(fake, "many_stores", "true", "false")
  clock <- local_virtual_clock()
  variants <- c(shared, "4:101:A:G", "4:102:A:G", "4:103:A:G")
  ids <- many[1:6]
  reference <- lifted(og_associations(fake_source(fake), variants, ids, releases = many_rel))
  # 6 Analyses in Jobs of 2 (3 chunks) x 4 variants in Jobs of 2 (2 chunks) = 6 Jobs.
  withr::local_options(opengwasr.job_limits = list(analyses = 2, variants = 2))
  src <- fake_source(fake, max_in_flight = 2)
  from <- length(events())
  got <- og_associations(src, variants, ids, releases = many_rel)
  expect_equal(bare(got), bare(reference))
  expect_length(og_provenance(got)$job_ids, 6)
  expect_equal(og_provenance(got)$total, nrow(got))
  expect_equal(count("submit", from), 6)
  expect_equal(peak_in_flight(from), 2)
})

test_that("og_submit() of a split plan returns one handle whose result equals the synchronous call", {
  fake_set(fake, "many_stores", "true", "false")
  src <- fake_source(fake)
  clock <- local_virtual_clock()
  withr::local_options(opengwasr.job_limits = list(analyses = 4))
  handle <- og_submit(src, og_associations, c(shared, "4:101:A:G"), many, releases = many_rel)
  expect_s3_class(handle, "og_jobs")
  expect_length(handle$job_ids, 3)
  expect_length(clock$waits, 0)
  got <- og_job_result(src, handle)
  direct <- og_associations(src, c(shared, "4:101:A:G"), many, releases = many_rel)
  expect_equal(bare(got), bare(direct))
  expect_equal(og_provenance(got)$job_ids, handle$job_ids)
  expect_equal(og_provenance(got)$total, nrow(got))
})

test_that("og_submit() refuses a plan over the per-user queue before submitting anything", {
  src <- fake_source(fake)
  withr::local_options(opengwasr.job_limits = list(variants = 10))
  alids <- sprintf("1:%d:A:C", 1:1010)   # 101 Jobs of at most 10 variants
  from <- length(events())
  e <- expect_error(og_submit(src, og_associations, alids, "OGS-00011:GCST003116",
                              releases = "OGS-00011/OGS-00011-completed"),
                    class = "opengwasr_too_many_jobs")
  expect_equal(e$n_jobs, 101L)
  expect_s3_class(e, "opengwasr_refusal")
  expect_equal(count("submit", from), 0)
})

test_api("a modest split, live: three GWAS Stores' top hits in three Jobs equal the unsplit call", {
  skip_if(backend == "fake")   # the fake is covered by the top-hits test above
  ids <- c("OGS-00011:GCST003116", "OGS-00009:ukb-b-19953", "OGS-00027:ENSG00000000419")
  pins <- c("OGS-00011/OGS-00011-completed", "OGS-00009/OGS-00009-completed", "OGS-00027/OGS-00027")
  quietly <- function(x) suppressWarnings(x, classes = "opengwasr_truncated")
  reference <- quietly(og_tophits(src, ids, clump = FALSE, limit = 2, releases = pins))
  withr::local_options(opengwasr.job_limits = list(releases = 1))
  got <- quietly(og_tophits(src, ids, clump = FALSE, limit = 2, releases = pins))
  expect_equal(bare(got), bare(reference))
  prov <- og_provenance(got)
  expect_length(prov$job_ids, 3)
  expect_length(og_provenance(reference)$job_ids, 1)
  expect_equal(prov$total, nrow(got))
  expect_equal(prov$total, og_provenance(reference)$total)
  expect_setequal(prov$store_releases, pins)
})
