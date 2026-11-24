# PLINK References (#14): `ld = og_plink_reference(path)` for LD matrices, clumping and top hits.
# They skip without a PLINK binary. Expected values are worked out in plain R from the committed
# fixture's genotypes (helper-plink.R), not from PLINK.

fake <- local_fake_api(n_synthetic = 1)
withr::local_options(opengwasr.poll_cap = 0.05, opengwasr.progress = FALSE)
src <- fake_source(fake)
cad <- "OGS-00011:GCST003116"

# The fake's CAD top hits as a data frame of Associations (rsIDs of the fixture, one it lacks).
cad_hits <- function() og_tophits(src, cad, clump = FALSE)

test_that("a PLINK Reference checks its fileset and finds PLINK", {
  expect_error(og_plink_reference(file.path(tempdir(), "nothing")), class = "opengwasr_plink_reference_missing")
  expect_error(og_plink_reference(c("a", "b")), class = "opengwasr_argument")
  ref <- local_plink_reference()
  expect_s3_class(ref, "og_plink_reference")
  expect_equal(ref$path, plink_fixture())
  expect_true(file.exists(ref$plink_bin))
  expect_equal(format(ref)[[1]], "<og_plink_reference>")
  expect_error(og_plink_reference(plink_fixture(), plink_bin = "/no/such/plink"), class = "opengwasr_plink_not_found")
  # An explicit binary wins.
  expect_equal(og_plink_reference(plink_fixture(), plink_bin = ref$plink_bin)$plink_bin, ref$plink_bin)
})

test_that("with no PLINK anywhere, the error says how to get it", {
  found <- tryCatch(find_plink(NULL), error = function(e) NULL)
  skip_if(!is.null(found), "PLINK is available here.")
  e <- expect_error(og_plink_reference(plink_fixture()), class = "opengwasr_plink_not_found")
  expect_match(conditionMessage(e), "genetics.binaRies")
  expect_match(conditionMessage(e), "plink_bin")
})

test_that("choose_ld_store() takes a PLINK Reference as the user's choice, without asking the API", {
  ref <- local_plink_reference()
  before <- length(raw_api(fake$url(), fake_key, "_fake/bodies"))
  chosen <- choose_ld_store(src, ref)
  expect_equal(chosen$choice, "user")
  expect_equal(chosen$store$ld_store, ref$path)
  expect_identical(chosen$plink, ref)
})

test_that("og_ld_matrix() reads r from PLINK, named by rsID and the reference's alleles", {
  ref <- local_plink_reference()
  rsids <- c("rs1333049", "rs4977574", "rs2891168", "rs1421085")
  m <- og_ld_matrix(src, rsids, ld = ref)
  bim <- fixture_bim()
  names <- paste(rsids, bim$a1[match(rsids, bim$rsid)], bim$a2[match(rsids, bim$rsid)], sep = "_")
  expect_s3_class(m, "og_ld_matrix")
  expect_equal(dimnames(m), list(names, names))
  expected <- stats::cor(fixture_dosage()[, rsids])
  dimnames(expected) <- list(names, names)
  plain <- unclass(m)
  attr(plain, "og_provenance") <- NULL
  expect_equal(plain, expected, tolerance = 1e-5)
  expect_equal(unname(diag(plain)), rep(1, 4), tolerance = 1e-5)
  # The sign refers to the first allele: hand-checked, the two chromosome 9 variants in one block correlate positively.
  expect_gt(m["rs4977574_G_A", "rs2891168_G_A"], 0.5)
  prov <- og_provenance(m)
  expect_equal(prov$ld, ref$path)
  expect_equal(prov$ld_choice, "user")
  expect_equal(prov$absent, character())
  expect_equal(prov$plink_bin, ref$plink_bin)
  expect_match(prov$query, "ld = ", fixed = TRUE)
})

test_that("variants without a match in the reference are left out, with a message giving the count", {
  ref <- local_plink_reference()
  # rs10738610 isn't in the fixture; rs0 names nothing; the ALID has no variant-table rsID.
  e <- expect_message(
    m <- og_ld_matrix(src, c("rs4977574", "rs10738610", "rs0", "rs2891168"), ld = ref),
    class = "opengwasr_plink_unmatched")
  expect_equal(e$count, 2)
  expect_equal(e$total, 4)
  expect_match(conditionMessage(e), "2 of 4 variants matched the PLINK Reference by rsID; 2 not in it were left out.", fixed = TRUE)
  expect_equal(rownames(m), c("rs4977574_G_A", "rs2891168_G_A"))
  expect_equal(og_provenance(m)$absent, c("rs10738610", "rs0"))
  # Nothing matches: an empty matrix, and no PLINK run.
  expect_message(none <- og_ld_matrix(src, "rs0", ld = ref), class = "opengwasr_plink_unmatched")
  expect_equal(dim(none), c(0, 0))
})

test_that("ALIDs are matched through the GWAS Stores' variant table, or the data's own rsid column", {
  ref <- local_plink_reference()
  # 16:53767042:C:T and :A:G are both rs1421085 in the variant table: one variant, one name.
  m <- og_ld_matrix(src, c("16:53767042:C:T", "16:53767042:A:G", "rs9939609"), ld = ref)
  expect_equal(rownames(m), c("rs1421085_C_T", "rs9939609_A_T"))
  # A data frame of Associations carries rsid: no lookup is made.
  before <- length(raw_api(fake$url(), fake_key, "_fake/bodies"))
  hits <- cad_hits()
  after_hits <- length(raw_api(fake$url(), fake_key, "_fake/bodies"))
  expect_message(m2 <- og_ld_matrix(src, hits, ld = ref), class = "opengwasr_plink_unmatched")
  expect_length(raw_api(fake$url(), fake_key, "_fake/bodies"), after_hits)
  expect_equal(sort(sub("_.*", "", rownames(m2))), sort(c("rs2891168", "rs4977574", "rs10757275", "rs1333049")))
  expect_equal(og_provenance(m2)$absent, "9:22123767:A:C")
})

test_that("og_clump() clumps locally, ranked by |z|, and says what it did", {
  ref <- local_plink_reference()
  hits <- cad_hits()
  hits <- hits[hits$rsid != "rs10738610", ]
  attr(hits, "og_provenance") <- NULL
  expect_error(og_clump(src, hits, ld = ref), class = "opengwasr_plink_settings_required")
  expect_error(og_clump(src, hits, ld = ref, r2 = 0.2), class = "opengwasr_plink_settings_required")
  # rs2891168 (|z| 20) leads rs4977574 (16) and rs10757275 (8) in LD with it; rs1333049 stands alone.
  d <- fixture_dosage()
  expect_gt(stats::cor(d[, "rs2891168"], d[, "rs4977574"])^2, 0.4)
  expect_gt(stats::cor(d[, "rs2891168"], d[, "rs10757275"])^2, 0.2)
  for (settings in list(c(0.2, 100), c(0.2, 0.01), c(0.99, 100))) {
    out <- og_clump(src, hits, ld = ref, r2 = settings[[1]], kb = settings[[2]])
    want <- expected_clump(hits$rsid, abs(hits$z), settings[[1]], settings[[2]])
    expect_equal(out$rsid, hits$rsid[hits$rsid %in% want$lead])
    expect_equal(out$clump_size, want$size[match(out$rsid, want$lead)])
    expect_equal(unique(out$clump_ld), ref$path)
    expect_equal(unique(out$clump_ld_choice), "user")
    expect_equal(unique(out$clump_r2), settings[[1]])
    expect_equal(unique(out$clump_kb), settings[[2]])
    clump <- og_provenance(out)$clump
    expect_equal(c(clump$hits, clump$absent, clump$leads), c(4, 0, nrow(want)))
    expect_equal(clump$ld, ref$path)
    expect_equal(c(clump$r2, clump$kb), settings)
  }
  # The first setting, by hand: one clump of three led by rs2891168, and rs1333049 alone.
  out <- og_clump(src, hits, ld = ref, r2 = 0.2, kb = 100)
  expect_equal(out$rsid, c("rs2891168", "rs1333049"))
  expect_equal(out$clump_size, c(3L, 1L))
})

test_that("og_clump() ranks a variant listed in several Analyses by its largest |z|", {
  ref <- local_plink_reference()
  data <- data.frame(alid = c("a1", "a2", "a3"), rsid = c("rs4977574", "rs2891168", "rs4977574"),
                     z = c(3, 5, -9), stringsAsFactors = FALSE)
  data$alid <- c("9:22098575:A:G", "9:22098620:A:G", "9:22098575:A:G")
  out <- og_clump(src, data, ld = ref, r2 = 0.2, kb = 100)
  # rs4977574's |z| 9 beats rs2891168's 5, so it leads; every one of its Associations is returned.
  expect_equal(out$rsid, c("rs4977574", "rs4977574"))
})

test_that("og_tophits() with a PLINK Reference fetches unclumped hits, clumps them locally, then applies limit", {
  ref <- local_plink_reference()
  expect_error(og_tophits(src, cad, ld = ref), class = "opengwasr_plink_settings_required")
  expect_error(og_tophits(src, cad, clump = FALSE, ld = ref, r2 = 0.2, kb = 100), class = "opengwasr_argument")
  before <- length(raw_api(fake$url(), fake_key, "_fake/bodies"))
  expect_message(top <- og_tophits(src, cad, ld = ref, r2 = 0.2, kb = 100), class = "opengwasr_plink_unmatched")
  sent <- tail(Filter(function(b) identical(b$endpoint, "/api/tophits"), raw_api(fake$url(), fake_key, "_fake/bodies")), 1)[[1]]$body
  expect_equal(sent$limit, 10000)
  expect_null(sent$clump)
  expect_equal(top$rsid, c("rs2891168", "rs1333049"))
  expect_equal(top$clump_size, c(3L, 1L))
  expect_equal(unique(top$clump_ld), ref$path)
  expect_equal(unique(top$clump_ld_choice), "user")
  expect_equal(og_provenance(top)$clump$ld, ref$path)
  clump <- og_provenance(top)$clump
  expect_equal(c(clump$id, clump$hits, clump$absent, clump$leads), c(cad, "5", "1", "2"))
  expect_equal(c(clump$r2, clump$kb), c(0.2, 100))
  # `limit` counts index variants, strongest first, and warns when it cuts some off.
  expect_message(
    expect_warning(one <- og_tophits(src, cad, limit = 1, ld = ref, r2 = 0.2, kb = 100), class = "opengwasr_truncated"),
    class = "opengwasr_plink_unmatched")
  expect_equal(one$rsid, "rs2891168")
  # Provenance says `limit` cut index variants off, as the API's does; uncut, it doesn't.
  expect_true(og_provenance(one)$truncated)
  expect_false(og_provenance(top)$truncated)
})

test_that("the warning for top hits the API cut off says clumping saw only the strongest", {
  ref <- structure(list(path = "ref", plink_bin = "plink"), class = "og_plink_reference")
  contribution <- plink_clump_contribution(list(plink = ref, store = list(ld_store = "ref"), choice = "user"),
                                           list(r2 = 0.2, kb = 100, limit = 10, threshold = 5e-8),
                                           data.frame(id = c("A", "B")), NULL)
  msg <- contribution$truncated(c("A", "B"))
  expect_match(msg[[1]], "At least one of the 2 Analyses has more than 10,000 top hits", fixed = TRUE)
  expect_match(msg[[1]], "clumping saw only the strongest 10,000", fixed = TRUE)
})

test_that("og_ld_proxies() reads LD Stores only", {
  ref <- local_plink_reference()
  expect_error(og_ld_proxies(src, "16:53767042:C:T", ld = ref), class = "opengwasr_argument")
})

test_that("PLINK's temporary files are removed, on success and on failure", {
  ref <- local_plink_reference()
  left <- function() list.files(tempdir(), pattern = "^opengwasr-plink-")
  og_ld_matrix(src, c("rs4977574", "rs2891168"), ld = ref)
  expect_equal(left(), character())
  broken <- ref
  broken$path <- file.path(tempdir(), "missing-fileset")
  expect_error(plink_run(broken, list(), function(file) character(), identity), class = "opengwasr_plink_failed")
  expect_equal(left(), character())
})

test_that("the window is exact to the base pair: just inside and just outside `kb`", {
  ref <- local_plink_reference()
  # rs4977574 (22098575) and rs2891168 (22098620) are 45 bp apart and in LD (r^2 above 0.4). PLINK takes the window as exclusive: 0.046 kb (46 bp) takes it, 0.045 kb does not, so a window off by 1 bp either way flips one.
  data <- data.frame(alid = c("9:22098620:A:G", "9:22098575:A:G"), rsid = c("rs2891168", "rs4977574"),
                     z = c(5, 4), stringsAsFactors = FALSE)
  rsids <- data$rsid
  for (case in list(list(kb = 0.046, size = 2L), list(kb = 0.045, size = 1L))) {
    out <- og_clump(src, data, ld = ref, r2 = 0.2, kb = case$kb)
    want <- expected_clump(rsids, abs(data$z), 0.2, case$kb)
    expect_equal(nrow(out), nrow(want))
    expect_equal(out$clump_size[[1]], case$size)
    expect_equal(want$size[[1]], case$size)
  }
})

# Acceptance: through the fake and, with credentials and PLINK, the dev Data API too. The fixture's
# rsIDs match few live Associations, so what is asserted holds for any of them.
test_api("top hits clumped by a PLINK Reference, and an LD matrix of Associations, run end to end", {
  ref <- local_plink_reference()
  top <- suppressMessages(og_tophits(src, cad, ld = ref, r2 = 0.2, kb = 100))
  expect_true(all(c("clump_ld", "clump_ld_choice", "clump_r2", "clump_kb", "clump_size") %in% names(top)))
  expect_true(all(top$clump_ld == ref$path))
  expect_equal(og_provenance(top)$clump$ld, ref$path)
  hits <- suppressWarnings(og_tophits(src, cad, clump = FALSE, limit = 50))  # live: more than 50 exist
  m <- suppressMessages(og_ld_matrix(src, hits, ld = ref))
  expect_equal(nrow(m), ncol(m))
  expect_equal(og_provenance(m)$ld, ref$path)
  if (backend == "fake") {
    expect_equal(top$rsid, c("rs2891168", "rs1333049"))
    expect_equal(nrow(m), 4)
  }
})

# #28: a PLINK Reference needs no API when nothing is looked up. No OPENGWAS_* set, `og_api()`
# not mocked: `src = NULL` is the only Source there is.
test_that("og_ld_matrix() and og_clump() with a PLINK Reference need no Source when nothing is looked up", {
  ref <- local_plink_reference()
  withr::local_envvar(OPENGWAS_URL = NA, OPENGWAS_DEV_KEY = NA, OPENGWAS_DEV_USER = NA)
  withr::local_options(opengwasr.url = NULL, opengwasr.progress = FALSE)
  expect_error(og_api(), class = "opengwasr_auth")
  rsids <- c("rs1333049", "rs4977574", "rs2891168")
  m <- og_ld_matrix(NULL, rsids, ld = ref)
  expect_equal(dim(m), c(3, 3))
  expect_equal(unname(m), unname(og_ld_matrix(src, rsids, ld = ref)))
  expect_equal(og_provenance(m)$ld, ref$path)
  data <- data.frame(alid = c("9:22098574:A:G", "9:22103183:A:C", "9:22125503:C:G"), rsid = rsids,
                     z = c(9, 6, 8), id = "x")
  m2 <- og_ld_matrix(NULL, data, ld = ref)
  expect_equal(unclass(unname(m2)), unclass(unname(m)), ignore_attr = TRUE)
  out <- og_clump(NULL, data, ld = ref, r2 = 0.2, kb = 100)
  expect_equal(out, og_clump(src, data, ld = ref, r2 = 0.2, kb = 100), ignore_attr = TRUE)
  expect_equal(og_provenance(out)$clump$ld, ref$path)
})

test_that("a lookup that is needed without a Source says what is missing", {
  ref <- local_plink_reference()
  withr::local_envvar(OPENGWAS_URL = NA, OPENGWAS_DEV_KEY = NA, OPENGWAS_DEV_USER = NA)
  withr::local_options(opengwasr.url = NULL, opengwasr.progress = FALSE)
  alid <- "9:22098574:A:G"
  e <- expect_error(og_ld_matrix(NULL, alid, ld = ref), class = "opengwasr_source_required")
  expect_s3_class(e, "opengwasr_error")
  expect_match(conditionMessage(e), "no Source was given")
  expect_match(conditionMessage(e), "1 ALID has no rsID", fixed = TRUE)
  expect_match(conditionMessage(e), alid, fixed = TRUE)
  data <- data.frame(alid = alid, z = 5, id = "x")  # no rsid column: the ALID's rsID is looked up
  expect_error(og_clump(NULL, data, ld = ref, r2 = 0.2, kb = 100), class = "opengwasr_source_required")
  # An LD Store needs the API for everything.
  expect_error(og_ld_matrix(NULL, "rs4977574", ld = "EUR"), class = "opengwasr_source_required")
  expect_error(og_clump(NULL, data.frame(alid = alid, z = 5, id = "x", rsid = "rs1"), ld = "EUR"),
               class = "opengwasr_source_required")
})

test_that("every query function says what is wrong with a missing or foreign Source", {
  for (f in list(og_ld_proxies, og_ld_reflookup, og_ld_stores, og_tophits, og_associations, og_analyses,
                 og_phewas, og_range, og_status, og_user, og_variants_rsid, og_variants_chrpos)) {
    args <- if (identical(f, og_ld_stores) || identical(f, og_status) || identical(f, og_user)) list(NULL) else list(NULL, "rs1")
    expect_error(do.call(f, args), class = "opengwasr_source_required")
  }
  expect_error(og_ld_proxies("not a source", "rs1", ld = "EUR"), class = "opengwasr_argument")
  expect_error(og_clump("x", data.frame()), class = "opengwasr_argument")
})

test_that("no_source() has the defaults of og_api()", {
  withr::local_envvar(OPENGWAS_URL = "http://example.test", OPENGWAS_DEV_KEY = "k", OPENGWAS_DEV_USER = NA)
  withr::local_options(opengwasr.timeout = 77, opengwasr.max_in_flight = 9)
  ns <- no_source()
  api <- og_api()
  expect_equal(c(ns$timeout, ns$max_in_flight), c(api$timeout, api$max_in_flight))
})
