# PLINK References (#14, #43): a Local Source's LD resource, `og_local(bfile = )`, for LD matrices
# and clumping. Here mostly an LD-only Local Source, with top hits from the API Source (the "API |
# PLINK" row of vignette("ld")'s table). They skip without a PLINK binary. Expected values are worked
# out in plain R from the committed fixture's genotypes (helper-plink.R), not from PLINK.

fake <- local_fake_api(n_synthetic = 1)
withr::local_options(opengwasr.poll_cap = 0.05, opengwasr.progress = FALSE)
src <- fake_source(fake)
cad <- "OGS-00011:GCST003116"

# The fake's CAD top hits as a data frame of Associations (rsIDs of the fixture, one it lacks).
cad_hits <- function() og_tophits(src, cad, clump = FALSE)

test_that("og_local(bfile = ) checks its fileset and finds PLINK, with no Python", {
  # No Python is touched: an LD-only Local Source opens no Store Release.
  local_mocked_bindings(local_ogquery = function(...) stop("Python was asked for"))
  expect_error(og_local(bfile = file.path(tempdir(), "nothing")), class = "opengwasr_plink_reference_missing")
  expect_error(og_local(bfile = c("a", "b")), class = "opengwasr_argument")
  ld_src <- local_plink_source()
  ref <- og_ld_resource(ld_src)
  expect_s3_class(ld_src, "og_local_source")
  expect_s3_class(ref, "og_plink_reference")
  expect_equal(ref$path, plink_fixture())
  expect_true(file.exists(ref$plink_bin))
  expect_equal(format(ref)[[1]], "<og_plink_reference>")
  expect_equal(nrow(ld_src$releases), 0)
  expect_true("• Store Releases: none (LD only)" %in% format(ld_src))
  expect_true(paste0("• LD resource: PLINK Reference ", ref$path) %in% format(ld_src))
  e <- expect_error(og_local(bfile = plink_fixture(), plink = "/no/such/plink"), class = "opengwasr_plink_not_found")
  expect_equal(e$arg, "plink")
  # An explicit binary wins.
  expect_equal(og_ld_resource(og_local(bfile = plink_fixture(), plink = ref$plink_bin))$plink_bin, ref$plink_bin)
  # og_plink_reference() is no longer exported: the Source builds its PLINK Reference.
  expect_false("og_plink_reference" %in% getNamespaceExports("opengwasr"))
})

test_that("with no PLINK anywhere, the error says how to get it", {
  found <- tryCatch(find_plink(NULL), error = function(e) NULL)
  skip_if(!is.null(found), "PLINK is available here.")
  e <- expect_error(og_local(bfile = plink_fixture()), class = "opengwasr_plink_not_found")
  expect_match(conditionMessage(e), "genetics.binaRies")
  expect_match(conditionMessage(e), "`plink`", fixed = TRUE)
  expect_equal(e$arg, "plink")
})

test_that("choose_ld_store() takes a Local Source's PLINK Reference as the user's choice, without asking the API", {
  ld_src <- local_plink_source()
  ref <- og_ld_resource(ld_src)
  before <- length(raw_api(fake$url(), fake_key, "_fake/bodies"))
  chosen <- choose_ld_store(ld_src)
  expect_equal(chosen$choice, "user")
  expect_equal(chosen$from, "source")
  expect_equal(chosen$store$ld_store, ref$path)
  expect_identical(chosen$plink, ref)
  expect_length(raw_api(fake$url(), fake_key, "_fake/bodies"), before)
})

test_that("og_ld_matrix() reads r from PLINK, named by rsID and the reference's alleles", {
  ld_src <- local_plink_source()
  ref <- og_ld_resource(ld_src)
  rsids <- c("rs1333049", "rs4977574", "rs2891168", "rs1421085")
  m <- og_ld_matrix(ld_src, rsids)
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
  expect_equal(prov$ld_from, "source")
  expect_equal(prov$absent, character())
  expect_equal(prov$plink_bin, ref$plink_bin)
  expect_equal(prov$plink_version, ref$plink_version)
  expect_false(grepl("ld = ", prov$query, fixed = TRUE))
})

test_that("variants without a match in the reference are left out, with a message giving the count", {
  ld_src <- local_plink_source()
  # rs10738610 isn't in the fixture; rs0 names nothing.
  e <- expect_message(
    m <- og_ld_matrix(ld_src, c("rs4977574", "rs10738610", "rs0", "rs2891168")),
    class = "opengwasr_plink_unmatched")
  expect_equal(e$count, 2)
  expect_equal(e$total, 4)
  expect_match(conditionMessage(e), "2 of 4 variants matched the PLINK Reference by rsID; 2 not in it were left out.", fixed = TRUE)
  expect_equal(rownames(m), c("rs4977574_G_A", "rs2891168_G_A"))
  expect_equal(og_provenance(m)$absent, c("rs10738610", "rs0"))
  # Nothing matches: an empty matrix, and no PLINK run.
  expect_message(none <- og_ld_matrix(ld_src, "rs0"), class = "opengwasr_plink_unmatched")
  expect_equal(dim(none), c(0, 0))
})

test_that("Associations from the API Source are matched by their own rsid column, with no lookup", {
  ld_src <- local_plink_source()
  hits <- cad_hits()
  after_hits <- length(raw_api(fake$url(), fake_key, "_fake/bodies"))
  expect_message(m <- og_ld_matrix(ld_src, hits), class = "opengwasr_plink_unmatched")
  expect_length(raw_api(fake$url(), fake_key, "_fake/bodies"), after_hits)
  expect_equal(sort(sub("_.*", "", rownames(m))), sort(c("rs2891168", "rs4977574", "rs10757275", "rs1333049")))
  expect_equal(og_provenance(m)$absent, "9:22123767:A:C")
})

test_that("og_clump() clumps locally, ranked by |z|, and says what it did", {
  ld_src <- local_plink_source()
  ref <- og_ld_resource(ld_src)
  hits <- cad_hits()
  hits <- hits[hits$rsid != "rs10738610", ]
  attr(hits, "og_provenance") <- NULL
  expect_error(og_clump(ld_src, hits), class = "opengwasr_plink_settings_required")
  expect_error(og_clump(ld_src, hits, r2 = 0.2), class = "opengwasr_plink_settings_required")
  # rs2891168 (|z| 20) leads rs4977574 (16) and rs10757275 (8) in LD with it; rs1333049 stands alone.
  d <- fixture_dosage()
  expect_gt(stats::cor(d[, "rs2891168"], d[, "rs4977574"])^2, 0.4)
  expect_gt(stats::cor(d[, "rs2891168"], d[, "rs10757275"])^2, 0.2)
  for (settings in list(c(0.2, 100), c(0.2, 0.01), c(0.99, 100))) {
    out <- og_clump(ld_src, hits, r2 = settings[[1]], kb = settings[[2]])
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
  out <- og_clump(ld_src, hits, r2 = 0.2, kb = 100)
  expect_equal(out$rsid, c("rs2891168", "rs1333049"))
  expect_equal(out$clump_size, c(3L, 1L))
})

test_that("og_clump() ranks a variant listed in several Analyses by its largest |z|", {
  ld_src <- local_plink_source()
  data <- data.frame(alid = c("a1", "a2", "a3"), rsid = c("rs4977574", "rs2891168", "rs4977574"),
                     z = c(3, 5, -9), stringsAsFactors = FALSE)
  data$alid <- c("9:22098575:A:G", "9:22098620:A:G", "9:22098575:A:G")
  out <- og_clump(ld_src, data, r2 = 0.2, kb = 100)
  # rs4977574's |z| 9 beats rs2891168's 5, so it leads; every one of its Associations is returned.
  expect_equal(out$rsid, c("rs4977574", "rs4977574"))
})

test_that("og_clump() with PLINK clumps each Analysis on its own, one PLINK run each; by_analysis = FALSE clumps them as one set", {
  ld_src <- local_plink_source()
  # rs2891168 and rs4977574 are in LD (r^2 above 0.4, 45 bp apart). Analysis "a" has both, "b"
  # rs4977574, its strongest, and rs1333049; rows interleaved, so the order kept is data's.
  data <- data.frame(id = c("a", "b", "a", "b"),
                     alid = c("9:22098620:A:G", "9:22098575:A:G", "9:22098575:A:G", "9:22125503:C:G"),
                     rsid = c("rs2891168", "rs4977574", "rs4977574", "rs1333049"), z = c(5, -9, 4, 3),
                     stringsAsFactors = FALSE)
  d <- fixture_dosage()
  expect_gt(stats::cor(d[, "rs2891168"], d[, "rs4977574"])^2, 0.4)
  runs <- 0
  real_run <- plink_run
  local_mocked_bindings(plink_run = function(...) { runs <<- runs + 1; real_run(...) })
  each <- og_clump(ld_src, data, r2 = 0.2, kb = 100)
  expect_equal(runs, 2)
  # Each Analysis keeps its own strongest: rs2891168 leads "a" (absorbing its rs4977574), and
  # rs4977574 leads "b", where nothing it is in LD with competes.
  for (id in c("a", "b")) {
    mine <- data[data$id == id, ]
    want <- expected_clump(mine$rsid, abs(mine$z), 0.2, 100)
    expect_equal(each$rsid[each$id == id], mine$rsid[mine$rsid %in% want$lead])
  }
  expect_equal(each$id, c("a", "b", "b"))
  expect_equal(each$rsid, c("rs2891168", "rs4977574", "rs1333049"))
  expect_equal(each$clump_size, c(2L, 1L, 1L))
  clump <- og_provenance(each)$clump
  expect_equal(clump[c("id", "hits", "absent", "leads")], data.frame(id = c("a", "b"), hits = c(2, 2), absent = c(0, 0), leads = c(1, 2)))
  expect_equal(og_provenance(each)$query, "og_clump(r2 = 0.2, kb = 100, by_analysis = TRUE)")
  # As one set, rs4977574 (|z| 9, in "b") leads, absorbs rs2891168, and keeps both its Associations.
  runs <- 0
  joint <- og_clump(ld_src, data, r2 = 0.2, kb = 100, by_analysis = FALSE)
  expect_equal(runs, 1)
  want <- expected_clump(unique(data$rsid), c(5, 9, 3), 0.2, 100)
  expect_equal(unique(joint$rsid), want$lead)
  expect_equal(joint$id, c("b", "a", "b"))
  expect_equal(joint$rsid, c("rs4977574", "rs4977574", "rs1333049"))
  expect_equal(joint$clump_size, c(2L, 2L, 1L))
  expect_true(is.na(og_provenance(joint)$clump$id))
  # With no `id` column, one set either way.
  expect_equal(og_clump(ld_src, data[-1], r2 = 0.2, kb = 100), joint[-1], ignore_attr = TRUE)
})

test_that("LD from a PLINK Reference warns that it isn't adjusted for principal components, once per reference in a session", {
  ld_src <- local_plink_source()
  ref <- og_ld_resource(ld_src)
  data <- data.frame(id = c("a", "b"), alid = c("9:22098620:A:G", "9:22098575:A:G"),
                     rsid = c("rs2891168", "rs4977574"), z = c(5, 4), stringsAsFactors = FALSE)
  # Clumping and LD matrices both warn, from where PLINK runs; the second call, and every call
  # after, says nothing.
  calls <- list(function() og_clump(ld_src, data, r2 = 0.2, kb = 100, by_analysis = FALSE),
                function() og_ld_matrix(ld_src, c("rs4977574", "rs2891168")))
  for (call in calls) {
    reset_plink_unadjusted(ref$path)
    w <- expect_warning(call(), class = "opengwasr_plink_unadjusted")
    expect_equal(w$path, ref$path)
    expect_match(conditionMessage(w), "without adjusting for population structure (principal components)", fixed = TRUE)
    expect_no_warning(call())
    expect_no_warning(og_clump(ld_src, data, r2 = 0.2, kb = 100))  # two PLINK runs, no warning
  }
  # Another PLINK Reference is another warning, once.
  dir <- withr::local_tempdir()
  file.copy(paste0(plink_fixture(), c(".bed", ".bim", ".fam")), dir)
  other <- og_local(bfile = file.path(dir, "ref"))
  w <- expect_warning(og_ld_matrix(other, c("rs4977574", "rs2891168")), class = "opengwasr_plink_unadjusted")
  expect_equal(w$path, og_ld_resource(other)$path)
  expect_no_warning(og_ld_matrix(other, c("rs4977574", "rs2891168")))
  # Nothing to compute, nothing to warn of: an rsID the reference lacks runs no PLINK.
  reset_plink_unadjusted(ref$path)
  expect_no_warning(suppressMessages(og_ld_matrix(ld_src, "rs1")))
  # og_tophits() clumping with a Local Source's PLINK Reference, too.
  skip_if_no_local()
  reset_plink_unadjusted(ref$path)
  expect_warning(suppressMessages(og_tophits(local_source_of("dense", bfile = plink_fixture()), "s:bmi_irn",
                                             r2 = 0.001, kb = 10000)),
                 class = "opengwasr_plink_unadjusted")
})

test_that("PLINK matches each row by its own rsID: rows sharing an ALID with different rsIDs, or none, are matched apart", {
  ld_src <- local_plink_source()
  x <- "9:22098620:A:G"
  y <- "9:22125503:C:G"
  data <- data.frame(id = c("a", "a", "b", "c"), alid = c(x, y, x, x),
                     rsid = c("rs2891168", "rs1333049", "rs4977574", NA), z = c(5, 3, 9, 7), stringsAsFactors = FALSE)
  # Four (ALID, rsID) pairs, one with no rsID: the message and `absent` count rows' pairs.
  m <- expect_message(each <- og_clump(ld_src, data, r2 = 0.2, kb = 100), class = "opengwasr_plink_unmatched")
  expect_equal(c(m$count, m$total), c(1, 4))
  expect_equal(each$id, c("a", "a", "b"))
  expect_equal(each$rsid, c("rs2891168", "rs1333049", "rs4977574"))
  expect_equal(og_provenance(each)$absent, x)
  expect_equal(og_provenance(each)$clump[c("id", "hits", "absent", "leads")],
               data.frame(id = c("a", "b", "c"), hits = c(2, 1, 1), absent = c(0, 0, 1), leads = c(2, 1, 0)))
  # As one set: rs4977574 (|z| 9) leads and absorbs rs2891168; each row keeps its own rsID, so only
  # the row naming rs4977574 is its index variant's, not every row of its ALID.
  joint <- suppressMessages(og_clump(ld_src, data, r2 = 0.2, kb = 100, by_analysis = FALSE))
  expect_equal(joint$id, c("a", "b"))
  expect_equal(joint$rsid, c("rs1333049", "rs4977574"))
  expect_equal(joint$clump_size, c(1L, 2L))
  # An LD matrix of the same rows: each distinct rsID the reference holds, once.
  mat <- suppressMessages(og_ld_matrix(ld_src, data))
  expect_setequal(sub("_.*", "", rownames(mat)), c("rs2891168", "rs1333049", "rs4977574"))
})

# The "API | PLINK" row of vignette("ld")'s table: top hits from the API Source, unclumped, clumped
# by an LD-only Local Source's PLINK Reference.
test_that("og_clump(og_local(bfile = ), og_tophits(og_api(), ids, clump = FALSE)) clumps the API's top hits with PLINK", {
  ld_src <- local_plink_source()
  ref <- og_ld_resource(ld_src)
  hits <- og_tophits(src, cad, clump = FALSE)
  expect_error(og_clump(ld_src, hits), class = "opengwasr_plink_settings_required")
  before <- length(raw_api(fake$url(), fake_key, "_fake/bodies"))
  expect_message(top <- og_clump(ld_src, hits, r2 = 0.2, kb = 100), class = "opengwasr_plink_unmatched")
  expect_length(raw_api(fake$url(), fake_key, "_fake/bodies"), before)  # nothing more asked of the API
  expect_equal(top$rsid, c("rs2891168", "rs1333049"))
  expect_equal(top$clump_size, c(3L, 1L))
  expect_equal(unique(top$clump_ld), ref$path)
  expect_equal(unique(top$clump_ld_choice), "user")
  prov <- og_provenance(top)
  expect_equal(prov$ld_from, "source")
  expect_equal(prov$plink_version, ref$plink_version)
  expect_equal(c(prov$clump$hits, prov$clump$absent, prov$clump$leads), c(5, 1, 2))
  expect_equal(c(prov$clump$r2, prov$clump$kb), c(0.2, 100))
  expect_equal(prov$clump$clumps, "computed")
})

test_that("the warning for top hits the API cut off says clumping saw only the strongest", {
  ref <- structure(list(path = "ref", plink_bin = "plink", plink_version = "1.90b6.21"), class = "og_plink_reference")
  contribution <- plink_clump_contribution(list(plink = ref, store = list(ld_store = "ref"), choice = "user"),
                                           list(r2 = 0.2, kb = 100, limit = 10, threshold = 5e-8),
                                           data.frame(id = c("A", "B")), NULL)
  msg <- contribution$truncated(c("A", "B"))
  expect_match(msg[[1]], "At least one of the 2 Analyses has more than 10,000 top hits", fixed = TRUE)
  expect_match(msg[[1]], "clumping saw only the strongest 10,000", fixed = TRUE)
})

test_that("og_ld_proxies() and og_ld_reflookup() read LD Stores only: a Local Source is refused", {
  ld_src <- local_plink_source()
  expect_error(og_ld_proxies(ld_src, "rs4977574"), class = "opengwasr_local_unsupported")
  expect_error(og_ld_reflookup(ld_src, "rs4977574"), class = "opengwasr_local_unsupported")
})

test_that("PLINK's temporary files are removed, on success and on failure", {
  ld_src <- local_plink_source()
  ref <- og_ld_resource(ld_src)
  left <- function() list.files(tempdir(), pattern = "^opengwasr-plink-")
  og_ld_matrix(ld_src, c("rs4977574", "rs2891168"))
  expect_equal(left(), character())
  broken <- ref
  broken$path <- file.path(tempdir(), "missing-fileset")
  reset_plink_unadjusted(broken$path)  # another reference: warned of once, as it is about to run
  expect_warning(expect_error(plink_run(broken, list(), function(file) character(), identity), class = "opengwasr_plink_failed"),
                 class = "opengwasr_plink_unadjusted")
  expect_equal(left(), character())
})

test_that("the window is exact to the base pair: just inside and just outside `kb`", {
  ld_src <- local_plink_source()
  # rs4977574 (22098575) and rs2891168 (22098620) are 45 bp apart and in LD (r^2 above 0.4). PLINK takes the window as exclusive: 0.046 kb (46 bp) takes it, 0.045 kb does not, so a window off by 1 bp either way flips one.
  data <- data.frame(alid = c("9:22098620:A:G", "9:22098575:A:G"), rsid = c("rs2891168", "rs4977574"),
                     z = c(5, 4), stringsAsFactors = FALSE)
  rsids <- data$rsid
  for (case in list(list(kb = 0.046, size = 2L), list(kb = 0.045, size = 1L))) {
    out <- og_clump(ld_src, data, r2 = 0.2, kb = case$kb)
    want <- expected_clump(rsids, abs(data$z), 0.2, case$kb)
    expect_equal(nrow(out), nrow(want))
    expect_equal(out$clump_size[[1]], case$size)
    expect_equal(want$size[[1]], case$size)
  }
})

# Acceptance: through the fake and, with credentials and PLINK, the dev Data API too. The fixture's
# rsIDs match few live Associations, so what is asserted holds for any of them.
test_api("the API's top hits clumped by a PLINK Reference, and an LD matrix of Associations, run end to end", {
  ld_src <- local_plink_source()
  ref <- og_ld_resource(ld_src)
  hits <- suppressWarnings(og_tophits(src, cad, clump = FALSE, limit = 50))  # live: more than 50 exist
  top <- suppressMessages(og_clump(ld_src, hits, r2 = 0.2, kb = 100))
  expect_true(all(c("clump_ld", "clump_ld_choice", "clump_r2", "clump_kb", "clump_size") %in% names(top)))
  expect_true(all(top$clump_ld == ref$path))
  expect_equal(og_provenance(top)$clump$ld, ref$path)
  m <- suppressMessages(og_ld_matrix(ld_src, hits))
  expect_equal(nrow(m), ncol(m))
  expect_equal(og_provenance(m)$ld, ref$path)
  if (backend == "fake") {
    expect_equal(top$rsid, c("rs2891168", "rs1333049"))
    expect_equal(nrow(m), 4)
  }
})

# #28, now #43: a PLINK Reference needs no API when nothing is looked up. No OPENGWAS_* set: an
# LD-only Local Source is the only Source there is, and it never calls the API.
test_that("an LD-only Local Source needs no credentials, and answers as from any Source", {
  ld_src <- local_plink_source()
  ref <- og_ld_resource(ld_src)
  withr::local_envvar(OPENGWAS_URL = NA, OPENGWAS_DEV_KEY = NA, OPENGWAS_DEV_USER = NA)
  withr::local_options(opengwasr.url = NULL, opengwasr.progress = FALSE)
  expect_error(og_api(), class = "opengwasr_auth")
  rsids <- c("rs1333049", "rs4977574", "rs2891168")
  m <- og_ld_matrix(og_local(bfile = plink_fixture()), rsids)
  expect_equal(dim(m), c(3, 3))
  expect_equal(og_provenance(m)$ld, ref$path)
  data <- data.frame(alid = c("9:22098574:A:G", "9:22103183:A:C", "9:22125503:C:G"), rsid = rsids,
                     z = c(9, 6, 8), id = "x")
  m2 <- og_ld_matrix(ld_src, data)
  expect_equal(unclass(unname(m2)), unclass(unname(m)), ignore_attr = TRUE)
  out <- og_clump(ld_src, data, r2 = 0.2, kb = 100)
  want <- expected_clump(rsids, abs(data$z), 0.2, 100)
  expect_equal(out$rsid, rsids[rsids %in% want$lead])
  expect_equal(out$clump_size, want$size[match(out$rsid, want$lead)])
  expect_equal(og_provenance(out)$clump$ld, ref$path)
  expect_equal(og_provenance(out)$job_ids, character())
})

test_that("an LD-only Local Source refuses ALIDs it can't look up, naming og_local(gwas = , bfile = )", {
  ld_src <- local_plink_source()
  alids <- c("9:22098574:A:G", "9:22103183:A:C")
  # Refused before PLINK runs.
  local_mocked_bindings(plink_run = function(...) stop("PLINK ran"))
  e <- expect_error(og_ld_matrix(ld_src, c(alids, "rs1333049")), class = "opengwasr_gwas_store_required")
  expect_s3_class(e, "opengwasr_refusal")
  expect_equal(e$alids, alids)
  expect_equal(e$arg, "gwas")
  expect_match(conditionMessage(e), "og_local(gwas = , bfile = )", fixed = TRUE)
  expect_match(conditionMessage(e), "2 ALIDs", fixed = TRUE)
  # The ways round, as `forms`: Store Releases, an `rsid` column, or the rsIDs looked up on the API.
  expect_equal(e$forms, c("gwas", "rsid_column", "api_rsids"))
  expect_match(conditionMessage(e), "og_variants_chrpos(og_api(), positions)", fixed = TRUE)
  data <- data.frame(alid = alids, z = c(5, 4), id = "x")  # no rsid column: the ALIDs' rsIDs would be looked up
  e <- expect_error(og_clump(ld_src, data, r2 = 0.2, kb = 100), class = "opengwasr_gwas_store_required")
  expect_equal(e$alids, alids)
})

test_that("every query function says what is wrong with a missing or foreign Source", {
  for (f in list(og_ld_proxies, og_ld_reflookup, og_ld_stores, og_tophits, og_associations, og_analyses,
                 og_phewas, og_range, og_status, og_user, og_variants_rsid, og_variants_chrpos, og_clump, og_ld_matrix)) {
    args <- if (identical(f, og_ld_stores) || identical(f, og_status) || identical(f, og_user)) list(NULL) else list(NULL, "rs1")
    expect_error(do.call(f, args), class = "opengwasr_source_required")
  }
  expect_error(og_ld_proxies("not a source", "rs1"), class = "opengwasr_argument")
  expect_error(og_clump("x", data.frame()), class = "opengwasr_argument")
})

test_that("PLINK runs on one thread", {
  skip_on_os("windows")
  # A stand-in PLINK that prints its arguments, one a line, and fails, so the log carries them.
  bin <- withr::local_tempfile(fileext = ".sh")
  writeLines(c("#!/bin/sh", "for a in \"$@\"; do echo \"$a\"; done", "exit 3"), bin)
  Sys.chmod(bin, "0755")
  ref <- structure(list(path = plink_fixture(), plink_bin = bin), class = "og_plink_reference")
  e <- expect_error(plink_run(ref, list(), function(file) c("--r", "square"), identity), class = "opengwasr_plink_failed")
  at <- match("--threads", e$log)
  expect_false(is.na(at))
  expect_equal(e$log[[at + 1]], "1")
})
