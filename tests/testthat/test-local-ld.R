# Clumping and LD on a Local Source (#34, ADR 0006): the Source carries its LD resource,
# `og_local(gwas, plink, bfile)`, and every LD verb uses it unless `ld` overrides it for one call.
# Local tests skip without Python and ogquery, PLINK ones without a PLINK binary. Expected values come
# from the bundled fileset's genotypes, decoded in plain R (helper-plink.R), never from PLINK.

withr::local_options(opengwasr.progress = FALSE)

bmi <- "s:bmi_irn"
clump_cols <- c("clump_ld", "clump_ld_choice", "clump_r2", "clump_kb", "clump_size")

# The example Store Release `dense` (GWAS Store `s`), with the bundled PLINK Reference or without
# any: each opened once (local_source_of()).
dense_plink <- function() local_source_of("dense", bfile = plink_fixture())
dense_bare <- function() local_source_of("dense")

test_that("og_local() refuses the LD resources it can't read yet, a mixture of them, and `plink` alone, before any Python", {
  gwas <- example_releases("dense")
  e <- expect_error(og_local(gwas, ld_store = "/data/ld/EUR"), class = "opengwasr_local_unsupported")
  expect_s3_class(e, "opengwasr_ld_resource_unavailable")
  expect_s3_class(e, "opengwasr_refusal")
  expect_equal(e$resource, "ld_store")
  e <- expect_error(og_local(gwas, geno_store = "/data/geno/EUR"), class = "opengwasr_local_unsupported")
  expect_s3_class(e, "opengwasr_ld_resource_unavailable")
  expect_equal(e$resource, "geno_store")
  # Two kinds at once, whichever: refused as an argument error, never one chosen over the other.
  e <- expect_error(og_local(gwas, bfile = plink_fixture(), ld_store = "/data/ld/EUR"), class = "opengwasr_ld_resource_mixed")
  expect_equal(e$given, c("plink", "ld_store"))
  e <- expect_error(og_local(gwas, plink = "/usr/bin/plink", geno_store = "/g"), class = "opengwasr_ld_resource_mixed")
  expect_equal(e$given, c("plink", "geno_store"))
  e <- expect_error(og_local(gwas, ld_store = "/l", geno_store = "/g"), class = "opengwasr_ld_resource_mixed")
  expect_s3_class(e, "opengwasr_argument")
  expect_equal(e$given, c("ld_store", "geno_store"))
  e <- expect_error(og_local(gwas, plink = "/usr/bin/plink"), class = "opengwasr_argument")
  expect_equal(e$arg, "bfile")
  # og_plink_reference() checks the fileset and PLINK, naming the argument the user gave.
  e <- expect_error(og_local(gwas, bfile = file.path(tempdir(), "nothing")), class = "opengwasr_plink_reference_missing")
  expect_equal(e$arg, "bfile")
  e <- expect_error(og_local(gwas, bfile = c("a", "b")), class = "opengwasr_argument")
  expect_equal(e$arg, "bfile")
  e <- expect_error(og_local(gwas, bfile = plink_fixture(), plink = file.path(tempdir(), "no-plink")),
                    class = "opengwasr_plink_not_found")
  expect_equal(e$arg, "plink")
  # og_plink_reference() itself names its own.
  expect_equal(expect_error(og_plink_reference(file.path(tempdir(), "nothing")), class = "opengwasr_plink_reference_missing")$arg, "path")
  expect_equal(expect_error(og_plink_reference(plink_fixture(), plink_bin = "/no/plink"), class = "opengwasr_plink_not_found")$arg, "plink_bin")
})

test_that("og_local(gwas, plink, bfile) holds the PLINK Reference og_plink_reference() builds, and prints it", {
  skip_if_no_local()
  ref <- local_plink_reference()
  src <- og_local(gwas = example_releases("dense"), plink = ref$plink_bin, bfile = plink_fixture())
  expect_identical(src$ld, ref)
  # `plink = NULL` finds PLINK as og_plink_reference() does.
  expect_identical(dense_plink()$ld, ref)
  shown <- format(src)
  expect_true(paste0("• LD resource: PLINK Reference ", ref$path) %in% shown)
  expect_true(paste0("  PLINK: ", ref$plink_bin) %in% shown)
  bare <- dense_bare()
  expect_null(bare$ld)
  expect_true("• LD resource: none" %in% format(bare))
})

test_that("a Local Source with no LD resource refuses clumping, naming the LD forms and `clump = FALSE`, which works", {
  skip_if_no_local()
  local <- dense_bare()
  e <- expect_error(og_tophits(local, bmi), class = "opengwasr_ld_resource_required")
  expect_s3_class(e, "opengwasr_refusal")
  # It names the LD forms (the Source's PLINK Reference, one for this call, the API's LD Stores in
  # two steps) and `clump = FALSE`.
  expect_equal(e$forms, c("bfile", "ld_plink_reference", "api_two_step"))
  expect_true(e$clump_false)
  top <- og_tophits(local, bmi, clump = FALSE)
  expect_equal(nrow(top), 8)
  expect_false(any(clump_cols %in% names(top)))
  # The other LD verbs refuse it the same way, without the clumping advice.
  e <- expect_error(og_ld_matrix(local, c("rs5000", "rs1000")), class = "opengwasr_ld_resource_required")
  expect_false(e$clump_false)
  expect_equal(e$forms, c("bfile", "ld_plink_reference", "api_two_step"))
  expect_error(og_clump(local, top, r2 = 0.001, kb = 10000), class = "opengwasr_ld_resource_required")
  # An LD Store named in `ld` would make a local query call the API: refused.
  e <- expect_error(og_tophits(local, bmi, ld = "EUR"), class = "opengwasr_local_unsupported")
  expect_s3_class(e, "opengwasr_ld_resource_unavailable")
  expect_equal(e$resource, "ld_store")
  expect_equal(e$forms, c("ld_plink_reference", "bfile", "api_two_step"))
  expect_error(og_ld_matrix(local, "rs5000", ld = "EUR"), class = "opengwasr_local_unsupported")
  expect_error(og_clump(local, top, ld = "EUR"), class = "opengwasr_local_unsupported")
  # LD Stores only: refused on any Local Source.
  expect_error(og_ld_proxies(local, "rs5000"), class = "opengwasr_local_unsupported")
  expect_error(og_ld_reflookup(local, "rs5000"), class = "opengwasr_local_unsupported")
})

test_that("og_tophits() on a Local Source with a PLINK Reference clumps on the fly, as PLINK's rule gives on the fileset", {
  skip_if_no_local()
  ref <- local_plink_reference()
  src <- dense_plink()
  # A PLINK Reference has no default r2 and kb, whether it is the Source's or `ld`'s.
  expect_error(og_tophits(src, bmi), class = "opengwasr_plink_settings_required")
  expect_error(og_tophits(src, bmi, r2 = 0.001), class = "opengwasr_plink_settings_required")
  expect_error(og_tophits(dense_bare(), bmi, ld = ref), class = "opengwasr_plink_settings_required")
  hits <- og_tophits(src, bmi, clump = FALSE)
  e <- expect_message(top <- og_tophits(src, bmi, r2 = 0.001, kb = 10000), class = "opengwasr_plink_unmatched")
  # 1:5000:C:T has no rsID and rs1007 isn't in the fileset: 6 of the 8 top hits are clumped.
  expect_equal(c(e$count, e$total), c(2, 8))
  matched <- hits[!is.na(hits$rsid) & hits$rsid %in% fixture_bim()$rsid, ]
  rsids <- unique(matched$rsid)
  score <- vapply(rsids, function(r) max(abs(matched$z[matched$rsid == r])), 0)
  want <- expected_clump(rsids, score, 0.001, 10000)
  # By hand: rs5000 takes rs1000 (r^2 about 0.39), rs1005 takes rs1006 (0.57), and rs1004 is alone
  # (its r^2 with rs5000 is below 0.001).
  expect_equal(want$lead, c("rs5000", "rs1005", "rs1004"))
  expect_equal(want$size, c(2L, 2L, 1L))
  # Every Association of an index variant, in the top hits' own order.
  expect_equal(top$alid, c("1:5000:C:G", "1:5000:A:C", "2:300:C:T", "1:9000:A:G"))
  expect_equal(top$rsid, c("rs5000", "rs5000", "rs1005", "rs1004"))
  expect_equal(top$clump_size, want$size[match(top$rsid, want$lead)])
  expect_equal(unique(top$clump_ld), ref$path)
  expect_equal(unique(top$clump_ld_choice), "user")
  expect_equal(unique(top$clump_r2), 0.001)
  expect_equal(unique(top$clump_kb), 10000)
  expect_equal(utils::tail(names(top), 5), clump_cols)
  # Provenance: the configuration, and that the Clumps were computed on the fly.
  prov <- og_provenance(top)
  expect_equal(prov$clump, data.frame(id = bmi, ld = ref$path, ld_choice = "user", r2 = 0.001, kb = 10000,
                                      hits = 8, absent = 2, leads = 3, clumps = "computed"))
  expect_equal(prov$plink_bin, ref$plink_bin)
  expect_equal(prov$ld_from, "source")
  expect_equal(prov$store_releases, "s/r")
  expect_match(format(prov), "3 index variants, computed", fixed = TRUE, all = FALSE)
})

test_that("the Source's PLINK Reference clumps exactly as the same reference given as `ld` for one call, on the same rows", {
  skip_if_no_local()
  ref <- local_plink_reference()
  by_source <- suppressMessages(og_tophits(dense_plink(), bmi, r2 = 0.001, kb = 10000))
  by_ld <- suppressMessages(og_tophits(dense_bare(), bmi, ld = ref, r2 = 0.001, kb = 10000))
  expect_equal(by_source, by_ld, ignore_attr = TRUE)
  expect_equal(og_provenance(by_source)$clump, og_provenance(by_ld)$clump)
  # Provenance says where the LD came from.
  expect_equal(c(og_provenance(by_source)$ld_from, og_provenance(by_ld)$ld_from), c("source", "call"))
  # And as og_clump() with that reference and no Source at all, on the unclumped rows (the path the
  # API Source's `ld = og_plink_reference()` takes): the same index variants, columns and settings.
  hits <- og_tophits(dense_bare(), bmi, clump = FALSE)
  joint <- suppressMessages(og_clump(NULL, hits, ld = ref, r2 = 0.001, kb = 10000))
  expect_equal(joint$alid, by_source$alid)
  expect_equal(joint[clump_cols], by_source[clump_cols])
  expect_equal(og_provenance(joint)$clump[c("ld", "ld_choice", "r2", "kb", "leads", "clumps")],
               og_provenance(by_source)$clump[c("ld", "ld_choice", "r2", "kb", "leads", "clumps")])
  # `limit` counts index variants, strongest first, and says when it cut some off.
  w <- expect_warning(one <- suppressMessages(og_tophits(dense_plink(), bmi, limit = 1, r2 = 0.001, kb = 10000)),
                      class = "opengwasr_truncated")
  expect_equal(unique(one$rsid), "rs5000")
  expect_true(og_provenance(one)$truncated)
})

test_that("og_clump() and og_ld_matrix() on a Local Source read its PLINK Reference, looking rsIDs up in its own Store Releases", {
  skip_if_no_local()
  ref <- local_plink_reference()
  src <- dense_plink()
  hits <- og_tophits(src, bmi, clump = FALSE)
  expect_error(og_clump(src, hits), class = "opengwasr_plink_settings_required")
  joint <- suppressMessages(og_clump(src, hits, r2 = 0.001, kb = 10000))
  expect_equal(joint$rsid, c("rs5000", "rs5000", "rs1005", "rs1004"))
  expect_equal(joint$clump_size, c(2L, 2L, 2L, 1L))
  expect_equal(unique(joint$clump_ld), ref$path)
  # ALIDs without an `rsid` column: their rsIDs come from the Local Source's variant table.
  hits_no_rsid <- hits[c("id", "alid", "z")]
  expect_equal(suppressMessages(og_clump(src, hits_no_rsid, r2 = 0.001, kb = 10000))$alid, joint$alid)
  alids <- c("1:5000:C:G", "1:1000:A:G", "2:300:C:T", "2:7000:A:C")
  rsids <- c("rs5000", "rs1000", "rs1005", "rs1006")
  m <- og_ld_matrix(src, alids)
  bim <- fixture_bim()
  names <- paste(rsids, bim$a1[match(rsids, bim$rsid)], bim$a2[match(rsids, bim$rsid)], sep = "_")
  expect_equal(dimnames(m), list(names, names))
  expected <- stats::cor(fixture_dosage()[, rsids])
  plain <- unclass(m)
  attr(plain, "og_provenance") <- NULL
  expect_equal(plain, expected, tolerance = 1e-5, ignore_attr = TRUE)
  prov <- og_provenance(m)
  expect_equal(prov$ld, ref$path)
  expect_equal(prov$ld_choice, "user")
  expect_equal(prov$ld_from, "source")
  expect_equal(prov$absent, character())
  # The lookup read the Store Release `s/r`, and ran no Job.
  expect_equal(prov$store_releases, "s/r")
  expect_equal(prov$job_ids, character())
  # A variant the fileset lacks (rs1007), and an rsID none holds, are left out with the message.
  e <- expect_message(m2 <- og_ld_matrix(src, c("rs5000", "X:2500:G:T", "rs0")), class = "opengwasr_plink_unmatched")
  expect_equal(e$count, 2)
  expect_equal(rownames(m2), "rs5000_C_G")
  # LD Stores only: a PLINK Reference, the Source's or `ld`'s, is refused.
  expect_error(og_ld_proxies(src, "rs5000"), class = "opengwasr_local_unsupported")
  expect_error(og_ld_reflookup(src, "rs5000", ld = ref), class = "opengwasr_local_unsupported")
})

test_that("`ld` overrides a Local Source's PLINK Reference for one call", {
  skip_if_no_local()
  ref <- local_plink_reference()
  # The same fileset at another path, so the answer says which reference was read.
  dir <- withr::local_tempdir()
  file.copy(paste0(plink_fixture(), c(".bed", ".bim", ".fam")), dir)
  other <- og_plink_reference(file.path(dir, "ref"))
  src <- dense_plink()
  m <- og_ld_matrix(src, c("rs5000", "rs1000"), ld = other)
  expect_equal(og_provenance(m)$ld, other$path)
  expect_equal(og_provenance(m)$ld_from, "call")
  expect_equal(og_provenance(og_ld_matrix(src, c("rs5000", "rs1000")))$ld, ref$path)
  top <- suppressMessages(og_tophits(src, bmi, ld = other, r2 = 0.001, kb = 10000))
  expect_equal(unique(top$clump_ld), other$path)
  joint <- suppressMessages(og_clump(src, og_tophits(src, bmi, clump = FALSE), ld = other, r2 = 0.001, kb = 10000))
  expect_equal(unique(joint$clump_ld), other$path)
  # The Source's own is unchanged.
  expect_identical(src$ld, ref)
})

test_that("og_ld_resource() gives a Local Source's PLINK Reference, or NULL without one", {
  skip_if_no_local()
  expect_null(og_ld_resource(dense_bare()))
  ref <- local_plink_reference()
  got <- og_ld_resource(dense_plink())
  expect_s3_class(got, "og_plink_reference")
  expect_identical(got, ref)
})
