# Clumping and LD on a Local Source (#34, #43, ADR 0006): the Source carries its LD resource,
# `og_local(gwas, plink, bfile)`, and every LD verb uses it; other LD is another Source,
# `og_local(inherit = src, bfile = )`. `og_local(bfile = )` alone is an LD-only Local Source.
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
  # The fileset and PLINK are checked, naming the argument the user gave.
  e <- expect_error(og_local(gwas, bfile = file.path(tempdir(), "nothing")), class = "opengwasr_plink_reference_missing")
  expect_equal(e$arg, "bfile")
  e <- expect_error(og_local(gwas, bfile = c("a", "b")), class = "opengwasr_argument")
  expect_equal(e$arg, "bfile")
  e <- expect_error(og_local(gwas, bfile = plink_fixture(), plink = file.path(tempdir(), "no-plink")),
                    class = "opengwasr_plink_not_found")
  expect_equal(e$arg, "plink")
  # Neither Store Releases nor an LD resource: nothing to answer.
  e <- expect_error(og_local(), class = "opengwasr_argument")
  expect_equal(e$arg, "gwas")
  expect_error(og_local(gwas = NULL), class = "opengwasr_argument")
})

test_that("og_local(gwas, plink, bfile) holds the PLINK Reference og_local(bfile) builds, and prints it", {
  skip_if_no_local()
  ref <- local_plink_reference()
  src <- og_local(gwas = example_releases("dense"), plink = ref$plink_bin, bfile = plink_fixture())
  expect_identical(og_ld_resource(src), ref)
  # `plink = NULL` finds PLINK as for an LD-only Local Source.
  expect_identical(og_ld_resource(dense_plink()), ref)
  shown <- format(src)
  expect_true(paste0("• LD resource: PLINK Reference ", ref$path) %in% shown)
  expect_true(paste0("  PLINK ", ref$plink_version, ": ", ref$plink_bin) %in% shown)
  bare <- dense_bare()
  expect_null(og_ld_resource(bare))
  expect_true("• LD resource: none" %in% format(bare))
})

test_that("a Local Source with no LD resource refuses clumping, naming the LD forms and `clump = FALSE`, which works", {
  skip_if_no_local()
  local <- dense_bare()
  e <- expect_error(og_tophits(local, bmi), class = "opengwasr_ld_resource_required")
  expect_s3_class(e, "opengwasr_refusal")
  # It names the LD forms (a PLINK Reference on the Source, the API's LD Stores in two steps) and
  # `clump = FALSE`.
  expect_equal(e$forms, c("bfile", "api_two_step"))
  expect_match(conditionMessage(e), "og_local(inherit = src, bfile = ", fixed = TRUE)
  expect_true(e$clump_false)
  top <- og_tophits(local, bmi, clump = FALSE)
  expect_equal(nrow(top), 8)
  expect_false(any(clump_cols %in% names(top)))
  # The other LD verbs refuse it the same way, without the clumping advice.
  e <- expect_error(og_ld_matrix(local, c("rs5000", "rs1000")), class = "opengwasr_ld_resource_required")
  expect_false(e$clump_false)
  expect_equal(e$forms, c("bfile", "api_two_step"))
  expect_error(og_clump(local, top, r2 = 0.001, kb = 10000), class = "opengwasr_ld_resource_required")
  # A downloaded LD Store can't be read yet, given to a new Source or inherited into one.
  e <- expect_error(og_local(inherit = local, ld_store = "/data/ld/EUR"), class = "opengwasr_local_unsupported")
  expect_s3_class(e, "opengwasr_ld_resource_unavailable")
  expect_equal(e$resource, "ld_store")
  # LD Stores only: refused on any Local Source.
  expect_error(og_ld_proxies(local, "rs5000"), class = "opengwasr_local_unsupported")
  expect_error(og_ld_reflookup(local, "rs5000"), class = "opengwasr_local_unsupported")
})

test_that("og_tophits() on a Local Source with a PLINK Reference clumps on the fly, as PLINK's rule gives on the fileset", {
  skip_if_no_local()
  ref <- local_plink_reference()
  src <- dense_plink()
  # A PLINK Reference has no default r2 and kb, however the Source came by it.
  expect_error(og_tophits(src, bmi), class = "opengwasr_plink_settings_required")
  expect_error(og_tophits(src, bmi, r2 = 0.001), class = "opengwasr_plink_settings_required")
  expect_error(og_tophits(og_local(inherit = dense_bare(), bfile = plink_fixture()), bmi), class = "opengwasr_plink_settings_required")
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

test_that("the Local | PLINK and API | PLINK rows clump alike: og_tophits() on the Source, og_clump() on an LD-only Source", {
  skip_if_no_local()
  ref <- local_plink_reference()
  by_source <- suppressMessages(og_tophits(dense_plink(), bmi, r2 = 0.001, kb = 10000))
  # og_local(inherit = , bfile = ) is the same Source as one made with `bfile`.
  by_inherit <- suppressMessages(og_tophits(og_local(inherit = dense_bare(), bfile = plink_fixture()), bmi, r2 = 0.001, kb = 10000))
  expect_equal(by_source, by_inherit, ignore_attr = TRUE)
  expect_equal(og_provenance(by_source)$clump, og_provenance(by_inherit)$clump)
  expect_equal(c(og_provenance(by_source)$ld_from, og_provenance(by_inherit)$ld_from), c("source", "source"))
  # And as og_clump() on an LD-only Local Source, on the unclumped rows (the path top hits from
  # anywhere take): the same index variants, columns and settings.
  hits <- og_tophits(dense_bare(), bmi, clump = FALSE)
  joint <- suppressMessages(og_clump(local_plink_source(), hits, r2 = 0.001, kb = 10000))
  expect_equal(joint$alid, by_source$alid)
  expect_equal(joint[clump_cols], by_source[clump_cols])
  expect_equal(og_provenance(joint)$clump[c("ld", "ld_choice", "r2", "kb", "leads", "clumps")],
               og_provenance(by_source)$clump[c("ld", "ld_choice", "r2", "kb", "leads", "clumps")])
  expect_equal(og_provenance(joint)$ld_from, "source")
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
  # LD Stores only: a Local Source, whose LD is a PLINK Reference, is refused.
  expect_error(og_ld_proxies(src, "rs5000"), class = "opengwasr_local_unsupported")
  expect_error(og_ld_reflookup(src, "rs5000"), class = "opengwasr_local_unsupported")
})

test_that("og_local(inherit = local, bfile = ref) keeps the Store Releases and replaces the PLINK Reference", {
  skip_if_no_local()
  ref <- local_plink_reference()
  # The same fileset at another path, so the answer says which reference was read.
  dir <- withr::local_tempdir()
  file.copy(paste0(plink_fixture(), c(".bed", ".bim", ".fam")), dir)
  src <- dense_plink()
  other_src <- with_mocked_bindings(og_local(inherit = src, bfile = file.path(dir, "ref")),
                                    local_ogquery = function(...) stop("Store Releases were opened again"))
  other <- og_ld_resource(other_src)
  expect_equal(other$path, file.path(normalise_path(dir), "ref"))
  expect_identical(other_src$releases, src$releases)
  expect_identical(other_src$handles, src$handles)  # the releases as opened, not opened again
  m <- og_ld_matrix(other_src, c("rs5000", "rs1000"))
  expect_equal(og_provenance(m)$ld, other$path)
  expect_equal(og_provenance(m)$ld_from, "source")
  expect_equal(og_provenance(og_ld_matrix(src, c("rs5000", "rs1000")))$ld, ref$path)
  top <- suppressMessages(og_tophits(other_src, bmi, r2 = 0.001, kb = 10000))
  expect_equal(unique(top$clump_ld), other$path)
  expect_equal(og_provenance(top)$store_releases, "s/r")
  # The inherited Source is unchanged.
  expect_identical(og_ld_resource(src), ref)
  # Without an LD argument, the LD resource is inherited too; `bfile = NULL` drops it.
  expect_identical(og_ld_resource(og_local(inherit = other_src)), other)
  dropped <- og_local(inherit = other_src, bfile = NULL)
  expect_null(og_ld_resource(dropped))
  expect_identical(dropped$releases, src$releases)
})

test_that("an LD group given to og_local(inherit = ) replaces the inherited one, never mixes with it", {
  skip_if_no_local()
  # PLINK never runs here, so it is stood in for: this runs wherever Python does.
  local_mocked_bindings(find_plink = function(...) list(bin = "/stand-in/plink", version = "1.90b6.21"))
  src <- og_local(example_releases("dense"), bfile = plink_fixture())
  # Mixed would be `opengwasr_ld_resource_mixed`: this is refused as unavailable instead.
  e <- expect_error(og_local(inherit = src, ld_store = "/data/ld/EUR"), class = "opengwasr_ld_resource_unavailable")
  expect_false(inherits(e, "opengwasr_ld_resource_mixed"))
  expect_equal(e$resource, "ld_store")
  e <- expect_error(og_local(inherit = src, geno_store = "/data/geno/EUR"), class = "opengwasr_ld_resource_unavailable")
  expect_false(inherits(e, "opengwasr_ld_resource_mixed"))
  # The constructor's checks apply to what is given.
  e <- expect_error(og_local(inherit = src, bfile = file.path(tempdir(), "nothing")), class = "opengwasr_plink_reference_missing")
  expect_equal(e$arg, "bfile")
})

test_that("og_local(inherit = ) must be a Local Source", {
  api <- withr::with_envvar(c(OPENGWAS_URL = "http://example.test", OPENGWAS_DEV_KEY = "k"), og_api())
  e <- expect_error(og_local(inherit = api, bfile = plink_fixture()), class = "opengwasr_argument")
  expect_equal(e$arg, "inherit")
  expect_match(conditionMessage(e), "a Local Source", fixed = TRUE)
  expect_error(og_local(inherit = "dense"), class = "opengwasr_argument")
  # And the API Source's `inherit` refuses a Local Source.
  ld_only <- local_plink_source()
  e <- expect_error(og_api(inherit = ld_only), class = "opengwasr_argument")
  expect_equal(e$arg, "inherit")
})

test_that("an LD-only Local Source with Store Releases inherited into it, or given, is a full Local Source", {
  skip_if_no_local()
  ld_only <- local_plink_source()
  ref <- og_ld_resource(ld_only)
  full <- og_local(inherit = ld_only, gwas = example_releases("dense"))
  expect_identical(og_ld_resource(full), ref)
  expect_equal(full$releases$store_id, "s")
  top <- suppressMessages(og_tophits(full, bmi, r2 = 0.001, kb = 10000))
  expect_equal(top$rsid, c("rs5000", "rs5000", "rs1005", "rs1004"))
  # ALIDs are looked up in its Store Releases now.
  expect_equal(dim(og_ld_matrix(full, c("1:5000:C:G", "1:1000:A:G"))), c(2, 2))
  # Dropping the PLINK Reference from an LD-only Source leaves nothing.
  expect_error(og_local(inherit = ld_only, bfile = NULL), class = "opengwasr_argument")
})

test_that("og_ld_resource() gives a Local Source's PLINK Reference, or NULL without one", {
  skip_if_no_local()
  expect_null(og_ld_resource(dense_bare()))
  ref <- local_plink_reference()
  got <- og_ld_resource(dense_plink())
  expect_s3_class(got, "og_plink_reference")
  expect_identical(got, ref)
})

# An LD-only Local Source (#43) refuses every verb that reads Store Releases. PLINK never runs here,
# so it is stood in for, and these run everywhere: no Python, no PLINK.
test_that("an LD-only Local Source refuses every GWAS verb with one class, and needs no Python", {
  local_mocked_bindings(find_plink = function(...) list(bin = "/stand-in/plink", version = "1.90b6.21"),
                        local_ogquery = function(...) stop("Python was asked for"))
  ld_only <- og_local(bfile = plink_fixture())
  expect_equal(nrow(ld_only$releases), 0)
  expect_equal(og_ld_resource(ld_only)$plink_version, "1.90b6.21")
  verbs <- list(
    og_tophits = function() og_tophits(ld_only, bmi),
    og_tophits_unclumped = function() og_tophits(ld_only, bmi, clump = FALSE),
    og_tophits_settings = function() og_tophits(ld_only, bmi, r2 = 0.1, kb = 100),
    og_associations = function() og_associations(ld_only, "rs5000", bmi),
    og_analyses = function() og_analyses(ld_only),
    og_analyses_ids = function() og_analyses(ld_only, bmi),
    og_analyses_pinned = function() og_analyses(ld_only, releases = "s/r"),
    og_phewas = function() og_phewas(ld_only, "1:1000:A:G"),
    og_range = function() og_range(ld_only, bmi, chromosome = "1", start = 1, end = 10),
    og_variants_rsid = function() og_variants_rsid(ld_only, "rs5000"),
    og_variants_chrpos = function() og_variants_chrpos(ld_only, "1:1000")
  )
  for (name in names(verbs)) {
    e <- expect_error(verbs[[name]](), class = "opengwasr_gwas_store_required", info = name)
    expect_s3_class(e, "opengwasr_refusal")
    expect_null(e$alids)
    expect_equal(e$forms, "gwas")
    expect_equal(e$arg, "gwas")
    expect_match(conditionMessage(e), "og_local(gwas = , bfile = )", fixed = TRUE)
  }
  # What a Local Source refuses anyway, it still refuses as before.
  expect_error(og_ld_proxies(ld_only, "rs5000"), class = "opengwasr_local_unsupported")
  expect_error(og_status(ld_only), class = "opengwasr_local_unsupported")
})

# A Local Source with Store Releases, never opened (no Python), carrying `ref`.
held_with <- function(ref) {
  new_local_source(data.frame(store_id = "s", release_id = "r", format_version = "0.3.0", path = "/nowhere",
                              stringsAsFactors = FALSE), new.env(parent = emptyenv()), ref)
}

test_that("og_local(inherit = src, gwas = NULL) drops the Store Releases and keeps the LD: an LD-only Local Source", {
  local_mocked_bindings(find_plink = function(...) list(bin = "/stand-in/plink", version = "1.90b6.21"),
                        local_ogquery = function(...) stop("Python was asked for"))
  ref <- og_ld_resource(og_local(bfile = plink_fixture()))
  src <- held_with(ref)
  ld_only <- og_local(inherit = src, gwas = NULL)
  expect_equal(nrow(ld_only$releases), 0)
  expect_identical(og_ld_resource(ld_only), ref)
  expect_true("• Store Releases: none (LD only)" %in% format(ld_only))
  expect_error(og_tophits(ld_only, bmi), class = "opengwasr_gwas_store_required")
  # Dropping both leaves nothing.
  expect_error(og_local(inherit = src, gwas = NULL, bfile = NULL), class = "opengwasr_argument")
})

test_that("og_local(inherit = src, plink = ) keeps src's bfile and changes the PLINK that reads it", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  one <- fake_plink(dir, "plink", plink19_banner)
  two <- fake_plink(dir, "plink2", plink2_banner)
  src <- held_with(og_ld_resource(og_local(bfile = plink_fixture(), plink = one)))
  changed <- og_local(inherit = src, plink = two)
  ref <- og_ld_resource(changed)
  expect_equal(ref$path, plink_fixture())
  expect_equal(ref$plink_bin, two)
  expect_equal(ref$plink_version, "2.0.0-b.1.4")
  expect_identical(changed$releases, src$releases)
  expect_equal(og_ld_resource(src)$plink_bin, one)  # unchanged
  # The binary is checked as for any Local Source, naming `plink`.
  e <- expect_error(og_local(inherit = src, plink = file.path(dir, "nothing")), class = "opengwasr_plink_not_found")
  expect_equal(e$arg, "plink")
  # With `bfile` too, both are the given ones.
  other <- withr::local_tempdir()
  file.copy(paste0(plink_fixture(), c(".bed", ".bim", ".fam")), other)
  both <- og_ld_resource(og_local(inherit = src, plink = two, bfile = file.path(other, "ref")))
  expect_equal(both$path, file.path(normalise_path(other), "ref"))
  # Any other LD argument still replaces the LD resource whole.
  expect_error(og_local(inherit = src, plink = two, ld_store = "/l"), class = "opengwasr_ld_resource_mixed")
  expect_error(og_local(inherit = src, ld_store = "/l"), class = "opengwasr_ld_resource_unavailable")
  expect_null(og_ld_resource(og_local(inherit = src, bfile = NULL)))
  # With no PLINK Reference to keep, `plink` alone is refused as before.
  bare <- held_with(NULL)
  e <- expect_error(og_local(inherit = bare, plink = two), class = "opengwasr_argument")
  expect_equal(e$arg, "bfile")
})

test_that("og_local_check() on an LD-only Local Source loads no Python, reports its PLINK Reference, and no Store Releases", {
  local_mocked_bindings(find_plink = function(...) list(bin = "/stand-in/plink", version = "1.90b6.21"),
                        local_ogquery = function(...) stop("Python was asked for"),
                        local_py_started = function() stop("Python was asked for"))
  ld_only <- og_local(bfile = plink_fixture())
  check <- og_local_check(ld_only)
  expect_s3_class(check, "og_local_check")
  expect_identical(check$ld, og_ld_resource(ld_only))
  expect_equal(nrow(check$releases), 0)
  expect_true(is.na(check$python))
  expect_null(check$unavailable)
  expect_equal(check$problems, character())
  shown <- format(check)
  expect_false(any(grepl("Store Releases", shown, fixed = TRUE)))
  expect_false(any(grepl("^Python", shown)))
  expect_true(paste0("PLINK Reference: ", plink_fixture()) %in% shown)
  expect_true("  PLINK 1.90b6.21: /stand-in/plink" %in% shown)
})

test_that("og_local_check() on a Local Source with Store Releases and a PLINK Reference reports both", {
  skip_if_no_local()
  local_mocked_bindings(find_plink = function(...) list(bin = "/stand-in/plink", version = "1.90b6.21"))
  src <- og_local(example_releases("dense"), bfile = plink_fixture())
  check <- og_local_check(src)
  expect_identical(check$ld, og_ld_resource(src))
  expect_equal(check$releases$store_id, "s")
  shown <- format(check)
  expect_true("Store Releases:" %in% shown)
  expect_true(paste0("PLINK Reference: ", plink_fixture()) %in% shown)
  # Given paths, there is no LD resource to report.
  expect_null(og_local_check(example_releases("dense"))$ld)
})
