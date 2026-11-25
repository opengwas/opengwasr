# PLINK 1.9 and PLINK 2 (#41): the binary named by path or command, the automatic search, the
# version read once and kept, other versions refused, and the two giving the same answer. The
# stand-in PLINKs are shell scripts, so those tests skip on Windows; the equivalence test needs both
# real binaries (CI's ubuntu release job finds PLINK 2, and OPENGWASR_TEST_PLINK1 names PLINK 1.9).

withr::local_options(opengwasr.progress = FALSE)

test_that("`plink_bin` is a path or a command on the PATH, and the version it reports is kept", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  bin <- fake_plink(dir, "plink2", plink2_banner)
  withr::local_envvar(PATH = paste(dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
  ref <- og_plink_reference(plink_fixture(), plink_bin = "plink2")
  expect_s3_class(ref, "og_plink_reference")
  expect_equal(ref$plink_bin, bin)
  expect_equal(ref$plink_version, "2.0.0-b.1.4")
  expect_equal(og_plink_reference(plink_fixture(), plink_bin = bin)$plink_bin, bin)
  expect_true(paste0("  PLINK 2.0.0-b.1.4: ", bin) %in% format(ref))
  # A command that isn't on the PATH, a directory, or nothing executable: not found, naming the argument.
  e <- expect_error(og_plink_reference(plink_fixture(), plink_bin = "no-such-plink-here"), class = "opengwasr_plink_not_found")
  expect_s3_class(e, "opengwasr_argument")
  expect_equal(e$arg, "plink_bin")
  expect_equal(expect_error(og_plink_reference(plink_fixture(), plink_bin = dir), class = "opengwasr_plink_not_found")$arg, "plink_bin")
  # og_local() names its own argument.
  e <- expect_error(og_local(example_releases("dense"), bfile = plink_fixture(), plink = "no-such-plink-here"),
                    class = "opengwasr_plink_not_found")
  expect_equal(e$arg, "plink")
})

test_that("og_local(plink = <command>) holds the PLINK Reference og_plink_reference() builds", {
  skip_on_os("windows")
  skip_if_no_local()
  dir <- withr::local_tempdir()
  bin <- fake_plink(dir, "plink2", plink2_banner)
  withr::local_envvar(PATH = paste(dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
  src <- og_local(gwas = example_releases("dense"), plink = "plink2", bfile = plink_fixture())
  expect_equal(src$ld$plink_bin, bin)
  expect_equal(src$ld$plink_version, "2.0.0-b.1.4")
  expect_true(paste0("  PLINK 2.0.0-b.1.4: ", bin) %in% format(src))
})

test_that("a name without a path separator is a command, even with a file or directory of that name here", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  bin <- fake_plink(dir, "plink", plink19_banner)
  withr::local_envvar(PATH = dir)  # only the stand-in
  here <- withr::local_tempdir()
  withr::local_dir(here)
  dir.create("plink")  # a `plink/` directory in the working directory
  expect_equal(og_plink_reference(plink_fixture(), plink_bin = "plink")$plink_bin, bin)
  # With a separator it is a path: here, the directory, which isn't PLINK.
  e <- expect_error(og_plink_reference(plink_fixture(), plink_bin = "./plink"), class = "opengwasr_plink_not_found")
  expect_equal(e$arg, "plink_bin")
  # A file named like the command, here, doesn't win over the PATH either.
  unlink("plink", recursive = TRUE)
  fake_plink(here, "plink2", plink2_banner)
  e <- expect_error(og_plink_reference(plink_fixture(), plink_bin = "plink2"), class = "opengwasr_plink_not_found")
  expect_equal(og_plink_reference(plink_fixture(), plink_bin = "./plink2")$plink_version, "2.0.0-b.1.4")
})

test_that("with nothing given, PLINK is the first supported of genetics.binaRies', `plink`, then `plink2` on the PATH", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  withr::local_envvar(PATH = dir)  # only the stand-ins
  local_mocked_bindings(packaged_plink = function() NULL)
  e <- expect_error(og_plink_reference(plink_fixture()), class = "opengwasr_plink_not_found")
  expect_equal(nrow(e$skipped), 0)
  two <- fake_plink(dir, "plink2", plink2_banner)
  expect_equal(og_plink_reference(plink_fixture())$plink_bin, two)
  one <- fake_plink(dir, "plink", plink19_banner)
  ref <- og_plink_reference(plink_fixture())
  expect_equal(ref$plink_bin, one)
  expect_equal(ref$plink_version, "1.90b6.21")
  packaged <- fake_plink(withr::local_tempdir(), "plink", plink19_banner)
  local_mocked_bindings(packaged_plink = function() packaged)
  expect_equal(og_plink_reference(plink_fixture())$plink_bin, packaged)
})

test_that("the search skips a `plink` that isn't a supported PLINK, and lists what it skipped when none is", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  withr::local_envvar(PATH = dir)
  # PuTTY's plink (putty-tools) reports no PLINK version; genetics.binaRies here has PLINK 1.07.
  putty <- fake_plink(dir, "plink", "plink: Release 0.81")
  old <- fake_plink(withr::local_tempdir(), "plink", c("@-------@", "|        PLINK!       |     v1.07      |   10/Aug/2009     |"))
  local_mocked_bindings(packaged_plink = function() old)
  two <- fake_plink(dir, "plink2", plink2_banner)
  ref <- og_plink_reference(plink_fixture())
  expect_equal(ref$plink_bin, two)
  expect_equal(ref$plink_version, "2.0.0-b.1.4")
  # A PLINK 2 build before 2.0.0-a.6.0 is skipped too: none left.
  fake_plink(dir, "plink2", "PLINK v2.00a5.12LM 64-bit Intel (25 Jun 2024)")
  e <- expect_error(og_plink_reference(plink_fixture()), class = "opengwasr_plink_not_found")
  expect_false(inherits(e, "opengwasr_argument"))
  expect_equal(e$skipped$plink_bin, c(old, putty, two))
  expect_equal(e$skipped$version, c("1.07", NA, "2.00a5.12LM"))
  expect_equal(e$supported, c("1.9", "2 (2.0.0-a.6.0 or later)"))
})

test_that("the version is read once, when the PLINK Reference is made, not by each query", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  bin <- fake_plink(dir, "plink", plink19_banner)
  ref <- og_plink_reference(plink_fixture(), plink_bin = bin)
  expect_length(readLines(paste0(bin, ".calls")), 1)
  # The stand-in fails anything but --version: PLINK ran, and --version didn't run again.
  expect_error(og_ld_matrix(NULL, c("rs4977574", "rs2891168"), ld = ref), class = "opengwasr_plink_failed")
  expect_length(readLines(paste0(bin, ".calls")), 1)
})

test_that("a PLINK Reference made without a version (before #41) has it read on first use, or refused", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  bin <- fake_plink(dir, "plink", plink19_banner)
  ref <- structure(list(path = plink_fixture(), plink_bin = bin), class = "og_plink_reference")
  expect_equal(plink_dialect(ref)$name, "1.9")
  # Through a query: read once, then PLINK runs (the stand-in fails it).
  expect_error(og_ld_matrix(NULL, c("rs4977574", "rs2891168"), ld = ref), class = "opengwasr_plink_failed")
  expect_length(readLines(paste0(bin, ".calls")), 2)
  ref$plink_bin <- fake_plink(dir, "old", "PLINK v1.07")
  e <- expect_error(og_ld_matrix(NULL, c("rs4977574", "rs2891168"), ld = ref), class = "opengwasr_plink_unsupported")
  expect_equal(e$version, "1.07")
  expect_error(plink_dialect(ref), class = "opengwasr_plink_unsupported")
})

test_that("a PLINK named that isn't a supported one is refused, suggesting a supported one on the PATH", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  withr::local_envvar(PATH = dir)
  local_mocked_bindings(packaged_plink = function() NULL)
  cases <- list(list(name = "plink107", banner = c("@----------------------------------------------------------@",
                                                   "|        PLINK!       |     v1.07      |   10/Aug/2009     |"),
                     version = "1.07"),
                list(name = "plink3", banner = "PLINK v3.0.0 64-bit (1 Jan 2030)", version = "3.0.0"),
                list(name = "plink18", banner = "PLINK v1.80 64-bit", version = "1.80"),
                list(name = "plink2a3", banner = "PLINK v2.00a3.7LM 64-bit Intel (24 Oct 2022)", version = "2.00a3.7LM"),
                list(name = "plink2a6dev", banner = "PLINK v2.00a6LM 64-bit Intel (5 Jan 2024)", version = "2.00a6LM"),
                list(name = "putty", banner = "plink: Release 0.81", version = NA_character_),
                list(name = "silent", banner = character(), version = NA_character_))
  for (case in cases) {
    bin <- fake_plink(dir, case$name, case$banner)
    e <- expect_error(og_plink_reference(plink_fixture(), plink_bin = bin), class = "opengwasr_plink_unsupported")
    expect_equal(e$version, case$version)
    expect_equal(e$supported, c("1.9", "2 (2.0.0-a.6.0 or later)"))
    expect_equal(e$arg, "plink_bin")
    expect_null(e$suggest)
  }
  # With a supported plink2 on the PATH, the refusal suggests it.
  fake_plink(dir, "plink2", plink2_banner)
  e <- expect_error(og_plink_reference(plink_fixture(), plink_bin = file.path(dir, "plink107")), class = "opengwasr_plink_unsupported")
  expect_equal(e$suggest, "plink2")
  # Through og_local(), the argument is `plink`.
  e <- expect_error(og_local(example_releases("dense"), bfile = plink_fixture(), plink = file.path(dir, "plink3")),
                    class = "opengwasr_plink_unsupported")
  expect_equal(e$arg, "plink")
  expect_equal(e$suggest, "plink2")
})

test_that("PLINK 2's semantic versions are compared from 2.0.0-a.6.0", {
  supported <- c("2.0.0-a.6.0", "2.0.0-a.6.9LM", "2.0.0-a.7.8", "2.0.0-b.1.4", "2.0.0", "2.0.1-a.1.0", "1.90b6.21", "1.90p", "1.9.0-b.7.7")
  refused <- c("2.00a5.12LM", "2.00a6LM", "2.00a3.7", "2.0.0-a.5.10LM", "2.1.0", "1.80", "1.07", NA)
  for (v in supported) expect_false(is.null(plink_dialect_of(v)), label = v)
  for (v in refused) expect_null(plink_dialect_of(v), label = v)
})

test_that("the found PLINK's version is on the PLINK Reference and in provenance", {
  ref <- local_plink_reference()
  expect_false(is.null(plink_dialect_of(ref$plink_version)))
  m <- og_ld_matrix(NULL, c("rs4977574", "rs2891168"), ld = ref)
  expect_equal(og_provenance(m)$plink_version, ref$plink_version)
  expect_equal(og_provenance(m)$plink_bin, ref$plink_bin)
  data <- data.frame(alid = c("9:22098575:A:G", "9:22098620:A:G"), rsid = c("rs4977574", "rs2891168"), z = c(9, 6), id = "x")
  out <- og_clump(NULL, data, ld = ref, r2 = 0.2, kb = 100)
  expect_equal(og_provenance(out)$plink_version, ref$plink_version)
})

# Equivalence on the committed fileset. r agrees to 1e-6: both write it to 6 significant digits, and
# on this fileset the two files are identical. Clumps (index variants and their sizes) are identical,
# and each PLINK's equal what PLINK's rule gives with r² from haplotype frequencies
# (expected_clump()), worked out in R.
test_that("PLINK 1.9 and PLINK 2 each give PLINK's clumps, and the same LD matrix, on the committed fileset", {
  refs <- plink_references_by_series()
  refs <- Filter(Negate(is.null), refs)
  skip_if(length(refs) == 0, "No PLINK binary found.")
  bim <- fixture_bim()
  rsids <- bim$rsid
  # Every variant of the fileset, in three orders of strength, clumped at settings either side of
  # the fileset's r² and windows (0.045 and 0.046 kb straddle rs4977574 and rs2891168, 45 bp apart).
  orders <- list(seq_along(rsids), rev(seq_along(rsids)), c(14, 31, 3, 25, 9, 36, 20, 1, 17, 28, 6, 33, 11, 23, 2,
                                                            35, 15, 7, 30, 19, 26, 4, 12, 34, 22, 8, 29, 16, 5,
                                                            24, 32, 10, 27, 18, 13, 21))
  alids <- paste(bim$chr, bim$bp, pmin(bim$a1, bim$a2), pmax(bim$a1, bim$a2), sep = ":")
  for (strength in orders) {
    data <- data.frame(alid = alids, rsid = rsids, z = strength, id = "x", stringsAsFactors = FALSE)
    for (r2 in c(0.001, 0.2, 0.99)) for (kb in c(0.045, 0.046, 10000)) {
      want <- expected_clump(rsids, strength, r2, kb)
      for (ref in refs) {
        out <- og_clump(NULL, data, ld = ref, r2 = r2, kb = kb)
        what <- paste("PLINK", ref$plink_version, "at r2", r2, "kb", kb)
        expect_equal(out$rsid, rsids[rsids %in% want$lead], label = paste("index variants,", what))
        expect_equal(out$clump_size, want$size[match(out$rsid, want$lead)], label = paste("clump sizes,", what))
      }
    }
  }
  skip_if(length(refs) < 2, "Needs both PLINK 1.9 and PLINK 2 (OPENGWASR_TEST_PLINK1, OPENGWASR_TEST_PLINK2, or on the PATH).")
  m1 <- og_ld_matrix(NULL, rsids, ld = refs[["1.9"]])
  m2 <- og_ld_matrix(NULL, rsids, ld = refs[["2"]])
  expect_equal(dimnames(m2), dimnames(m1))
  expect_equal(dim(m1), c(length(rsids), length(rsids)))
  expect_equal(is.na(unclass(m2)), is.na(unclass(m1)), ignore_attr = TRUE)
  expect_lte(max(abs(unclass(m2) - unclass(m1)), na.rm = TRUE), 1e-6)
  expect_equal(og_provenance(m2)$plink_version, refs[["2"]]$plink_version)
})

test_that("clumping uses r² from haplotype frequencies, which decides a pair either side of the threshold", {
  # rs4977574 and rs9002: squared genotype correlation 0.202, r² from haplotype frequencies 0.167. At
  # r2 = 0.2 the first would put rs9002 in rs4977574's clump; PLINK, by the second, doesn't.
  d <- fixture_dosage()
  expect_gt(stats::cor(d[, "rs4977574"], d[, "rs9002"])^2, 0.2)
  expect_lt(haplotype_r2(d[, "rs4977574"], d[, "rs9002"]), 0.2)
  expect_equal(haplotype_r2(d[, "rs4977574"], d[, "rs9002"]), 0.167305, tolerance = 1e-5)  # PLINK's --r2-phased
  want <- expected_clump(c("rs4977574", "rs9002"), c(9, 3), 0.2, 100)
  expect_equal(want$lead, c("rs4977574", "rs9002"))
  refs <- Filter(Negate(is.null), plink_references_by_series())
  skip_if(length(refs) == 0, "No PLINK binary found.")
  data <- data.frame(alid = c("9:22098575:A:G", "9:22110000:A:G"), rsid = c("rs4977574", "rs9002"), z = c(9, 3), id = "x")
  for (ref in refs) {
    out <- og_clump(NULL, data, ld = ref, r2 = 0.2, kb = 100)
    expect_equal(out$rsid, want$lead)
    expect_equal(out$clump_size, want$size)
  }
})
