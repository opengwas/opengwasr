# PLINK 1.9 and PLINK 2 (#41): the binary named by path or command, the automatic search, the
# version read once and kept, other versions refused, and the two giving the same answer. The
# stand-in PLINKs are shell scripts, so those tests skip on Windows; the equivalence test needs both
# real binaries (CI's ubuntu release job points OPENGWASR_TEST_PLINK2 at PLINK 2).

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

test_that("with nothing given, PLINK is genetics.binaRies', then `plink`, then `plink2` on the PATH", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  withr::local_envvar(PATH = dir)  # only the stand-ins
  local_mocked_bindings(packaged_plink = function() NULL)
  expect_error(og_plink_reference(plink_fixture()), class = "opengwasr_plink_not_found")
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

test_that("any PLINK but 1.9 or 2, or a program that reports no PLINK version, is refused", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  cases <- list(list(name = "plink107", banner = c("@----------------------------------------------------------@",
                                                   "|        PLINK!       |     v1.07      |   10/Aug/2009     |"),
                     version = "1.07"),
                list(name = "plink3", banner = "PLINK v3.0.0 64-bit (1 Jan 2030)", version = "3.0.0"),
                list(name = "plink18", banner = "PLINK v1.80 64-bit", version = "1.80"),
                list(name = "notplink", banner = "hello", version = NA_character_),
                list(name = "silent", banner = character(), version = NA_character_))
  for (case in cases) {
    bin <- file.path(dir, case$name)
    writeLines(c("#!/bin/sh", paste0("echo '", case$banner, "'"), "exit 0"), bin)
    Sys.chmod(bin, "0755")
    e <- expect_error(og_plink_reference(plink_fixture(), plink_bin = bin), class = "opengwasr_plink_unsupported")
    expect_equal(e$version, case$version)
    expect_equal(e$supported, c("1.9", "2"))
    expect_equal(e$arg, "plink_bin")
  }
  # Through og_local(), the argument is `plink`.
  e <- expect_error(og_local(example_releases("dense"), bfile = plink_fixture(), plink = file.path(dir, "plink3")),
                    class = "opengwasr_plink_unsupported")
  expect_equal(e$arg, "plink")
})

test_that("the found PLINK's version is on the PLINK Reference and in provenance", {
  ref <- local_plink_reference()
  expect_true(plink_series(ref$plink_version) %in% c("1.9", "2"))
  m <- og_ld_matrix(NULL, c("rs4977574", "rs2891168"), ld = ref)
  expect_equal(og_provenance(m)$plink_version, ref$plink_version)
  expect_equal(og_provenance(m)$plink_bin, ref$plink_bin)
  data <- data.frame(alid = c("9:22098575:A:G", "9:22098620:A:G"), rsid = c("rs4977574", "rs2891168"), z = c(9, 6), id = "x")
  out <- og_clump(NULL, data, ld = ref, r2 = 0.2, kb = 100)
  expect_equal(og_provenance(out)$plink_version, ref$plink_version)
})

# Equivalence on the committed fileset. r agrees to 1e-6: both write it to 6 significant digits, and
# on this fileset the two files are identical. Clumps (index variants and their sizes) are identical.
test_that("PLINK 2 gives PLINK 1.9's LD matrix and clumps on the committed fileset", {
  refs <- plink_references_by_series()
  skip_if(is.null(refs[["1.9"]]) || is.null(refs[["2"]]),
          "Needs both PLINK 1.9 and PLINK 2 (OPENGWASR_TEST_PLINK1, OPENGWASR_TEST_PLINK2, or on the PATH).")
  bim <- fixture_bim()
  rsids <- bim$rsid
  m1 <- og_ld_matrix(NULL, rsids, ld = refs[["1.9"]])
  m2 <- og_ld_matrix(NULL, rsids, ld = refs[["2"]])
  expect_equal(dimnames(m2), dimnames(m1))
  expect_equal(dim(m1), c(length(rsids), length(rsids)))
  expect_equal(is.na(unclass(m2)), is.na(unclass(m1)), ignore_attr = TRUE)
  expect_lte(max(abs(unclass(m2) - unclass(m1)), na.rm = TRUE), 1e-6)
  expect_equal(og_provenance(m2)$plink_version, refs[["2"]]$plink_version)

  # Every variant of the fileset, in three orders of strength, clumped at settings either side of
  # the fileset's r² and windows (0.045 and 0.046 kb straddle rs4977574 and rs2891168, 45 bp apart).
  orders <- list(seq_along(rsids), rev(seq_along(rsids)), c(14, 31, 3, 25, 9, 36, 20, 1, 17, 28, 6, 33, 11, 23, 2,
                                                            35, 15, 7, 30, 19, 26, 4, 12, 34, 22, 8, 29, 16, 5,
                                                            24, 32, 10, 27, 18, 13, 21))
  for (strength in orders) {
    data <- data.frame(alid = paste(bim$chr, bim$bp, pmin(bim$a1, bim$a2), pmax(bim$a1, bim$a2), sep = ":"), rsid = rsids, z = strength, id = "x", stringsAsFactors = FALSE)
    for (r2 in c(0.001, 0.1, 0.2, 0.5, 0.99)) for (kb in c(0.045, 0.046, 100, 10000)) {
      c1 <- og_clump(NULL, data, ld = refs[["1.9"]], r2 = r2, kb = kb)
      c2 <- og_clump(NULL, data, ld = refs[["2"]], r2 = r2, kb = kb)
      expect_equal(c2$rsid, c1$rsid, label = paste("index variants at r2", r2, "kb", kb))
      expect_equal(c2$clump_size, c1$clump_size, label = paste("clump sizes at r2", r2, "kb", kb))
    }
  }
  # Where the r² PLINK 2 clumps by matters: rs4977574 and rs9002's squared genotype correlation is
  # 0.202, their r² from haplotype frequencies (1.9's) 0.167. At r2 = 0.2, both PLINKs leave rs9002
  # to lead a clump of its own.
  data <- data.frame(alid = c("9:22098575:A:G", "9:22110000:A:G"), rsid = c("rs4977574", "rs9002"), z = c(9, 3), id = "x")
  expect_gt(stats::cor(fixture_dosage()[, "rs4977574"], fixture_dosage()[, "rs9002"])^2, 0.2)
  for (ref in refs) {
    out <- og_clump(NULL, data, ld = ref, r2 = 0.2, kb = 100)
    expect_equal(out$rsid, c("rs4977574", "rs9002"))
    expect_equal(out$clump_size, c(1L, 1L))
  }
})
