# PLINK References in tests: the committed fileset, and expectations worked out without PLINK.
#
# `inst/plink/ref.{bed,bim,fam}` is synthetic (make-reference.R there: 300 people, 36 variants), and
# comes with the package, so the Local Source vignette reads it too. Every expected number comes from
# decoding the `.bed` here, in plain R, never from the code under test or from PLINK's own output.

plink_fixture <- function() {
  dir <- system.file("plink", package = "opengwasr", mustWork = TRUE)
  file.path(normalise_path(dir), "ref")  # as og_plink_reference() normalises it
}

# A PLINK Reference over the fixture, or skip the test: nothing here runs without a PLINK binary.
local_plink_reference <- function() {
  ref <- tryCatch(og_plink_reference(plink_fixture()), opengwasr_plink_not_found = function(e) NULL)
  if (is.null(ref)) testthat::skip("No PLINK binary found (genetics.binaRies, or plink on the PATH).")
  ref
}

# The fixture's genotypes as A1-allele counts (a people x variants matrix named by rsID), read from
# the `.bed` by its documented layout: 3 magic bytes, then per variant 2 bits a person, low bits
# first: 00 two A1, 01 missing, 10 one A1, 11 no A1.
fixture_dosage <- function() {
  bim <- utils::read.table(paste0(plink_fixture(), ".bim"), stringsAsFactors = FALSE)
  n <- length(readLines(paste0(plink_fixture(), ".fam")))
  bytes <- readBin(paste0(plink_fixture(), ".bed"), "raw", file.size(paste0(plink_fixture(), ".bed")))
  stopifnot(identical(bytes[1:3], as.raw(c(0x6c, 0x1b, 0x01))))
  per <- ceiling(n / 4)
  counts <- c(2, NA, 1, 0)
  d <- vapply(seq_len(nrow(bim)), function(v) {
    b <- as.integer(bytes[3 + (v - 1) * per + seq_len(per)])
    codes <- as.vector(vapply(b, function(x) c(x %% 4, (x %/% 4) %% 4, (x %/% 16) %% 4, (x %/% 64) %% 4), numeric(4)))
    counts[codes[seq_len(n)] + 1]
  }, numeric(n))
  colnames(d) <- bim$V2
  d
}

fixture_bim <- function() {
  bim <- utils::read.table(paste0(plink_fixture(), ".bim"), stringsAsFactors = FALSE,
                           col.names = c("chr", "rsid", "cm", "bp", "a1", "a2"))
  bim
}

# PLINK's clumping rule, greedy and in plain R: the strongest score first, each index variant takes
# every unclumped variant less than `kb` away on its chromosome (PLINK 1.90 leaves out one exactly `kb` away) whose r^2 with it is above `r2`. Returns the
# index variants (strongest first) and the size of each clump, itself included. r² here is the
# squared genotype correlation; PLINK 1.9 and 2 clump by r² from haplotype frequencies, which is close
# but not equal (test-plink-versions.R has a pair either side of 0.2), so tests using this rule keep
# their settings away from such pairs.
expected_clump <- function(rsids, score, r2, kb) {
  d <- fixture_dosage()
  bim <- fixture_bim()
  r_squared <- stats::cor(d[, rsids, drop = FALSE])^2
  bp <- bim$bp[match(rsids, bim$rsid)]
  chr <- bim$chr[match(rsids, bim$rsid)]
  taken <- rep(FALSE, length(rsids))
  lead <- character()
  size <- integer()
  for (i in order(-score)) {
    if (taken[i]) next
    members <- which(!taken & chr == chr[i] & abs(bp - bp[i]) < kb * 1000 & r_squared[i, ] > r2)
    taken[c(i, members)] <- TRUE
    lead <- c(lead, rsids[i])
    size <- c(size, length(unique(c(i, members))))
  }
  data.frame(lead = lead, size = size, stringsAsFactors = FALSE)
}

# A stand-in PLINK (a shell script; not on Windows) named `name` in `dir`: `--version` prints
# `banner` (and appends a line to `<dir>/<name>.calls`), anything else fails.
fake_plink <- function(dir, name, banner) {
  bin <- file.path(dir, name)
  writeLines(c("#!/bin/sh",
               "if [ \"$1\" = \"--version\" ]; then",
               paste0("  echo x >> '", bin, ".calls'"),
               if (length(banner)) paste0("  echo '", banner, "'"),
               "  exit 0",
               "fi",
               "exit 3"), bin)
  Sys.chmod(bin, "0755")
  normalise_path(bin)
}

plink19_banner <- "PLINK v1.90b6.21 64-bit (19 Oct 2020)"
plink2_banner <- "PLINK v2.0.0-b.1.4 64-bit (7 Nov 2026)"

# A PLINK Reference over the fixture for each supported PLINK this machine has, by series ("1.9",
# "2"), or NULL for one it hasn't. They come from OPENGWASR_TEST_PLINK1 and OPENGWASR_TEST_PLINK2
# (CI sets the second), the PLINK opengwasr finds, and `plink` and `plink2` on the PATH.
plink_references_by_series <- function() {
  found <- tryCatch(find_plink(NULL), error = function(e) "")
  bins <- c(Sys.getenv(c("OPENGWASR_TEST_PLINK1", "OPENGWASR_TEST_PLINK2")), found, unname(Sys.which(c("plink", "plink2"))))
  bins <- unique(bins[nzchar(bins)])
  refs <- Filter(Negate(is.null), lapply(bins, function(b) {
    tryCatch(og_plink_reference(plink_fixture(), plink_bin = b), error = function(e) NULL)
  }))
  series <- vapply(refs, function(r) plink_series(r$plink_version), "")
  list("1.9" = refs[series == "1.9"][1][[1]], "2" = refs[series == "2"][1][[1]])
}
