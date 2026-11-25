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
  if (is.null(ref)) testthat::skip("No PLINK binary found (genetics.binaRies, or plink or plink2 on the PATH).")
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

# r² between two variants as PLINK's --clump computes it (1.9 and 2 alike): from haplotype
# frequencies estimated by EM from unphased genotypes, not the squared genotype correlation
# (og_ld_matrix()'s r is that). `x` and `y` count the first allele; people missing either are left out.
# Of each pair of genotypes the haplotypes are known except in double heterozygotes, which are split
# between AB/ab and Ab/aB in proportion to the current frequencies until they settle.
haplotype_r2 <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]
  y <- y[keep]
  n <- table(factor(x, 0:2), factor(y, 0:2))  # n[i + 1, j + 1]: people with x = i, y = j
  # Known haplotype counts: AB, Ab, aB, ab (A, B the counted alleles).
  known <- c(AB = 2 * n[3, 3] + n[3, 2] + n[2, 3],
             Ab = 2 * n[3, 1] + n[3, 2] + n[2, 1],
             aB = 2 * n[1, 3] + n[1, 2] + n[2, 3],
             ab = 2 * n[1, 1] + n[1, 2] + n[2, 1])
  dh <- n[2, 2]
  total <- 2 * length(x)
  h <- (known + dh / 2) / total
  for (i in 1:1000) {
    cis <- if (h[["AB"]] * h[["ab"]] + h[["Ab"]] * h[["aB"]] > 0) h[["AB"]] * h[["ab"]] / (h[["AB"]] * h[["ab"]] + h[["Ab"]] * h[["aB"]]) else 0.5
    nxt <- (known + dh * c(cis, 1 - cis, 1 - cis, cis)) / total
    if (max(abs(nxt - h)) < 1e-12) break
    h <- nxt
  }
  p <- h[["AB"]] + h[["Ab"]]
  q <- h[["AB"]] + h[["aB"]]
  if (p * (1 - p) * q * (1 - q) == 0) return(NA_real_)
  (h[["AB"]] - p * q)^2 / (p * (1 - p) * q * (1 - q))
}

# haplotype_r2() of every pair of the fixture's variants on one chromosome (NA across chromosomes),
# named by rsID: worked out once per test process.
fixture_haplotype_r2 <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    d <- fixture_dosage()
    chr <- fixture_bim()$chr
    n <- ncol(d)
    r2 <- matrix(NA_real_, n, n, dimnames = list(colnames(d), colnames(d)))
    for (i in seq_len(n)) for (j in seq_len(i)) {
      if (chr[[i]] == chr[[j]]) r2[i, j] <- r2[j, i] <- if (i == j) 1 else haplotype_r2(d[, i], d[, j])
    }
    cached <<- r2
    r2
  }
})

# PLINK's clumping rule, greedy and in plain R: the strongest score first, each index variant takes
# every unclumped variant less than `kb` away on its chromosome (PLINK leaves out one exactly `kb`
# away) whose r² with it (haplotype_r2(), as PLINK's) is above `r2`. Returns the index variants
# (strongest first) and the size of each clump, itself included.
expected_clump <- function(rsids, score, r2, kb) {
  bim <- fixture_bim()
  r_squared <- fixture_haplotype_r2()[rsids, rsids, drop = FALSE]
  bp <- bim$bp[match(rsids, bim$rsid)]
  chr <- bim$chr[match(rsids, bim$rsid)]
  taken <- rep(FALSE, length(rsids))
  lead <- character()
  size <- integer()
  for (i in order(-score)) {
    if (taken[i]) next
    members <- which(!taken & chr == chr[i] & abs(bp - bp[i]) < kb * 1000 & !is.na(r_squared[i, ]) & r_squared[i, ] > r2)
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
  series <- vapply(refs, function(r) plink_dialect_of(r$plink_version)$name, "")
  list("1.9" = refs[series == "1.9"][1][[1]], "2" = refs[series == "2"][1][[1]])
}
