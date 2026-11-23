# PLINK References in tests: the committed fileset, and expectations worked out without PLINK.
#
# `fixtures/plink/ref.{bed,bim,fam}` is synthetic (make-reference.R there: 300 people, 31 variants).
# Every expected number comes from decoding the `.bed` here, in plain R, never from the code under
# test or from PLINK's own output.

plink_fixture <- function() file.path(normalizePath(test_path("fixtures", "plink")), "ref")

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
# index variants (strongest first) and the size of each clump, itself included.
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
