# Generates the committed PLINK Reference `ref.{bed,bim,fam}` (synthetic: no real people).
#
#   R_LIBS_USER=<library with genetics.binaRies> Rscript tests/testthat/fixtures/plink/make-reference.R
#
# 300 individuals, 31 variants. Genotypes come from haplotypes drawn with a shared latent factor per
# LD block, so each block's variants are in LD with each other and blocks are independent. The
# rsIDs of the fake Data API's coronary artery disease and BMI top hits (R/tests fake-server.R) are
# among the variants, in two blocks on chromosomes 9 and 16; rs10738610 is deliberately left out, so
# a top hit the reference lacks exists. Tests never read this script's numbers: they decode the
# `.bed` themselves (helper-plink.R) and compute r and the clumps in plain R.
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))
plink <- genetics.binaRies::get_plink_binary()
set.seed(20261123)
n <- 300

variant <- function(rsid, chr, bp, a1, a2, block, rho, maf) {
  data.frame(rsid = rsid, chr = chr, bp = bp, a1 = a1, a2 = a2, block = block, rho = rho, maf = maf,
             stringsAsFactors = FALSE)
}
v <- rbind(
  # chromosome 9, one tight block around the CAD hits, then a looser neighbour block
  variant("rs4977574",  9, 22098575, "G", "A", "A", 0.90, 0.45),
  variant("rs2891168",  9, 22098620, "G", "A", "A", 0.90, 0.48),
  variant("rs9001",     9, 22101000, "T", "C", "A", 0.80, 0.40),
  variant("rs10757275", 9, 22106226, "G", "A", "A", 0.70, 0.42),
  variant("rs9002",     9, 22110000, "A", "G", "A", 0.55, 0.30),
  variant("rs1333049",  9, 22115503, "C", "G", "B", 0.80, 0.47),
  variant("rs9003",     9, 22116000, "T", "A", "B", 0.80, 0.35),
  variant("rs9004",     9, 22300000, "A", "C", "C", 0.00, 0.30),
  # chromosome 9, far away: independent
  variant(sprintf("rs91%02d", 1:10), 9, 30000000 + (1:10) * 500000, "A", "G", paste0("far", 1:10), 0, 0.3),
  # chromosome 16
  variant("rs1421085",  16, 53767042, "C", "T", "D", 0.85, 0.40),
  variant("rs1558902",  16, 53767100, "T", "A", "D", 0.85, 0.40),
  variant("rs9939609",  16, 53800000, "A", "T", "D", 0.60, 0.35),
  variant(sprintf("rs16%02d", 1:10), 16, 60000000 + (1:10) * 200000, "G", "C", paste0("far16", 1:10), 0, 0.3)
)
stopifnot(nrow(v) == 31, !anyDuplicated(v$rsid))

latent <- list()
haplotype <- function(v) {
  if (is.null(latent[[v$block]])) latent[[v$block]] <<- matrix(rnorm(2 * n), ncol = 2)
  f <- latent[[v$block]]
  z <- sqrt(v$rho) * f + sqrt(1 - v$rho) * matrix(rnorm(2 * n), ncol = 2)
  (z < qnorm(v$maf)) * 1L  # 1 = the A1 allele
}
dosage <- sapply(seq_len(nrow(v)), function(i) { h <- haplotype(v[i, ]); h[, 1] + h[, 2] })

dir <- tempfile("ref"); dir.create(dir)
ped <- data.frame(fid = sprintf("F%03d", 1:n), iid = sprintf("I%03d", 1:n), pat = 0, mat = 0, sex = 0, pheno = -9)
alleles <- function(i, d) {
  a1 <- v$a1[[i]]; a2 <- v$a2[[i]]
  c(paste(a2, a2), paste(a1, a2), paste(a1, a1))[d + 1]
}
g <- sapply(seq_len(nrow(v)), function(i) alleles(i, dosage[, i]))
write.table(cbind(ped, g), file.path(dir, "ref.ped"), quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(data.frame(v$chr, v$rsid, 0, as.integer(v$bp)), file.path(dir, "ref.map"), quote = FALSE, row.names = FALSE, col.names = FALSE)
out <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE))), "ref"), mustWork = FALSE)
status <- system2(plink, c("--file", shQuote(file.path(dir, "ref")), "--make-bed", "--keep-allele-order",
                           "--out", shQuote(out)), stdout = FALSE)
stopifnot(status == 0)
unlink(paste0(out, c(".log", ".nosex")))
