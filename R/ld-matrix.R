#' LD matrices
#'
#' `og_ld_matrix()` returns the signed LD correlation r between every pair of `variants`, read from
#' an LD Store. See `vignette("ld")`.
#'
#' The matrix is named by ALID, on both dimensions; the sign of r refers to the first allele in each
#' name. `variants` are ALIDs or rsIDs (an rsID stands for every ALID it names), or a data frame of
#' Associations, whose `alid` column names the variants and whose Analyses' Assigned Ancestry chooses
#' the LD Store when `ld` is `NULL`. A variant the LD Store doesn't hold is left out of the matrix,
#' with a warning of class `opengwasr_ld_absent_variants` (`count`, `ids`); they are in
#' `og_provenance(m)$absent`.
#'
#' An LD Store doesn't hold every pair: a pair it doesn't hold is `NA`. One Job takes at most 500
#' variants; more are split into several Jobs and put back together, so the matrix covers every
#' pair.
#'
#' With `fill_below_t_far = TRUE`, a pair of variants on one chromosome, within the LD Store's
#' `far_bp` of each other, that it doesn't hold (so its r² is below the LD Store's Far Threshold)
#' is set to 0. Pairs further apart, or on different chromosomes, stay `NA`.
#'
#' A request the LD Store can't answer is refused by the API (`opengwasr_ld_refusal`, with `reason`,
#' `t_far`, `near_bp` and `far_bp`); a choice of LD Store that can't be made without guessing is
#' refused by opengwasr (`opengwasr_ld_unmatched`, with the LD Stores in `ld_stores`).
#'
#' @param src A Source, from [og_api()].
#' @param variants ALIDs and rsIDs, or a data frame of Associations.
#' @param ld An LD Store's name, an ancestry code (its LD Store), or `NULL` to match the Assigned
#'   Ancestry of the Associations' Analyses (refused for Unassigned or mixed ancestries, and for
#'   variants given without Analyses).
#' @param fill_below_t_far Set pairs below the Far Threshold to 0, as above.
#' @return A numeric matrix (class `og_ld_matrix`, which is also a `matrix`) with [og_provenance()] attached: `ld` (the LD Store that answered),
#'   `ld_choice` (`"user"`, `"ancestry_default"` or `"matched"`) and `absent`.
#' @examples
#' \dontrun{
#' src <- og_api()
#' m <- og_ld_matrix(src, c("16:53772541:A:G", "16:53767042:C:T"), ld = "EUR")
#' og_provenance(m)$ld
#' }
#' @export
og_ld_matrix <- function(src, variants, ld = NULL, fill_below_t_far = FALSE) UseMethod("og_ld_matrix")

#' @export
og_ld_matrix.og_api_source <- function(src, variants, ld = NULL, fill_below_t_far = FALSE) {
  # Kept: an R argument's type.
  if (!is_flag(fill_below_t_far)) {
    og_abort("`fill_below_t_far` must be `TRUE` or `FALSE`.", class = "opengwasr_argument")
  }
  ld_query(src, "api/ld/matrix", variants, ld, list(
    # Every pair is in some piece: pieces of two chunks hold every pair of their union.
    pieces = function(n) lapply(split_pairs(n, job_limits()$ld_matrix), function(p) union(p$a, p$b)),
    assemble = function(results, ctx) ld_matrix_assemble(results, ctx, fill_below_t_far)))
}

# The one matrix of every present variant, from the Jobs' rows. A row is `alid` and `r`, its r with
# each of the rows' variants in the same order (null, where the LD Store holds no r, is NA).
ld_matrix_assemble <- function(results, ctx, fill_below_t_far) {
  alids <- ctx$alids
  m <- matrix(NA_real_, length(alids), length(alids), dimnames = list(alids, alids))
  for (res in results) {
    rows <- res$rows
    if (nrow(rows) == 0) next
    r <- ld_r_matrix(rows$r, nrow(rows))
    at <- match(rows$alid, alids)
    if (is.null(r) || anyNA(at)) {
      job_invalid("Its rows aren't a square matrix of the variants asked for.", res$job$job_id)
    }
    m[at, at] <- r
  }
  present <- !alids %in% ctx$absent
  m <- m[present, present, drop = FALSE]
  if (fill_below_t_far) m <- ld_fill_below_t_far(m, ctx$store$far_bp)
  structure(m, class = c("og_ld_matrix", "matrix", "array"))
}

# A matrix that prints as one: its provenance is read with og_provenance(), not shown after every
# `round(m, 3)`.
#' @export
print.og_ld_matrix <- function(x, ...) {
  attr(x, "og_provenance") <- NULL
  print(unclass(x), ...)
  invisible(x)
}

# The `r` member of a Job's rows as a rows x rows numeric matrix (the JSON parser makes a matrix of
# equal-length arrays, and a list of them when any holds a null), or NULL if it isn't square.
ld_r_matrix <- function(r, n) {
  if (is.list(r)) r <- do.call(rbind, lapply(r, function(row) as.numeric(unlist(lapply(row, function(x) x %||% NA)))))
  if (is.numeric(r) && !is.matrix(r)) r <- matrix(r, nrow = n)
  if (!is.matrix(r) || !is.numeric(r) || nrow(r) != n || ncol(r) != n) return(NULL)
  r
}

# Kept on the client: the API's `fill` treats every null alike, and these are the ones the LD Store
# knows are below its Far Threshold: within `far_bp` on one chromosome.
ld_fill_below_t_far <- function(m, far_bp) {
  parts <- strsplit(colnames(m), ":", fixed = TRUE)
  chromosome <- vapply(parts, `[[`, "", 1)
  position <- as.numeric(vapply(parts, `[[`, "", 2))
  within <- outer(chromosome, chromosome, "==") & abs(outer(position, position, "-")) <= far_bp
  m[is.na(m) & within] <- 0
  m
}
