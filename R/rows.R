# Shaping Association rows: the one place that turns the API's rows into what opengwasr returns.
#
# The API sends `z`, `se` and `log10_p` and no `beta` or `pval`. opengwasr derives them here, the
# same way for every function that returns Associations (#6, #7, #8, #11 all call
# `shape_associations()`):
#
#   beta = z * se
#   pval = 10^log10_p     (underflows to 0 above |z| of about 38.5; `z` stays exact)
#
# `log10_p` is then dropped, so results carry one p-value. If a later API sends `beta` or `pval`,
# they pass through untouched (the epic's one exception). `effect_scale` always comes from the
# Analysis's metadata (`stored_effect_scale`, "sd" or "log_or"), never from a row; `id` is the
# Analysis's canonical id.

# The columns every function that returns Associations leads with, in this order
# (vignette: What the columns mean). Columns the API sends beyond these follow.
association_columns <- c("id", "store_id", "analysis_id", "analysis_label", "alid", "rsid", "chromosome",
                         "position", "effect_allele", "other_allele", "eaf", "beta", "se", "z", "pval",
                         "effect_scale", "association_status", "release_id")

#' @param rows The API's result rows (a data frame, possibly with no rows or columns).
#' @param analyses Analysis metadata for the Analyses in `rows`, from the Analysis owner: needs `id`
#'   and `stored_effect_scale`.
#' @return A data frame with association_columns first.
#' @noRd
shape_associations <- function(rows, analyses) {
  if (nrow(rows) == 0) rows <- empty_associations()
  rows$id <- canonical_ids(rows)
  if (is.null(rows$beta)) rows$beta <- rows$z * rows$se
  if (is.null(rows$pval)) rows$pval <- 10^rows$log10_p
  rows$log10_p <- NULL
  rows$effect_scale <- as.character(analyses$stored_effect_scale[match(rows$id, analyses$id)])
  first <- intersect(association_columns, names(rows))
  rows <- rows[, c(first, setdiff(names(rows), first)), drop = FALSE]
  rownames(rows) <- NULL
  rows
}

# No Associations, with the columns a result would have.
empty_associations <- function() {
  chr <- character()
  data.frame(store_id = chr, analysis_id = chr, analysis_label = chr, alid = chr, rsid = chr,
             chromosome = chr, position = integer(), effect_allele = chr, other_allele = chr,
             eaf = numeric(), se = numeric(), z = numeric(), log10_p = numeric(),
             association_status = chr, release_id = chr, stringsAsFactors = FALSE)
}
