# Shaping Association rows: the one place that turns the API's rows into what opengwasr returns.
#
# The API sends `z`, `se` and `log10_p` and no `beta` or `pval`. opengwasr derives them here, the
# same way for every function that returns Associations (#6, #7, #8, #11 all call
# `shape_associations()`):
#
#   beta = z * se
#   pval = 10^log10_p     (underflows to 0 above |z| of about 38.5; `z` stays exact)
#
# Sample size comes with every row: `n` is the row's own N where the API sends one (the dev API's
# molecular Analyses, such as OGS-00027's, do; most GWAS Stores don't), otherwise the Analysis's
# `sample_size`. `sample_size_kind`, `ncase` and `ncontrol` come from the Analysis (`n_cases`,
# `n_controls`) and describe the Analysis-level count, not where a row's `n` came from.
#
# `original_units = TRUE` rescales `beta` and `se` of Analyses stored per SD by their `original_sd`
# and sets `effect_scale` to "original" (`z` and `pval` don't change); log-odds Analyses pass through.
# It refuses every other Analysis stored per SD (see `check_original_units()`).
#
# `log10_p` is then dropped, so results carry one p-value. If a later API sends `beta` or `pval`,
# they pass through untouched (the epic's one exception). `effect_scale` always comes from the
# Analysis's metadata (`stored_effect_scale`, "sd" or "log_or"), never from a row; `id` is the
# Analysis's canonical id. `rsid` is NA for a variant with no rsID (the API sends ".").

# The columns every function that returns Associations leads with, in this order
# (vignette: What the columns mean). Columns the API sends beyond these follow.
association_columns <- c("id", "store_id", "analysis_id", "analysis_label", "alid", "rsid", "chromosome",
                         "position", "effect_allele", "other_allele", "eaf", "beta", "se", "z", "pval",
                         "effect_scale", "n", "sample_size_kind", "ncase", "ncontrol", "association_status",
                         "release_id")

#' @param rows The API's result rows (a data frame, possibly with no rows or columns).
#' @param analyses Analysis metadata for the Analyses in `rows`, from the Analysis owner: needs `id`
#'   and `stored_effect_scale`; for sample size and original units also `sample_size`,
#'   `sample_size_kind`, `n_cases`, `n_controls`, `original_sd` and `outcome_type`.
#' @param original_units Give `beta` and `se` in the trait's original units.
#' @return A data frame with association_columns first.
#' @noRd
shape_associations <- function(rows, analyses, original_units = FALSE, call = rlang::caller_env()) {
  if (original_units) check_original_units(analyses, call)
  if (nrow(rows) == 0) rows <- empty_associations()
  rows$id <- canonical_ids(rows)
  if (!is.null(rows$rsid)) rows$rsid <- rsid_or_na(rows$rsid)
  if (is.null(rows$beta)) rows$beta <- rows$z * rows$se
  if (is.null(rows$pval)) rows$pval <- 10^rows$log10_p
  rows$log10_p <- NULL
  at <- match(rows$id, analyses$id)
  field <- function(name, as) {
    x <- analyses[[name]]
    if (is.null(x)) x <- rep(NA, nrow(analyses))
    as(x)[at]
  }
  rows$effect_scale <- as.character(analyses$stored_effect_scale[at])
  rows$n <- if (is.null(rows$n)) field("sample_size", as.numeric) else ifelse(is.na(rows$n), field("sample_size", as.numeric), rows$n)
  rows$sample_size_kind <- field("sample_size_kind", as.character)
  rows$ncase <- field("n_cases", as.numeric)
  rows$ncontrol <- field("n_controls", as.numeric)
  if (original_units) {
    scale <- field("original_sd", as.numeric)
    per_sd <- rows$effect_scale == "sd"
    rows$beta[per_sd] <- rows$beta[per_sd] * scale[per_sd]
    rows$se[per_sd] <- rows$se[per_sd] * scale[per_sd]
    rows$effect_scale[per_sd] <- "original"
  }
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

# The Analyses `original_units = TRUE` can't honour. Log-odds Analyses pass through; an Analysis
# stored per SD is rescaled only when its `outcome_type` is known to be "continuous" and it has an
# `original_sd`. Every other one stored per SD is refused, with a `reason` per Analysis:
#   * "binary_per_sd": `outcome_type` is "binary" (a phenotype SD means nothing for a case-control
#     trait, whatever `original_sd` says; the dev catalogue's 1,846 are all `ukb-b-*`);
#   * "no_original_sd": `original_sd` is NA or absent;
#   * "unknown_trait_type": `outcome_type` is absent, NA or neither "binary" nor "continuous".
# A missing field refuses; it never rescales by NA. Kept: original units are computed client-side.
check_original_units <- function(analyses, call = rlang::caller_env()) {
  n <- nrow(analyses)
  per_sd <- analyses$stored_effect_scale %in% "sd"
  type <- as.character(analyses[["outcome_type"]] %||% rep(NA_character_, n))
  sd <- as.numeric(analyses[["original_sd"]] %||% rep(NA_real_, n))
  reason <- ifelse(type %in% "binary", "binary_per_sd",
                   ifelse(is.na(sd), "no_original_sd",
                          ifelse(type %in% "continuous", NA_character_, "unknown_trait_type")))
  bad <- per_sd & !is.na(reason)
  if (!any(bad)) return(invisible())
  ids <- analyses$id[bad]
  reason <- reason[bad]
  why <- c(binary_per_sd = "a binary trait stored per SD, which has no phenotype SD to rescale by",
           no_original_sd = "it has no recorded phenotype SD (`original_sd`)",
           unknown_trait_type = "its trait type isn't recorded as continuous")[reason]
  og_abort(
    c(paste0("Can't give ", if (length(ids) == 1) "this Analysis" else paste(length(ids), "Analyses"),
             " in original units."),
      stats::setNames(paste0("`", ids, "`: ", why, "."), rep("x", length(ids))),
      i = "Use `original_units = FALSE` (the default) for per-SD effects."),
    class = "opengwasr_original_units_unavailable", refusal = TRUE, ids = ids, reason = unname(reason),
    call = call
  )
}

# Put the Associations of a query back in one stable order, whether one Job or several answered:
# Analyses in the order asked for, then chromosome (1-22, X, Y, MT, then others by name), position,
# ALID. `ids` are the canonical Analysis ids as asked for.
order_associations <- function(rows, ids) {
  if (nrow(rows) < 2) return(rows)
  chr <- as.character(rows$chromosome)
  rank <- suppressWarnings(as.numeric(chr))
  special <- match(chr, c("X", "Y", "MT", "M"))
  rank[is.na(rank)] <- 22 + special[is.na(rank)]
  other <- is.na(rank)
  rank[other] <- 1000 + match(chr[other], sort(unique(chr[other])))
  o <- order(match(rows$id, ids), rank, rows$position, rows$alid, method = "radix")
  rows <- rows[o, , drop = FALSE]
  rownames(rows) <- NULL
  rows
}

# Put the rows of a split query that are in a given order per Analysis (top hits, strongest first)
# back by Analysis in the order asked for, keeping each Analysis's own order.
order_by_analysis <- function(rows, ids) {
  if (nrow(rows) < 2) return(rows)
  rows <- rows[order(match(rows$id, ids), seq_len(nrow(rows)), method = "radix"), , drop = FALSE]
  rownames(rows) <- NULL
  rows
}
