# Shaping Association rows: the one place that turns the API's rows into what opengwasr returns.
#
# The API sends `z`, `se` and `log10_p` and no `beta` or `pval`. opengwasr derives them here, the
# same way for every function that returns Associations (#6, #7, #8, #11 all call
# `shape_associations()`):
#
#   beta = z * se
#   pval = 10^log10_p     (underflows to 0 above |z| of about 38.5; `z` stays exact)
#
# `beta` is NaN where `se` isn't finite, as ogquery derives it; `pval` is always the row's own
# `log10_p`, whatever it is (opengwasr doesn't police the API's answer). `eaf` and `n` sent as "."
# are NA.
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
  # An Analysis of the rows with no metadata (the catalogue doesn't list it yet) can't be rescaled.
  if (original_units) check_original_units_known(unique(rows$id), analyses, call)
  if (!is.null(rows$rsid)) rows$rsid <- rsid_or_na(rows$rsid)
  for (name in intersect(c("eaf", "n"), names(rows))) rows[[name]] <- number_or_na(rows[[name]])
  # `beta` as ogquery derives it (ogquery stats.py `association_statistics()`, ADR 0104, lines
  # 155-161): NaN where the SE isn't finite (a missing Association, or an infinite SE a release
  # before format 0.2.3 may hold), where z * Inf would read as a value. `pval` is the row's own.
  if (is.null(rows$beta)) {
    rows$beta <- rows$z * rows$se
    rows$beta[!is.finite(rows$se)] <- NaN
  }
  if (is.null(rows$pval)) rows$pval <- 10^rows$log10_p
  rows$log10_p <- NULL
  at <- match(rows$id, analyses$id)
  field <- function(name, as) {
    x <- analyses[[name]]
    if (is.null(x)) x <- rep(NA, nrow(analyses))
    as(x)[at]
  }
  rows$effect_scale <- field("stored_effect_scale", as.character)
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

# A number the API may send as "." (ogquery's marker for none: `eaf`, `n`): a number, NA for ".".
# The API Source reads "." as null (no_value_as_null()), so its column is numbers; a Local Source's
# field holding both is text, each number written with 17 significant digits
# (api_association_rows()).
number_or_na <- function(x) {
  if (is.numeric(x)) return(x)
  x[x %in% "."] <- NA
  as.numeric(x)
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

# `original_units` for Analyses with no metadata: refused, as opengwasr can't rescale without `original_sd`.
check_original_units_known <- function(ids, analyses, call = rlang::caller_env()) {
  ids <- setdiff(ids, analyses$id)
  if (length(ids) == 0) return(invisible())
  og_abort(
    c(paste0("Can't give ", if (length(ids) == 1) "this Analysis" else paste(length(ids), "Analyses"),
             " in original units."),
      stats::setNames(paste0("`", ids, "`: the catalogue doesn't list it, so it has no recorded phenotype SD."),
                      rep("x", length(ids))),
      i = "Use `original_units = FALSE` (the default) for per-SD effects."),
    class = "opengwasr_original_units_unavailable", refusal = TRUE, ids = ids,
    reason = rep("no_metadata", length(ids)), call = call
  )
}

# Put the Associations of a query back in one stable order, whether one Job or several answered:
# Analyses in the order asked for, then chromosome (1-22, X, Y, MT, then others by name), position,
# ALID. `ids` are the canonical Analysis ids as asked for.
order_associations <- function(rows, ids) {
  if (nrow(rows) < 2) return(rows)
  o <- order(match(rows$id, ids), chromosome_rank(rows$chromosome), rows$position, rows$alid, method = "radix")
  rows <- rows[o, , drop = FALSE]
  rownames(rows) <- NULL
  rows
}

# Each chromosome's place in genome order: 1-22, X, Y, MT (and M), then any other name, by name.
chromosome_rank <- function(chromosome) {
  chr <- as.character(chromosome)
  rank <- suppressWarnings(as.numeric(chr))
  special <- match(chr, c("X", "Y", "MT", "M"))
  rank[is.na(rank)] <- 22 + special[is.na(rank)]
  other <- is.na(rank)
  rank[other] <- 1000 + match(chr[other], sort(unique(chr[other])))
  rank
}

# Put the rows of a split query that are in a given order per Analysis (top hits, strongest first)
# back by Analysis in the order asked for, keeping each Analysis's own order.
order_by_analysis <- function(rows, ids) {
  if (nrow(rows) < 2) return(rows)
  rows <- rows[order(match(rows$id, ids), seq_len(nrow(rows)), method = "radix"), , drop = FALSE]
  rownames(rows) <- NULL
  rows
}
