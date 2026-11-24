#' Top hits of Analyses
#'
#' Returns each Analysis's Associations at or below a p-value `threshold`, strongest first, up to
#' `limit` per Analysis. The query runs as a Job on the API, and `og_tophits()` waits for it (see
#' `vignette("large-queries-and-jobs")`).
#'
#' By default (`clump = TRUE`) the top hits are clumped on the API, one index variant per
#' independent signal (see `vignette("ld")`): every top hit at `threshold` is clumped, and `limit`
#' then caps the index variants. Each Analysis is clumped in the LD Store matching its Assigned
#' Ancestry, or in the one `ld` names; an Unassigned Analysis is refused (`opengwasr_ld_unmatched`)
#' unless `ld` is given, before any Job runs. The Jobs, one per Analysis, are submitted grouped by
#' LD Store; with progress on (option `opengwasr.progress`), a message of class
#' `opengwasr_clumping` says which LD Store each Analysis is clumped in. `r2` and `kb` default to the LD Store's Far Threshold
#' (`t_far`) and its far window (`far_bp`, 10,000 kb), and are always sent. A pair the LD Store can't
#' answer exactly is refused by the API (`opengwasr_ld_refusal`). Top hits the LD Store doesn't hold
#' can't be clumped: they are left out, with a warning of class `opengwasr_clump_absent` (`count`).
#' Clumped rows add `clump_ld`, `clump_ld_choice`, `clump_r2`, `clump_kb` and `clump_size`, and
#' `og_provenance(x)$clump` has each Analysis's `hits`, `absent` and `leads`. Pass `clump = FALSE`
#' for every top hit.
#'
#' With a PLINK Reference in `ld` (see [og_plink_reference()]) the clumping is local: each Analysis's
#' top hits are fetched unclumped (up to the API's most, 10,000), matched to the reference by rsID
#' (those not in it are left out, with a message) and clumped by PLINK, and `limit` is then applied
#' to its index variants. If the API cut an Analysis's top hits off at 10,000, you get an
#' `opengwasr_truncated` warning that clumping saw only the strongest; clumping on the server (an LD
#' Store in `ld`) has no such cap. `og_provenance(x)$clump` names the reference's path and the `r2`
#' and `kb`.
#'
#' The columns are those the vignette section "What the columns mean" lists: the Analysis `id`, the variant, `beta` (`z * se`), `se`, `z`, `pval`, `effect_scale`
#' (`"sd"` or `"log_or"` from the Analysis's stored effect scale, or `"original"` with
#' `original_units = TRUE`), `n`, `sample_size_kind`, `ncase` and `ncontrol`, `association_status` and
#' the `store_id` and `release_id` each Association came from. `n` is the Association's own sample size
#' where the API sends one, otherwise the Analysis's `sample_size`; the other three describe the Analysis. `pval` underflows to 0 when `|z|` is
#' above about 38.5; `z` stays exact, so rank the strongest signals by `abs(z)`.
#'
#' One Job takes at most 100 Analyses from 10 GWAS Stores, and at most 100,000 rows (`limit` times its
#' Analyses). A larger request is split into several Jobs,
#' run a few at a time (`og_api(max_in_flight = )`) and put back together: you get the rows an unsplit
#' request would give, with every Job in [og_provenance()]. The rows are ordered by Analysis, in the
#' order you asked for, and within each Analysis in the API's order (strongest first), whether one Job
#' or several answered.
#' `limit` applies to each Analysis, so splitting never changes it. A request the API refuses as too
#' costly (`opengwasr_request_too_large`) is never split to get round that.
#'
#' When `limit` cuts any Analysis's top hits off, you get a warning of class
#' `opengwasr_truncated`, and [og_provenance()] has `truncated = TRUE`.
#'
#' @param src A Source, from [og_api()].
#' @param analyses Analysis identifiers (see [og_analyses()]).
#' @param threshold The largest p-value to return. Each GWAS Store indexes `5e-8`, `5e-6` and
#'   `5e-4` and refuses a looser one (`opengwasr_unsupported_threshold`).
#' @param limit The most top hits to return per Analysis, at most 10,000.
#' @param observed_only Leave out Imputed Associations.
#' @param clump Clump the top hits to one per independent signal.
#' @param ld An LD Store's name, an ancestry code (its LD Store), a PLINK Reference from
#'   [og_plink_reference()], or `NULL` to match each Analysis's Assigned Ancestry. Only with
#'   `clump = TRUE`.
#' @param r2,kb The r² above which a top hit joins an index variant's clump, and the window in
#'   kilobases; `NULL` for the LD Store's `t_far` and far window, but required with a PLINK
#'   Reference. Only with `clump = TRUE`.
#' @param original_units Give `beta` and `se` in the trait's original units: Analyses stored per SD are
#'   multiplied by their recorded phenotype SD (`original_sd`), and log-odds Analyses are unchanged. It
#'   refuses, with class `opengwasr_original_units_unavailable` and before any query runs, when an
#'   Analysis is stored per SD and isn't a continuous trait with an `original_sd`; `ids` and `reason`
#'   on the error say which and why.
#' @param releases Store Releases to use, as in [og_analyses()].
#' @return A data frame with the columns above and [og_provenance()] attached.
#' @examples
#' \dontrun{
#' src <- og_api()
#' top <- og_tophits(src, "OGS-00011:GCST003116", clump = FALSE, limit = 10)
#' og_provenance(top)
#' }
#' @export
og_tophits <- function(src, analyses, threshold = 5e-8, limit = 1000, observed_only = FALSE,
                       clump = TRUE, ld = NULL, r2 = NULL, kb = NULL, original_units = FALSE,
                       releases = NULL) {
  UseMethod("og_tophits")
}

#' @export
og_tophits.og_api_source <- function(src, analyses, threshold = 5e-8, limit = 1000,
                                     observed_only = FALSE, clump = TRUE, ld = NULL, r2 = NULL,
                                     kb = NULL, original_units = FALSE, releases = NULL) {
  check_tophits_arguments(threshold, limit, clump, ld, r2, kb)
  # `limit` applies to each Analysis, so no split changes it; a Job's rows (`limit` x Analyses) must
  # fit the row cap. Clumping (R/clump.R) sets its own Jobs, fetch size and truncation message.
  association_query(src, "api/tophits", analyses, observed_only, original_units, releases,
                    body = list(threshold = threshold, limit = as.integer(limit)),
                    analyses_per_job = tophits_analyses_per_job(limit), order = "api",
                    truncated = function(ids) truncated_message(analyses_who(ids), limit, "top hits", threshold),
                    spec = list(clump = if (clump) list(ld = ld, r2 = r2, kb = kb, limit = limit, threshold = threshold)))
}

# Kept: R argument types and shapes, and `limit`'s range, which the splitter needs to size Jobs.
# Which thresholds are allowed is the API's to say (`opengwasr_unsupported_threshold`, or
# `opengwasr_invalid_request` outside 0 to 1).
check_tophits_arguments <- function(threshold, limit, clump, ld, r2, kb, call = rlang::caller_env()) {
  bad <- function(what) og_abort(what, class = "opengwasr_argument", call = call)
  check_threshold_limit(threshold, limit, call)
  if (limit < 1 || limit > max_tophits_per_analysis) {
    bad(paste0("`limit` must be one whole number from 1 to ", big_number(max_tophits_per_analysis), "."))
  }
  if (!is_flag(clump)) bad("`clump` must be `TRUE` or `FALSE`.")
  if (!clump && (!is.null(ld) || !is.null(r2) || !is.null(kb))) {
    bad("`ld`, `r2` and `kb` only apply with `clump = TRUE`.")
  }
  check_clump_settings(r2, kb, call)
  if (clump) check_plink_settings(ld, r2, kb, call)
}

# Kept: the R types of `threshold` and `limit` (shared by og_tophits() and og_phewas()); their
# ranges are the API's to check.
check_threshold_limit <- function(threshold, limit, call = rlang::caller_env()) {
  bad <- function(what) og_abort(what, class = "opengwasr_argument", call = call)
  if (!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold)) bad("`threshold` must be one number.")
  if (!is.numeric(limit) || length(limit) != 1 || is.na(limit) || limit != round(limit)) {
    bad("`limit` must be one whole number.")
  }
}

# What every `opengwasr_truncated` warning says: `who` has more than `limit` `what` at p <= `threshold`,
# then `cut` (what was kept) and `advice`.
truncated_message <- function(who, limit, what, threshold, cut = NULL, advice = raise_limit(limit)) {
  cut <- cut %||% paste0("; only the strongest ", big_number(limit), " were returned")
  c(paste0(who, " has more than ", big_number(limit), " ", what, " at p <= ", format(threshold), cut, "."),
    i = advice)
}

raise_limit <- function(limit, most = max_tophits_per_analysis) {
  if (limit < most) paste0("Raise `limit` (at most ", big_number(most), ") for more.")
}

analyses_who <- function(ids) if (length(ids) == 1) ids else paste0("At least one of the ", length(ids), " Analyses")
