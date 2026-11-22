#' Top hits of Analyses
#'
#' Returns each Analysis's Associations at or below a p-value `threshold`, strongest first, up to
#' `limit` per Analysis. The query runs as a Job on the API, and `og_tophits()` waits for it (see
#' `vignette("large-queries-and-jobs")`).
#'
#' Clumping (`clump = TRUE`, the default once it is built) isn't available yet: asking for it raises
#' an error of class `opengwasr_not_implemented`. Pass
#' `clump = FALSE` for every top hit. `ld`, `r2` and `kb` only mean something with clumping.
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
#' @param clump Clump the top hits to one per independent signal. Not yet available.
#' @param ld,r2,kb Clumping settings, used only with `clump = TRUE`.
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
  # fit the row cap.
  association_query(src, "api/tophits", analyses, observed_only, original_units, releases,
                    body = list(threshold = threshold, limit = as.integer(limit)),
                    analyses_per_job = tophits_analyses_per_job(limit), order = "api",
                    truncated = function(ids) truncated_message(ids, threshold, limit))
}

# Kept: R argument types and shapes, and `limit`'s range, which the splitter needs to size Jobs.
# Which thresholds are allowed is the API's to say (`opengwasr_unsupported_threshold`, or
# `opengwasr_invalid_request` outside 0 to 1).
check_tophits_arguments <- function(threshold, limit, clump, ld, r2, kb, call = rlang::caller_env()) {
  bad <- function(what) og_abort(what, class = "opengwasr_argument", call = call)
  if (!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold)) bad("`threshold` must be one number.")
  if (!is.numeric(limit) || length(limit) != 1 || is.na(limit) || limit != round(limit) ||
      limit < 1 || limit > max_tophits_per_analysis) {
    bad(paste0("`limit` must be one whole number from 1 to ", format(max_tophits_per_analysis, big.mark = ","), "."))
  }
  if (!is_flag(clump)) bad("`clump` must be `TRUE` or `FALSE`.")
  if (clump) {
    not_implemented("`clump = TRUE`", "#13", "Pass `clump = FALSE` for every top hit.", call)
  }
  if (!is.null(ld) || !is.null(r2) || !is.null(kb)) {
    bad("`ld`, `r2` and `kb` only apply with `clump = TRUE`.")
  }
}

# A feature the vignettes promise and a later ticket builds.
not_implemented <- function(what, ticket, advice, call) {
  og_abort(c(paste0(what, " isn't available yet: opengwasr ticket ", ticket, " builds it."), i = advice),
           class = "opengwasr_not_implemented", ticket = ticket, call = call)
}

# What the `opengwasr_truncated` warning says when `limit` cut an Analysis's top hits off.
truncated_message <- function(ids, threshold, limit) {
  who <- if (length(ids) == 1) ids else paste0("At least one of the ", length(ids), " Analyses")
  c(paste0(who, " has more than ", format(limit, big.mark = ","), " top hits at p <= ",
           format(threshold), "; only the strongest ", format(limit, big.mark = ","), " were returned."),
    i = if (limit < max_tophits_per_analysis) {
      paste0("Raise `limit` (at most ", format(max_tophits_per_analysis, big.mark = ","), ") for more.")
    })
}
