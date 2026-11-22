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
  # The call's one deadline starts here, so finding the Store Releases and Analyses counts.
  deadline <- local_query(src)
  check_tophits_arguments(analyses, threshold, limit, observed_only, clump, ld, r2, kb, original_units)
  releases <- resolve_releases(src, releases)
  info <- analyses_lookup(src, analyses, releases)
  if (original_units) check_original_units(info)  # refuse before the Job runs, not after
  releases <- releases_of_analyses(releases, info)

  args <- list(threshold = threshold, limit = limit, observed_only = observed_only, clump = clump,
               original_units = original_units)
  # One Job takes at most 100 Analyses from 10 GWAS Stores, and returns at most 100,000 rows: a bigger
  # request is split by those (Analyses grouped by GWAS Store; `limit` x Analyses within the row cap).
  # `limit` applies to each Analysis, so no split changes it.
  limits <- job_limits()
  pieces <- split_pieces(list(
    analyses = split_dim(nrow(info), tophits_analyses_per_job(limit, limits), group = info$store_id, group_limit = limits$releases)
  ))
  requests <- lapply(pieces, function(p) {
    list(path = "api/tophits",
         body = list(analyses = as.list(info$id[p$analyses]), threshold = threshold, limit = as.integer(limit),
                     observed_only = observed_only,
                     releases = piece_releases(releases, info$store_id, p$analyses)))
  })
  # What the answer is shaped from is pinned here, at plan time: `og_job_result()` finishes the
  # same plan later, however long after.
  plan <- query_plan(requests, unit = "Associations", finish = function(results) {
    for (out in results) check_tophits_metadata(out$job, nrow(out$rows))
    jobs <- lapply(results, `[[`, "job")
    if (any(vapply(jobs, function(j) j$truncated, NA))) warn_truncated(info$id, threshold, limit)
    found <- Filter(function(r) nrow(r) > 0, lapply(results, `[[`, "rows"))
    rows <- if (length(found) == 0) data.frame() else bind_frames(found)
    shaped <- order_by_analysis(shape_associations(rows, info, original_units), info$id)
    attach_provenance(shaped,
                      query = provenance_query("og_tophits", args),
                      args = c(list(analyses = analyses), args, list(releases = releases)),
                      jobs = jobs)
  })
  run_query(src, plan, deadline)
}

# The API says how many top hits it returned and whether `limit` cut any off. An answer that doesn't
# say, or says something the rows contradict, can't be trusted not to be partial.
check_tophits_metadata <- function(job, rows, call = rlang::caller_env()) {
  total <- job$total
  truncated <- job$truncated
  whole <- is.numeric(total) && length(total) == 1 && !is.na(total) && total >= 0 && total == round(total)
  ok <- whole && is.logical(truncated) && length(truncated) == 1 && !is.na(truncated) &&
    total >= rows && (truncated || total == rows)
  if (!ok) {
    job_invalid("Its `total` and `truncated` are missing or don't fit the top hits that were read.",
                job$job_id, call)
  }
}

check_tophits_arguments <- function(analyses, threshold, limit, observed_only, clump, ld, r2, kb,
                                    original_units, call = rlang::caller_env()) {
  check_query_arguments(analyses, observed_only, original_units, call)
  bad <- function(what) og_abort(what, class = "opengwasr_argument", call = call)
  if (!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold) || threshold <= 0 || threshold >= 1) {
    bad("`threshold` must be one p-value, above 0 and below 1.")
  }
  if (!is.numeric(limit) || length(limit) != 1 || is.na(limit) || limit != round(limit) ||
      limit < 1 || limit > 10000) {
    bad("`limit` must be one whole number from 1 to 10,000.")
  }
  if (!is_flag(clump)) bad("`clump` must be `TRUE` or `FALSE`.")
  if (clump) {
    not_implemented("`clump = TRUE`", "#13", "Pass `clump = FALSE` for every top hit.", call)
  }
  if (!is.null(ld) || !is.null(r2) || !is.null(kb)) {
    bad("`ld`, `r2` and `kb` only apply with `clump = TRUE`.")
  }
}

# The arguments every Association query shares: `analyses`, `observed_only` and `original_units`.
check_query_arguments <- function(analyses, observed_only, original_units, call = rlang::caller_env()) {
  bad <- function(what) og_abort(what, class = "opengwasr_argument", call = call)
  if (!is.character(analyses) || length(analyses) == 0 || anyNA(analyses) || any(!nzchar(analyses))) {
    bad("`analyses` must be a character vector of Analysis identifiers, with no missing values.")
  }
  if (!is_flag(observed_only)) bad("`observed_only` must be `TRUE` or `FALSE`.")
  if (!is_flag(original_units)) bad("`original_units` must be `TRUE` or `FALSE`.")
}

is_flag <- function(x) is.logical(x) && length(x) == 1 && !is.na(x)

# A feature the vignettes promise and a later ticket builds.
not_implemented <- function(what, ticket, advice, call) {
  og_abort(c(paste0(what, " isn't available yet: opengwasr ticket ", ticket, " builds it."), i = advice),
           class = "opengwasr_not_implemented", ticket = ticket, call = call)
}

warn_truncated <- function(ids, threshold, limit, call = rlang::caller_env()) {
  who <- if (length(ids) == 1) ids else paste0("At least one of the ", length(ids), " Analyses")
  og_warn(
    c(paste0(who, " has more than ", format(limit, big.mark = ","), " top hits at p <= ",
             format(threshold), "; only the strongest ", format(limit, big.mark = ","),
             " were returned."),
      i = if (limit < 10000) "Raise `limit` (at most 10,000) for more."),
    class = "opengwasr_truncated", n_analyses = length(ids), call = call
  )
}
