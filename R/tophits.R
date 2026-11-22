#' Top hits of Analyses
#'
#' Returns each Analysis's Associations at or below a p-value `threshold`, strongest first, up to
#' `limit` per Analysis. The query runs as a Job on the API, and `og_tophits()` waits for it (see
#' `vignette("large-queries-and-jobs")`).
#'
#' Clumping (`clump = TRUE`, the default once it is built) and `original_units = TRUE` aren't
#' available yet: asking for either raises an error of class `opengwasr_not_implemented`. Pass
#' `clump = FALSE` for every top hit. `ld`, `r2` and `kb` only mean something with clumping.
#'
#' The columns are those the vignette section "What the columns mean" lists: the Analysis `id`, the variant, `beta` (`z * se`), `se`, `z`, `pval`, `effect_scale`
#' (`"sd"` or `"log_or"`, from the Analysis's stored effect scale), `association_status` and the
#' `store_id` and `release_id` each Association came from. `pval` underflows to 0 when `|z|` is
#' above about 38.5; `z` stays exact, so rank the strongest signals by `abs(z)`.
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
#' @param original_units Give `beta` and `se` in the trait's original units. Not yet available.
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
  check_tophits_arguments(analyses, threshold, limit, observed_only, clump, ld, r2, kb, original_units)
  # The call's one deadline starts here, so finding the Store Releases and Analyses counts.
  deadline <- job_now() + src$timeout
  releases <- resolve_releases(src, releases)
  info <- analyses_lookup(src, analyses, releases)
  # Only the Store Releases of the Analyses asked for.
  used <- unique(info$store_id)
  releases <- Filter(function(r) r$store_id %in% used, releases)
  if (length(releases) > max_releases_per_job) {
    not_implemented(
      paste0("A query over ", length(releases), " GWAS Stores (a Job takes at most ", max_releases_per_job, ")"),
      "#9", "Ask for Analyses from at most 10 GWAS Stores at a time; opengwasr will split larger queries later.",
      rlang::current_env()
    )
  }

  body <- list(analyses = as.list(info$id), threshold = threshold, limit = as.integer(limit),
               observed_only = observed_only, releases = releases)
  out <- run_job(src, "api/tophits", body, unit = "Associations", deadline = deadline)
  check_tophits_metadata(out$job, nrow(out$rows))
  if (out$job$truncated) warn_truncated(info$id, threshold, limit)

  args <- list(threshold = threshold, limit = limit, observed_only = observed_only, clump = clump)
  attach_provenance(shape_associations(out$rows, info),
                    query = provenance_query("og_tophits", args),
                    args = c(list(analyses = analyses), args, list(releases = releases)),
                    jobs = list(out$job))
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

# The most Store Releases one Job takes (the API's limit).
max_releases_per_job <- 10L

check_tophits_arguments <- function(analyses, threshold, limit, observed_only, clump, ld, r2, kb,
                                    original_units, call = rlang::caller_env()) {
  bad <- function(what) og_abort(what, class = "opengwasr_argument", call = call)
  flag <- function(x) is.logical(x) && length(x) == 1 && !is.na(x)
  if (!is.character(analyses) || length(analyses) == 0 || anyNA(analyses) || any(!nzchar(analyses))) {
    bad("`analyses` must be a character vector of Analysis identifiers, with no missing values.")
  }
  if (!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold) || threshold <= 0 || threshold >= 1) {
    bad("`threshold` must be one p-value, above 0 and below 1.")
  }
  if (!is.numeric(limit) || length(limit) != 1 || is.na(limit) || limit != round(limit) ||
      limit < 1 || limit > 10000) {
    bad("`limit` must be one whole number from 1 to 10,000.")
  }
  if (!flag(observed_only)) bad("`observed_only` must be `TRUE` or `FALSE`.")
  if (!flag(clump)) bad("`clump` must be `TRUE` or `FALSE`.")
  if (!flag(original_units)) bad("`original_units` must be `TRUE` or `FALSE`.")
  if (clump) {
    not_implemented("`clump = TRUE`", "#13", "Pass `clump = FALSE` for every top hit.", call)
  }
  if (!is.null(ld) || !is.null(r2) || !is.null(kb)) {
    bad("`ld`, `r2` and `kb` only apply with `clump = TRUE`.")
  }
  if (original_units) {
    not_implemented("`original_units = TRUE`", "#8", "Use `original_units = FALSE` for per-SD effects.", call)
  }
}

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
