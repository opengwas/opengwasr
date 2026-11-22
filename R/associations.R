#' Associations of variants in Analyses
#'
#' Returns the Associations of `variants` in `analyses`: for example, the outcome data for your
#' instruments. Variants are rsIDs or ALIDs (see `vignette("opengwasr")`), mixed. The query runs as
#' Jobs on the API, and `og_associations()` waits for them (see `vignette("large-queries-and-jobs")`).
#'
#' An rsID is resolved to every ALID it names in the Analyses' GWAS Stores (a multiallelic site names
#' several), and you get every match; opengwasr never picks one. An rsID no GWAS Store holds has no
#' Associations: you get a warning of class `opengwasr_unresolved_variants`, with the number as
#' `count` and the rsIDs as `ids`. A variant an Analysis doesn't hold has no row. There is no
#' LD-proxy option (ADR 0004): Reference-Completed Store Releases already hold an Imputed Association
#' for most variants a source didn't report.
#'
#' The columns are those of [og_tophits()]. Every row has an `alid`; its `rsid` is `NA` for a variant
#' that has no rsID (the API sends `"."`).
#'
#' One call takes at most 1,000 distinct variants (ALIDs, after rsIDs are resolved), 100 Analyses and
#' Analyses from 10 GWAS Stores. A larger request raises an error of class
#' `opengwasr_not_implemented`.
#'
#' @param src A Source, from [og_api()].
#' @param variants rsIDs and ALIDs, such as `c("rs1421085", "16:53767042:C:T")`. An ALID must be in
#'   canonical form (see [og_alid()]); anything else is an `opengwasr_argument` error.
#' @param analyses Analysis identifiers (see [og_analyses()]).
#' @param observed_only Leave out Imputed Associations (`association_status == "imputed"`).
#' @param original_units Give `beta` and `se` in the trait's original units, as in [og_tophits()].
#' @param releases Store Releases to use, as in [og_analyses()].
#' @return A data frame with [og_provenance()] attached.
#' @examples
#' \dontrun{
#' src <- og_api()
#' og_associations(src, c("rs1421085", "16:53767042:C:T"), "OGS-00011:GCST003116")
#' }
#' @export
og_associations <- function(src, variants, analyses, observed_only = FALSE, original_units = FALSE,
                            releases = NULL) {
  UseMethod("og_associations")
}

#' @export
og_associations.og_api_source <- function(src, variants, analyses, observed_only = FALSE,
                                          original_units = FALSE, releases = NULL) {
  check_query_arguments(analyses, observed_only, original_units)
  kinds <- classify_variants(variants)
  check_associations_limits(kinds, analyses)
  deadline <- job_now() + src$timeout
  releases <- resolve_releases(src, releases)
  info <- analyses_lookup(src, analyses, releases)
  if (original_units) check_original_units(info)  # refuse before a Job runs, not after
  releases <- releases_of_analyses(releases, info)

  resolved <- resolve_variants(src, kinds, releases, deadline)
  if (length(resolved$alids) > max_variants_per_job) {
    not_implemented(
      paste0("A query over ", length(resolved$alids), " variants (a Job takes at most ", max_variants_per_job, ")"),
      "#9", "Ask for at most 1,000 variants at a time; opengwasr will split larger queries later.",
      rlang::current_env()
    )
  }

  jobs <- list()
  # The lookup's rows are variants, not Associations: only the Associations Job has a `total`.
  if (!is.null(resolved$job)) jobs <- list(utils::modifyList(resolved$job, list(total = 0, truncated = FALSE)))
  rows <- NULL
  if (length(resolved$alids) > 0) {
    body <- list(variants = as.list(resolved$alids), analyses = as.list(info$id),
                 observed_only = observed_only, releases = releases)
    out <- run_job(src, "api/associations", body, unit = "Associations", deadline = deadline)
    rows <- out$rows
    # The Job reports no `total` or `truncated`. It returns every Association it matches: a result
    # over the row limit fails as a `request` error rather than being cut off (ogdb-api guide,
    # "errors"), so the rows read are the whole answer.
    jobs <- c(jobs, list(utils::modifyList(out$job, list(total = nrow(rows), truncated = FALSE))))
  }
  if (length(resolved$unresolved) > 0) warn_unresolved(resolved$unresolved)

  rows <- rows %||% data.frame()
  # The API sends "." for a variant with no rsID; that is a missing value.
  if (!is.null(rows$rsid)) rows$rsid[rows$rsid %in% "."] <- NA_character_
  args <- list(observed_only = observed_only, original_units = original_units)
  attach_provenance(shape_associations(rows, info, original_units),
                    query = provenance_query("og_associations", args),
                    args = c(list(variants = variants, analyses = analyses), args, list(releases = releases)),
                    jobs = jobs)
}

# What can be refused before any request: more distinct ALIDs, rsIDs or Analyses than one Job takes.
# (rsIDs can resolve to more ALIDs, or to ALIDs already given, so the resolved set is checked later.)
check_associations_limits <- function(kinds, analyses, call = rlang::caller_env()) {
  too_many <- function(what, n, limit, noun) {
    not_implemented(
      paste0(what, " ", n, " ", noun, " (a Job takes at most ", format(limit, big.mark = ","), ")"),
      "#9", paste0("Ask for at most ", format(limit, big.mark = ","), " ", noun,
                   " at a time; opengwasr will split larger queries later."), call
    )
  }
  if (length(kinds$alids) > max_variants_per_job) too_many("A query over", length(kinds$alids), max_variants_per_job, "variants")
  if (length(kinds$rsids) > max_rsids_per_job) too_many("Looking up", length(kinds$rsids), max_rsids_per_job, "rsIDs")
  n <- length(unique(analyses))
  if (n > max_analyses_per_job) too_many("A query over", n, max_analyses_per_job, "Analyses")
}
