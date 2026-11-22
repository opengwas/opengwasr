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
#' One Job takes at most 1,000 distinct variants (ALIDs, after rsIDs are resolved), 100 Analyses and
#' Analyses from 10 GWAS Stores. A larger request is split into several Jobs, run a few at a time
#' (`og_api(max_in_flight = )`), and put back together: you get the rows an unsplit request would
#' give, in a stable order (Analyses in the order asked; then chromosome 1-22, X, Y, MT and other contigs by name; then position; then ALID, whether one Job or several answered), with every Job in
#' [og_provenance()]. A request the API refuses as too costly (`opengwasr_request_too_large`) is never split
#' to get round that: see `vignette("large-queries-and-jobs")`.
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
  deadline <- local_query(src)
  check_query_arguments(analyses, observed_only, original_units)
  kinds <- classify_variants(variants)
  releases <- resolve_releases(src, releases)
  info <- analyses_lookup(src, analyses, releases)
  if (original_units) check_original_units(info)  # refuse before a Job runs, not after
  releases <- releases_of_analyses(releases, info)

  resolved <- resolve_variants(src, kinds, releases, deadline)

  jobs <- list()
  # The lookup's rows are variants, not Associations: only the Associations Jobs have a `total`.
  jobs <- lapply(resolved$jobs, function(j) utils::modifyList(j, list(total = 0, truncated = FALSE)))
  args <- list(observed_only = observed_only, original_units = original_units)
  # One Job takes at most 100 Analyses x 1,000 variants from 10 GWAS Stores: a bigger request is
  # split by those dimensions (Analyses grouped by GWAS Store), and put back together in `finish`.
  requests <- list()
  limits <- job_limits()
  if (length(resolved$alids) > 0) {
    pieces <- split_pieces(list(
      analyses = split_dim(nrow(info), limits$analyses, group = info$store_id, group_limit = limits$releases),
      variants = split_dim(length(resolved$alids), limits$variants)
    ))
    requests <- lapply(pieces, function(p) {
      list(path = "api/associations",
           body = list(variants = as.list(resolved$alids[p$variants]), analyses = as.list(info$id[p$analyses]),
                       observed_only = observed_only,
                       releases = piece_releases(releases, info$store_id, p$analyses)))
    })
  }
  # Everything the answer is shaped from (the rsID resolution, the Analyses) is pinned in this
  # closure at plan time.
  plan <- query_plan(requests, unit = "Associations", finish = function(results) {
    all_jobs <- jobs
    pieces <- list()
    for (out in results) {
      # A Job reports no `total` or `truncated`. It returns every Association it matches: a result
      # over the row limit fails as a `request` error rather than being cut off (ogdb-api guide,
      # "errors"), so the rows read are the whole answer.
      all_jobs <- c(all_jobs, list(utils::modifyList(out$job, list(total = nrow(out$rows), truncated = FALSE))))
      if (nrow(out$rows) > 0) pieces[[length(pieces) + 1]] <- out$rows
    }
    if (length(resolved$unresolved) > 0) warn_unresolved(resolved$unresolved)

    rows <- if (length(pieces) == 0) data.frame() else bind_frames(pieces)
    # The API sends "." for a variant with no rsID; that is a missing value.
    if (!is.null(rows$rsid)) rows$rsid[rows$rsid %in% "."] <- NA_character_
    shaped <- order_associations(shape_associations(rows, info, original_units), info$id)
    attach_provenance(shaped,
                      query = provenance_query("og_associations", args),
                      args = c(list(variants = variants, analyses = analyses), args, list(releases = releases)),
                      jobs = all_jobs)
  })
  run_query(src, plan, deadline)
}
