#' PheWAS: variants across Analyses
#'
#' Returns the Associations of `variants` at p <= `threshold`, across every Analysis of the Store
#' Releases (or the `analyses` you name). The query runs as Jobs on the API, and `og_phewas()` waits
#' for them (see `vignette("large-queries-and-jobs")`).
#'
#' `variants` are rsIDs and ALIDs, mixed, as for [og_associations()]. An rsID is resolved to every ALID
#' it names in the Store Releases (a multiallelic site names several), and opengwasr runs one PheWAS per
#' ALID and combines them: you get every match, in the order the variants were given (an rsID's ALIDs in
#' the order the API gave them). An rsID no Store Release holds gives a warning of class
#' `opengwasr_unresolved_variants`, as for [og_associations()]. Every Job is in [og_provenance()].
#'
#' With `analyses = NULL` (the default) every Analysis of the Store Releases is searched. The
#' Analyses' metadata (`beta`'s effect scale, `n`, `ncase`) is then looked up once, after the rows, for
#' the Analyses that appear in them. `original_units = TRUE` can then only be refused (class
#' `opengwasr_original_units_unavailable`) after the search, naming the Analyses in the result; name
#' `analyses` and it is refused before any query runs.
#'
#' The columns are those of [og_associations()]; rows are in the order the Jobs returned them (one per
#' ALID, split by Store Release, each in the API's order). One Job takes at most 10 GWAS Stores and
#' 100 Analyses, and a larger request is split and put back together as for [og_associations()].
#'
#' `limit` applies to each ALID (not to all the variants together), and to each group of up to 10 GWAS Stores searched (the Store Releases
#' are searched in groups of that size), so you can get more than `limit` rows in all. When it cuts any off you get a warning of class
#' `opengwasr_truncated`, and [og_provenance()] has `truncated = TRUE`; `total` is the number of
#' Associations that passed `threshold`. `observed_only` is always sent; opengwasr never relies on the
#' API's default.
#'
#' @param src A Source, from [og_api()].
#' @param variants rsIDs and ALIDs, such as `c("rs1421085", "16:53767042:C:T")`; an ALID must be in
#'   canonical form (see [og_alid()]).
#' @param threshold The largest p-value to return; `1` returns every Association.
#' @param analyses `NULL` (every Analysis of the Store Releases), or Analysis identifiers (see
#'   [og_analyses()]).
#' @param releases Store Releases to use, as in [og_analyses()].
#' @param limit The most Associations to return per ALID and group of up to 10 GWAS Stores, at most
#'   10,000.
#' @param observed_only Leave out Imputed Associations.
#' @param original_units Give `beta` and `se` in the trait's original units, as in [og_tophits()].
#' @return A data frame with the columns of [og_associations()] and [og_provenance()] attached.
#' @examples
#' \dontrun{
#' src <- og_api()
#' ph <- og_phewas(src, c("rs1421085", "16:53767042:C:T"), releases = "OGS-00011")
#' }
#' @export
og_phewas <- function(src, variants, threshold = 1e-8, analyses = NULL, releases = NULL, limit = 1000,
                      observed_only = FALSE, original_units = FALSE) {
  UseMethod("og_phewas")
}

#' @export
og_phewas.og_api_source <- function(src, variants, threshold = 1e-8, analyses = NULL, releases = NULL,
                                    limit = 1000, observed_only = FALSE, original_units = FALSE) {
  check_threshold_limit(threshold, limit)  # `variants` are checked by the engine, as for og_associations()
  association_query(src, "api/phewas", analyses, observed_only, original_units, releases,
                    body = list(threshold = threshold, limit = as.integer(limit)),
                    variants = variants, order = "jobs",
                    truncated = function(ids) {
                      truncated_message("A PheWAS", limit, "Associations", threshold,
                                        cut = paste0(" for at least one ALID, or one group of up to ", max_releases_per_job,
                                                     " GWAS Stores; only ", big_number(limit), " were returned for each"),
                                        advice = raise_limit(limit, max_phewas_per_variant))
                    },
                    spec = list(all_analyses = TRUE, one_variant = TRUE))
}
