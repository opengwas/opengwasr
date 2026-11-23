#' Look up variants by rsID or position
#'
#' `og_variants_rsid()` returns the variants the GWAS Stores' own variant tables hold for `rsids`;
#' `og_variants_chrpos()` those at `positions`. They are how you find ALIDs (see `vignette("opengwasr")`).
#' Each runs as Jobs on the API, split when it is over a Job's limits (1,000 values, 10 GWAS Stores),
#' and waits for them.
#'
#' You get every match: one row per ALID per Store Release that holds it, so a multiallelic rsID, or a
#' position with several alleles, gives several rows. A value nothing matches has no row. Rows are in
#' the order the Jobs returned them. rsIDs are matched in lower case.
#'
#' @param src A Source, from [og_api()].
#' @param rsids rsIDs, such as `"rs1421085"`.
#' @param positions GRCh38 positions as `"chromosome:position"`, such as `"16:53767042"` (a `chr`
#'   prefix is allowed).
#' @param releases Store Releases to use, as in [og_analyses()].
#' @return A data frame with `alid`, `chromosome`, `position`, `effect_allele`, `other_allele`,
#'   `rsid`, `store_id` and `release_id`, with [og_provenance()] attached.
#' @examples
#' \dontrun{
#' src <- og_api()
#' og_variants_rsid(src, "rs1421085", releases = "OGS-00011")
#' og_variants_chrpos(src, "16:53767042", releases = "OGS-00011")
#' }
#' @export
og_variants_rsid <- function(src, rsids, releases = NULL) UseMethod("og_variants_rsid")

#' @export
og_variants_rsid.og_api_source <- function(src, rsids, releases = NULL) {
  lookup_query(src, "api/variants/rsid", "rsids", check_lookup_values(rsids, "rsids", tolower),
               releases, limit = "rsids")
}

#' @rdname og_variants_rsid
#' @export
og_variants_chrpos <- function(src, positions, releases = NULL) UseMethod("og_variants_chrpos")

#' @export
og_variants_chrpos.og_api_source <- function(src, positions, releases = NULL) {
  lookup_query(src, "api/variants/chrpos", "positions",
               check_lookup_values(positions, "positions", identity), releases, limit = "positions")
}

# Kept: R argument types, which the API can't see. What a value looks like is the API's to say.
check_lookup_values <- function(x, arg, normalise, call = rlang::caller_env()) {
  if (!is.character(x) || length(x) == 0 || anyNA(x) || any(!nzchar(x))) {
    og_abort(paste0("`", arg, "` must be a character vector, with no missing values."),
             class = "opengwasr_argument", call = call)
  }
  unique(normalise(x))
}

# The one body of a lookup query: Store Releases, the Jobs (split by values and Store Releases) and
# the rows they return, under the query-plan contract so og_submit() works.
# `field` is the body field of the values, `limit` the job limit (`job_limits()`) on how many one Job takes.
lookup_query <- function(src, path, field, values, releases, limit) {
  method <- parent.frame()
  fn <- sys.function(sys.parent())
  engine_query(src, method, fn, function(deadline) {
    releases <- resolve_releases(src, releases, method)
    list(requests = lookup_requests(path, field, values, releases, limit), releases = releases,
         finish = function(results) {
           # A lookup that reports `total` and `truncated` is checked against its rows; one that
           # doesn't (the endpoints don't cut off) counts its rows.
           jobs <- lapply(results, function(r) {
             n <- nrow(r$rows)
             if (is.null(r$job$total) && is.null(r$job$truncated)) {
               utils::modifyList(r$job, list(total = n, truncated = FALSE))
             } else {
               check_job_metadata(r$job, n, method)
               r$job
             }
           })
           list(value = lookup_rows(results), jobs = jobs)
         })
  })
}
