#' Associations in a genomic range
#'
#' Returns every Association of `analyses` between `start` and `end` on `chromosome`: positions are
#' GRCh38, one-based and inclusive. The query runs as Jobs on the API, and `og_range()` waits for
#' them (see `vignette("large-queries-and-jobs")`).
#'
#' One Job takes at most 10 Mb and 10 Analyses (from at most 10 GWAS Stores). A wider range or more
#' Analyses is split into windows of at most 10 Mb, crossed with Analyses, run a few at a time
#' (`og_api(max_in_flight = )`), and put back together: you get the rows an unsplit request would
#' give, ordered by Analysis (in the order asked), chromosome, position and ALID, with every Job in
#' [og_provenance()]. A range with more Associations than the API will return in one Job fails with
#' `opengwasr_request_too_large`; it is never split to get round that. The columns are those of
#' [og_associations()].
#'
#' @param src A Source, from [og_api()] or [og_local()].
#' @param analyses Analysis identifiers (see [og_analyses()]).
#' @param chromosome One chromosome, such as `"9"` or `"X"`.
#' @param start,end The first and last position, inclusive.
#' @param observed_only Leave out Imputed Associations.
#' @param original_units Give `beta` and `se` in the trait's original units, as in [og_tophits()].
#' @param releases Store Releases to use, as in [og_analyses()].
#' @return A data frame with [og_provenance()] attached.
#' @examples
#' \dontrun{
#' src <- og_api()
#' og_range(src, "OGS-00011:GCST003116", chromosome = "9", start = 22090000, end = 22130000)
#' }
#' @export
og_range <- function(src, analyses, chromosome, start, end, observed_only = FALSE,
                     original_units = FALSE, releases = NULL) {
  UseMethod("og_range")
}

#' @export
og_range.og_api_source <- function(src, analyses, chromosome, start, end, observed_only = FALSE,
                                   original_units = FALSE, releases = NULL) {
  # Kept: R argument types; `start` not after `end`, which the splitter needs to cut windows.
  bad <- function(what) og_abort(what, class = "opengwasr_argument")
  if (!is.character(chromosome) || length(chromosome) != 1 || is.na(chromosome) || !nzchar(chromosome)) {
    bad("`chromosome` must be one chromosome name, such as \"9\".")
  }
  whole <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && x == round(x)
  if (!whole(start) || !whole(end)) bad("`start` and `end` must be whole positions.")
  if (start > end) bad("`start` must not be after `end`.")
  limits <- job_limits()
  association_query(src, "api/associations/range", analyses, observed_only, original_units, releases,
                    analyses_per_job = limits$range_analyses,
                    spec = list(interval = list(chromosome = chromosome, start = start, end = end,
                                                limit = limits$range_window)))
}

#' @export
og_range.og_local_source <- og_range.og_api_source
