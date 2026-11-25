#' LD proxies
#'
#' `og_ld_proxies()` finds the variants in LD with each of `variants`: every variant within `kb`
#' kilobases whose r² with it is at least `r2`, strongest first. It is an LD lookup, for questions
#' about LD itself. It never substitutes one variant's Associations for another's (see
#' `vignette("ld")`).
#'
#' `variants` are as for [og_ld_matrix()], and the LD Store is chosen the same way: the Source's
#' `ld_store` (`og_api(ld_store = )`), else matched to the Assigned Ancestry of the Associations'
#' Analyses. A Local Source is refused: it reads no LD Stores. Variants the LD Store doesn't hold have no proxies, and give a warning of class
#' `opengwasr_ld_absent_variants`. The API refuses an `r2` and `kb` the LD Store can't answer exactly
#' (`opengwasr_ld_refusal`, with `reason`, `t_far`, `near_bp` and `far_bp`). One Job takes at most
#' 1,000 variants; more are split.
#'
#' @inheritParams og_ld_matrix
#' @param src A Source, from [og_api()].
#' @param r2 The smallest r² of a proxy.
#' @param kb The window, in kilobases either side of the variant.
#' @return A data frame with [og_provenance()] attached (`ld`, `ld_choice`, `absent`), one row per
#'   proxy: `alid`, `proxy_alid`, `r` (signed, for the first alleles of both), `r2` and `distance`
#'   (base pairs). Ordered by `alid` as asked, then by r² (strongest first).
#' @examples
#' \dontrun{
#' src <- og_api(ld_store = "EUR")
#' px <- og_ld_proxies(src, "16:53767042:C:T")
#' head(px)
#' }
#' @export
og_ld_proxies <- function(src, variants, r2 = 0.8, kb = 10000) UseMethod("og_ld_proxies")

#' @export
og_ld_proxies.og_api_source <- function(src, variants, r2 = 0.8, kb = 10000) {
  # Kept: R argument types. Which r2 and kb the LD Store can answer is the API's to say.
  one_number <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)
  if (!one_number(r2)) og_abort("`r2` must be one number.", class = "opengwasr_argument")
  if (!one_number(kb)) og_abort("`kb` must be one number.", class = "opengwasr_argument")
  ld_query(src, "api/ld/proxies", variants, list(
    pieces = function(n) dim_chunks(split_dim(n, job_limits()$ld_proxies)),
    body = list(r2 = r2, kb = kb),
    assemble = ld_proxies_assemble))
}

ld_proxies_assemble <- function(results, ctx) {
  rows <- result_rows(results)
  if (nrow(rows) == 0) {
    return(data.frame(alid = character(), proxy_alid = character(), r = numeric(), r2 = numeric(),
                      distance = integer(), stringsAsFactors = FALSE))
  }
  out <- data.frame(alid = rows$query, proxy_alid = rows$proxy, r = rows$r, r2 = rows$r2,
                    distance = rows$distance, stringsAsFactors = FALSE)
  out <- out[order(match(out$alid, ctx$alids), -out$r2, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}
