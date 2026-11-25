#' Is a variant in the LD Store?
#'
#' `og_ld_reflookup()` says whether each of `variants` is held by an LD Store, so you can check
#' instruments before clumping or building an LD matrix. See `vignette("ld")`.
#'
#' `variants` and the LD Store are as for [og_ld_proxies()]; a Local Source is refused. An rsID stands for every ALID it names, one row each; an rsID no GWAS Store holds has
#' one row with `alid` `NA` and `present` `FALSE`. A variant the LD Store doesn't hold is a row, not
#' a warning. One Job takes at most 10,000 variants; more are split.
#'
#' @inheritParams og_ld_proxies
#' @return A data frame with [og_provenance()] attached (`ld`, `ld_choice`, `absent`), one row per
#'   variant asked, in the order asked: `alid`, `rsid` (`NA` where not known: an ALID given
#'   without an `rsid` column) and `present`.
#' @examples
#' \dontrun{
#' src <- og_api(ld_store = "EUR")
#' og_ld_reflookup(src, c("16:53767042:C:T", "rs1421085"))
#' }
#' @export
og_ld_reflookup <- function(src, variants) UseMethod("og_ld_reflookup")

#' @export
og_ld_reflookup.og_api_source <- function(src, variants) {
  ld_query(src, "api/ld/reflookup", variants, list(
    pieces = function(n) dim_chunks(split_dim(n, job_limits()$ld_reflookup)),
    absence_is_answer = TRUE,
    assemble = ld_reflookup_assemble))
}

# One row per variant asked, in the order asked, duplicates kept (a data frame of Associations is a
# set of variants: each ALID once, with its `rsid` column's value when it has one). An rsID gives a
# row per ALID it names (`ctx$rsid_map`), or one with no ALID when none does.
ld_reflookup_assemble <- function(results, ctx) {
  rows <- result_rows(results)
  held <- if (nrow(rows) > 0) rows$alid[rows$present %in% TRUE] else character()
  v <- ctx$variants
  if (is.data.frame(v)) {
    alid <- unique(v$alid)
    rsid <- if (is.character(v$rsid)) v$rsid[match(alid, v$alid)] else rep(NA_character_, length(alid))
    out <- data.frame(alid = alid, rsid = rsid, stringsAsFactors = FALSE)
  } else {
    named <- split(ctx$rsid_map$alid, ctx$rsid_map$rsid)
    rsid <- is_rsid(v)
    v <- ifelse(rsid, tolower(v), v)
    alids <- lapply(seq_along(v), function(i) {
      if (!rsid[[i]]) return(v[[i]])
      if (is.null(named[[v[[i]]]])) NA_character_ else named[[v[[i]]]]
    })
    out <- data.frame(alid = unlist(alids), rsid = rep(ifelse(rsid, v, NA_character_), lengths(alids)),
                      stringsAsFactors = FALSE)
  }
  out$present <- out$alid %in% held
  rownames(out) <- NULL
  out
}
