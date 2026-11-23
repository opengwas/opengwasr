# Provenance: what produced a data frame, which no single row can hold.
#
# Every query function ends with `attach_provenance()`; `og_provenance()` reads it back. It is an
# attribute, and base `rbind()` keeps the first frame's, so each carries a fingerprint of its rows:
# `og_provenance()` refuses it once the rows are no longer the ones the query returned.

#' Provenance of a query's result
#'
#' `og_provenance(x)` says how `x` was produced: the call, the Store Releases that answered, the
#' Job ids, and the API's counts, including whether `limit` cut any off (`total`, `truncated`).
#'
#' It is attached to the data frame a query function returns, and describes exactly those rows. After
#' you `rbind()` or subset the rows they are no longer what the query returned: the attribute may
#' survive, but `og_provenance()` then warns (class `opengwasr_provenance_stale`) and returns `NULL`
#' rather than give counts that no longer fit. Some operations, such as `merge()`, drop the attribute
#' altogether, and `og_provenance()` then stops with class `opengwasr_no_provenance`. Keep it, or its
#' counts, before you combine data frames. Adding columns doesn't matter. Every Association also
#' carries its own `store_id` and `release_id`.
#'
#' @param x A data frame returned by a query function such as [og_tophits()].
#' @return A list of class `og_provenance`:
#'   * `query`: the call and its arguments, as text; `args`: the arguments as a list.
#'   * `store_releases`: the Store Releases that answered, as `<store_id>/<release_id>`.
#'   * `job_ids`: the API's Job ids; `existing`: whether the API reattached you to a Job it already had.
#'   * `cost_estimate`: the API's cost estimate for the Jobs, summed.
#'   * `total`, `truncated`: how many Associations matched (what the API counted), and whether `limit` cut any off; `returned`: the rows of `x`.
#'   * `retrieved`: when the answer was read, as a `POSIXct`.
#'   * Clumped results: `clump`, a data frame with one row per Analysis clumped (one row, `id` `NA`,
#'     for [og_clump()]'s one set): `id`, `ld`, `ld_choice`, `r2`, `kb`, and the API's counts `hits`
#'     (the variants clumped), `absent` (those not in the LD Store) and `leads` (index variants).
#'   * LD results: `ld`, `ld_choice` and `absent` (see [og_ld_matrix()]).
#'
#'   A data frame whose provenance was dropped (by `merge()`, say) raises an error of class
#'   `opengwasr_no_provenance`; one whose rows changed after `rbind()` or subsetting keeps a stale
#'   attribute, and gives the `opengwasr_provenance_stale` warning and `NULL`.
#' @examples
#' \dontrun{
#' top <- og_tophits(src, "OGS-00011:GCST003116", clump = FALSE, limit = 3)
#' og_provenance(top)$truncated
#' }
#' @export
og_provenance <- function(x) {
  prov <- attr(x, "og_provenance", exact = TRUE)
  if (!is.null(prov) && !identical(prov$fingerprint, provenance_fingerprint(x))) {
    og_warn(c("The rows of `x` are no longer the ones the query returned, so its provenance no longer applies.",
              i = "Combining (`rbind()`, `merge()`) or subsetting the rows makes provenance stale. Keep `og_provenance()` before you do."),
            class = "opengwasr_provenance_stale")
    return(NULL)
  }
  if (is.null(prov)) {
    og_abort(c("`x` carries no provenance.",
               i = "Provenance is attached to what a query function returns. It is gone from a data frame that `merge()` or other operations rebuilt."),
             class = "opengwasr_no_provenance")
  }
  prov
}

#' Attach provenance to a query's result.
#'
#' @param x The result data frame.
#' @param query The call as text, such as `"og_tophits(limit = 3)"`.
#' @param args The call's arguments, as a list.
#' @param jobs The `job` element of each collected Job that produced `x`.
#' @param ... Further fields, for functions with more to record (clumping, LD).
#' @noRd
attach_provenance <- function(x, query, args, jobs, ...) {
  total <- vapply(jobs, function(j) as.numeric(j$total %||% NA), 0)
  truncated <- vapply(jobs, function(j) as.logical(j$truncated %||% NA), NA)
  attr(x, "og_provenance") <- structure(
    list(
      query = query,
      args = args,
      store_releases = unique(unlist(lapply(jobs, `[[`, "releases"))) %||% character(),
      fingerprint = provenance_fingerprint(x),
      job_ids = vapply(jobs, function(j) j$job_id, ""),
      cost_estimate = {
        cost <- vapply(jobs, function(j) as.numeric(j$cost_estimate %||% NA), 0)
        if (anyNA(cost)) NA_real_ else sum(cost)
      },
      existing = any(vapply(jobs, function(j) isTRUE(j$existing), NA)),
      total = if (anyNA(total)) NA_real_ else sum(total),
      truncated = if (anyNA(truncated)) NA else any(truncated),
      returned = NROW(x),
      retrieved = Sys.time(),
      ...
    ),
    class = "og_provenance"
  )
  x
}

# The number of rows and a hash of their identity columns, so provenance can tell the rows it
# describes from other rows.
provenance_fingerprint <- function(x) {
  if (is.matrix(x)) return(rlang::hash(list(dim(x), dimnames(x))))  # an LD matrix: named by ALID
  rlang::hash(list(nrow(x), x$id, x$alid))
}

# "og_tophits(threshold = 5e-08, limit = 3)" from a function name and named arguments.
provenance_query <- function(name, args) {
  shown <- vapply(args, function(a) paste(deparse(a, width.cutoff = 500), collapse = ""), "")
  paste0(name, "(", if (length(args) > 0) paste0(names(args), " = ", shown, collapse = ", "), ")")
}

#' @export
format.og_provenance <- function(x, ...) {
  retrieved <- format(x$retrieved, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
  # `total`: what matched; `returned`: the rows you got (fewer when `limit` cut the answer off).
  counts <- if (is.na(x$total)) NULL else paste0(
    "\u2022 Rows: ", format(x$returned %||% x$total, big.mark = ","), " returned",
    if (!identical(x$returned %||% x$total, x$total)) paste0(" of ", format(x$total, big.mark = ","), " matching"),
    if (isTRUE(x$truncated)) " (truncated)"
  )
  c(
    paste0("\u2500\u2500 opengwasr provenance ", strrep("\u2500", 17)),
    paste0("\u2022 Query: ", x$query),
    paste0("\u2022 Store Releases: ", paste(x$store_releases, collapse = ", ")),
    if (!is.null(x$ld)) paste0("\u2022 LD Store: ", x$ld, " (", x$ld_choice, "); ",
                               length(x$absent), " variant(s) absent"),
    if (!is.null(x$clump)) format_clump(x$clump),
    paste0("\u2022 Jobs: ", paste(x$job_ids, collapse = ", ")),
    counts,
    paste0("\u2022 Retrieved: ", retrieved)
  )
}

#' @export
print.og_provenance <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}
