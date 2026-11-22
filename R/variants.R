# Variant resolution: the one place that turns what a user calls variants into ALIDs.
#
# A user names variants by ALID (`chromosome:position:A1:A2`) or by rsID, mixed. The GWAS Stores
# are queried by ALID, so every query function that takes `variants` calls `classify_variants()`
# once and then `resolve_variants()`. An rsID is resolved through POST /api/variants/rsid (a Job)
# in the Store Releases the query uses, and stands for EVERY ALID it names there: a multiallelic
# site names several, and one position can carry two spellings of the same indel. opengwasr never
# picks one (ADR 0004: no proxies, no guessing). An rsID no Store Release holds is "unresolved",
# reported by `warn_unresolved()`, never dropped quietly. #11 builds `og_variants_rsid()` and
# `og_variants_chrpos()` on this.

#' Classify a user's variants: ALIDs or rsIDs, nothing else.
#'
#' An ALID is refused unless it is one `og_alid()` could have made: a canonical chromosome, a
#' whole position above 0, two different A/C/G/T alleles, and, when the alleles have the same length,
#' in alphabetical order. (An ALID in any other form names no variant in any GWAS Store.)
#'
#' @param variants A character vector of ALIDs and rsIDs.
#' @return `list(alids, rsids)`, of class `og_variant_kinds`: the unique ALIDs and unique
#'   (lower-case) rsIDs, in order of first appearance. Anything else, or a missing value, raises
#'   `opengwasr_argument`.
#' @noRd
classify_variants <- function(variants, arg = "variants", call = rlang::caller_env()) {
  if (inherits(variants, "og_variant_kinds")) return(variants)
  bad <- function(what, ...) og_abort(c(what, ...), class = "opengwasr_argument", call = call)
  if (!is.character(variants) || length(variants) == 0 || anyNA(variants)) {
    bad(paste0("`", arg, "` must be a character vector of rsIDs and ALIDs, with no missing values."))
  }
  is_rsid <- grepl("^rs[0-9]+$", variants, ignore.case = TRUE)
  # Kept until opengwas/ogdb-api#183 makes the API refuse non-canonical ALIDs; then it goes.
  is_alid <- !is_rsid & is_canonical_alid(variants)
  if (any(!is_rsid & !is_alid)) {
    wrong <- unique(variants[!is_rsid & !is_alid])
    bad(paste0(length(wrong), " of the `", arg, "` ", if (length(wrong) == 1) "is" else "are",
               " neither an rsID nor an ALID: ", paste0("\"", utils::head(wrong, 5), "\"", collapse = ", "),
               if (length(wrong) > 5) ", ...", "."),
        i = paste0("An rsID looks like \"rs1421085\"; an ALID like \"16:53767042:C:T\", with the alleles ",
                   "in alphabetical order (or, for an indel, the reference allele first). ",
                   "`og_alid()` makes ALIDs from chromosome, position and alleles."))
  }
  structure(list(alids = unique(variants[is_alid]), rsids = unique(tolower(variants[is_rsid]))),
            class = "og_variant_kinds")
}

# TRUE where `x` is an ALID in canonical form (see `classify_variants()`).
is_canonical_alid <- function(x) {
  m <- regmatches(x, regexec("^([^:]+):([1-9][0-9]*):([ACGT]+):([ACGT]+)$", x))
  vapply(m, function(p) {
    if (length(p) != 5) return(FALSE)
    a1 <- p[[4]]
    a2 <- p[[5]]
    identical(alid_chromosome(p[[2]]), p[[2]]) && a1 != a2 &&
      (nchar(a1) != nchar(a2) || a1 < a2)
  }, NA)
}

#' Resolve a mixed vector of ALIDs and rsIDs to the ALIDs to query.
#'
#' @param variants ALIDs and rsIDs, or the result of [classify_variants()].
#' @param releases Store Releases to resolve rsIDs in, as `list(store_id, release_id)` from
#'   [resolve_releases()]. Only used when there are rsIDs.
#' @return `list(alids, map, lookup, unresolved, job)`: every ALID to query (those given, then those
#'   the rsIDs name, unique); `map`, a data frame of `rsid` and `alid`, one row per ALID an rsID names;
#'   `lookup`, the API's rows (`alid`, `chromosome`, `position`, `effect_allele`, `other_allele`,
#'   `rsid`, `store_id`, `release_id`; none with no rsIDs); `unresolved`, the rsIDs no Store Release
#'   holds; `jobs`, the `job` of every Job the lookup ran (more than one when it was split; empty with
#'   no rsIDs), and `job`, the first of them (`NULL` with no rsIDs).
#' @noRd
resolve_variants <- function(src, variants, releases, deadline = job_now() + src$timeout,
                             call = rlang::caller_env()) {
  kinds <- classify_variants(variants, call = call)
  lookup <- data.frame(alid = character(), chromosome = character(), position = integer(),
                       effect_allele = character(), other_allele = character(), rsid = character(),
                       store_id = character(), release_id = character(), stringsAsFactors = FALSE)
  jobs <- list()
  if (length(kinds$rsids) > 0) {
    # A lookup over more rsIDs, or Store Releases, than one Job takes is split like any query, and
    # runs with the same bounded concurrency inside the call's deadline.
    limits <- job_limits()
    pieces <- split_pieces(list(rsids = split_dim(length(kinds$rsids), limits$rsids),
                                releases = split_dim(length(releases), limits$releases)))
    requests <- lapply(pieces, function(p) {
      list(path = "api/variants/rsid",
           body = list(rsids = as.list(kinds$rsids[p$rsids]), releases = releases[p$releases]))
    })
    out <- run_window(src, requests, deadline, "variants", call)
    found <- Filter(function(o) nrow(o$rows) > 0, out)
    if (length(found) > 0) lookup <- bind_frames(lapply(found, `[[`, "rows"))
    jobs <- lapply(out, `[[`, "job")
  }
  map <- unique(lookup[c("rsid", "alid")])
  list(alids = unique(c(kinds$alids, map$alid)), map = map, lookup = lookup,
       unresolved = setdiff(kinds$rsids, map$rsid), job = if (length(jobs) > 0) jobs[[1]], jobs = jobs)
}

warn_unresolved <- function(ids, call = rlang::caller_env()) {
  n <- length(ids)
  og_warn(
    paste0(n, if (n == 1) " rsID is" else " rsIDs are", " in none of these Analyses' GWAS Stores, so ",
           if (n == 1) "it has" else "they have", " no Associations: ",
           paste0(utils::head(ids, 10), collapse = ", "), if (n > 10) ", ...", "."),
    class = "opengwasr_unresolved_variants", count = n, ids = ids, call = call
  )
}
