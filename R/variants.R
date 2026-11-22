# Variant resolution: the one place that turns what a user calls variants into ALIDs.
#
# A user names variants by ALID (`chromosome:position:A1:A2`) or by rsID, mixed. The GWAS Stores
# are queried by ALID, so every query function that takes `variants` calls `classify_variants()`
# once and then `resolve_variants()`. An rsID is resolved through POST /api/variants/rsid (a Job)
# in the Store Releases the query uses, and stands for EVERY ALID it names there: a multiallelic
# site names several, and one position can carry two spellings of the same indel. opengwasr never
# picks one (ADR 0004: no proxies, no guessing). An rsID no Store Release holds is "unresolved",
# reported by `warn_unresolved()`, never dropped quietly. `og_variants_rsid()` and
# `og_variants_chrpos()` (R/variant-lookups.R) are the lookups themselves, on the same helpers.

#' Classify a user's variants: ALIDs or rsIDs, nothing else.
#'
#' An ALID is refused unless it is one `og_alid()` could have made: a canonical chromosome, a
#' whole position above 0, two different A/C/G/T alleles, and, when the alleles have the same length,
#' in alphabetical order. (An ALID in any other form names no variant in any GWAS Store.)
#'
#' @param variants A character vector of ALIDs and rsIDs.
#' @return `list(alids, rsids, given)`, of class `og_variant_kinds`: the unique ALIDs and unique
#'   (lower-case) rsIDs, in order of first appearance, and `given`, every variant once in that order. Anything else, or a missing value, raises
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
  # `given`: every variant once, in the order given (rsIDs in lower case).
  structure(list(alids = unique(variants[is_alid]), rsids = unique(tolower(variants[is_rsid])),
                 given = unique(ifelse(is_rsid, tolower(variants), variants))),
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
#' @return `list(alids, map, lookup, unresolved, jobs)`: every ALID to query (those given, then those the
#'   rsIDs name, unique); `ordered`, the same in the order the variants were given (an rsID standing
#'   for the ALIDs it names, in the API's order); `map`, a data frame of `rsid` and `alid`, one row per ALID an rsID names;
#'   `lookup`, the API's rows (`alid`, `chromosome`, `position`, `effect_allele`, `other_allele`,
#'   `rsid`, `store_id`, `release_id`; none with no rsIDs); `unresolved`, the rsIDs no Store Release
#'   holds; `jobs`, the `job` of every Job the lookup ran (more than one when it was split; empty with
#'   no rsIDs).
#' @noRd
resolve_variants <- function(src, variants, releases, deadline = job_now() + src$timeout,
                             call = rlang::caller_env()) {
  kinds <- classify_variants(variants, call = call)
  lookup <- empty_lookup()
  jobs <- list()
  if (length(kinds$rsids) > 0) {
    # A lookup over more rsIDs, or Store Releases, than one Job takes is split like any query, and
    # runs with the same bounded concurrency inside the call's deadline.
    out <- run_window(src, lookup_requests("api/variants/rsid", "rsids", kinds$rsids, releases, "rsids"),
                      deadline, call)
    lookup <- lookup_rows(out)
    jobs <- lapply(out, `[[`, "job")
  }
  map <- unique(lookup[c("rsid", "alid")])
  # In the order the variants were given: an ALID, or the ALIDs its rsID names (in the API's order).
  ordered <- unlist(lapply(kinds$given, function(v) if (v %in% kinds$alids) v else map$alid[map$rsid == v]))
  list(alids = unique(c(kinds$alids, map$alid)), ordered = unique(ordered), map = map, lookup = lookup,
       unresolved = setdiff(kinds$rsids, map$rsid), jobs = jobs)
}

# The variant lookups (rsIDs here, positions in og_variants_chrpos()) share one shape: values to look
# up (`field`, at most `limit` a Job) in Store Releases (at most 10 GWAS Stores a Job), one row per
# variant a value matches in each Store Release. A lookup over more is split like any query.
lookup_requests <- function(path, field, values, releases, limit) {
  limits <- job_limits()
  pieces <- split_pieces(list(values = split_dim(length(values), limits[[limit]]),
                              releases = split_dim(length(releases), limits$releases)))
  lapply(pieces, function(p) {
    list(path = path, body = stats::setNames(list(as.list(values[p$values]), releases[p$releases]),
                                             c(field, "releases")))
  })
}

# The rows of a lookup's Jobs (one `list(rows, job)` each), in request order.
lookup_rows <- function(results) {
  found <- Filter(function(r) nrow(r) > 0, lapply(results, `[[`, "rows"))
  if (length(found) == 0) empty_lookup() else bind_frames(found)
}

empty_lookup <- function() {
  data.frame(alid = character(), chromosome = character(), position = integer(),
             effect_allele = character(), other_allele = character(), rsid = character(),
             store_id = character(), release_id = character(), stringsAsFactors = FALSE)
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
