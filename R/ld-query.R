# The LD query engine: the one pipeline of every LD operation that sends variants to an LD Store
# (the matrix, proxies and `og_clump()`).
#
# These return matrices or LD rows, not Associations, so they don't go through association_query(),
# but they follow the same contract (R/query-plan.R) and the same shape: a query method is a thin
# spec (its endpoint, how its variants split into Jobs, its own body fields, and how its Jobs'
# rows assemble into its value), and everything else happens here, once: the call's deadline,
# variants to ALIDs (rsIDs resolved), the LD Store chosen, one Job per piece, the LD Store's absent
# variants (warned about, and kept in provenance) and provenance. As with association_query(), a
# method calls it directly and never reassigns an argument first: provenance reads them from its frame.

#' Run an LD query.
#'
#' @param src,path,variants,ld The method's `src`, the endpoint, and its `variants` and `ld`.
#'   `variants` is a character vector of ALIDs and rsIDs, or a data frame of Associations (its
#'   `alid`, and its `id` and Store Releases for matching `ld` = `NULL` by Assigned Ancestry).
#' @param spec One list of what is specific to the query (new hooks are added here, not as arguments):
#'   `pieces`, `function(n)`: the pieces `n` ALIDs are split into, as a list of index vectors, each
#'   within what one Job takes (every piece is one Job); `body`, the endpoint's own fields, the same
#'   in every Job (default none), or `function(alids, store)` giving them for a piece's ALIDs and the
#'   LD Store's row (clumping's scores and default `r2`); `assemble`, `function(results, ctx)`: the
#'   query's value from the Jobs' `list(rows, job)` (in piece order) and `ctx`, `list(alids, absent,
#'   store, choice)`: the ALIDs asked for, those the LD Store doesn't hold, its row of
#'   [og_ld_stores()] and how it was chosen; optionally `provenance`, `function(results, ctx)`:
#'   further provenance fields (clumping's `clump`).
#' @return The value `assemble()` returns with [og_provenance()] attached (`ld`, `ld_choice`,
#'   `absent`); under [og_submit()], the `og_jobs` handle whose collection returns the same.
#' @noRd
ld_query <- function(src, path, variants, ld, spec) {
  method <- parent.frame()
  fn <- sys.function(sys.parent())
  engine_query(src, method, fn, function(deadline) {
    prep <- ld_prepare(src, variants, ld, deadline, method)
    alids <- prep$alids
    requests <- lapply(if (length(alids) > 0) spec$pieces(length(alids)), function(idx) {
      own <- if (is.function(spec$body)) spec$body(alids[idx], prep$store) else spec$body
      list(path = path, body = c(list(store = prep$store$ld_store, alids = as.list(alids[idx])), own))
    })
    # The rsID lookup's Jobs read GWAS Store Releases and count no rows; the LD Store is `ld`, not a Store Release.
    lookup_jobs <- lapply(prep$jobs, utils::modifyList, list(total = 0, truncated = FALSE))
    store <- prep$store
    list(requests = requests, releases = NULL, finish = function(results) {
      # The LD Store says which of the variants it doesn't hold, in each Job; a variant it doesn't
      # hold is absent whichever Job asked.
      absent <- unique(unlist(lapply(results, function(r) r$job$metadata$absent)))
      left_out <- c(absent, prep$unresolved)
      if (length(left_out) > 0) warn_ld_absent(left_out, store$ld_store, method)
      ctx <- list(alids = alids, absent = absent, store = store, choice = prep$choice)
      value <- spec$assemble(results, ctx)
      ld_jobs <- lapply(results, function(r) { r$job$releases <- character(); r$job })
      list(value = value, jobs = c(lookup_jobs, ld_jobs),
           provenance = c(list(ld = store$ld_store, ld_choice = prep$choice, absent = left_out),
                          if (!is.null(spec$provenance)) spec$provenance(results, ctx)))
    })
  })
}

# The ALIDs to ask the LD Store about, and the LD Store: `list(alids, store, choice, unresolved,
# jobs)`. The LD Store is chosen before any Job runs, so a refusal costs none.
ld_prepare <- function(src, variants, ld, deadline, call) {
  if (is.data.frame(variants)) {
    if (!is.character(variants$alid)) {
      og_abort("A data frame of `variants` needs an `alid` column.", class = "opengwasr_argument", call = call)
    }
    kinds <- classify_variants(variants$alid, "variants$alid", call)
    ancestries <- if (is.null(ld) && "id" %in% names(variants)) associations_ancestries(src, variants, call)
  } else {
    kinds <- classify_variants(variants, call = call)
    ancestries <- NULL
  }
  chosen <- choose_ld_store(src, ld, ancestries, call)
  resolved <- resolve_variants(src, kinds, if (length(kinds$rsids) > 0) resolve_releases(src, NULL, call),
                               deadline, call)
  list(alids = resolved$alids, store = chosen$store, choice = chosen$choice,
       unresolved = resolved$unresolved, jobs = resolved$jobs)
}

# Kept: the API lists only the ALIDs the LD Store doesn't hold; an rsID that names none is as absent.
warn_ld_absent <- function(ids, ld_store, call) {
  n <- length(ids)
  og_warn(
    paste0(n, if (n == 1) " variant is" else " variants are", " not in LD Store ", ld_store,
           ", so ", if (n == 1) "it was" else "they were", " left out: ",
           paste0(utils::head(ids, 10), collapse = ", "), if (n > 10) ", ...", "."),
    class = "opengwasr_ld_absent_variants", count = n, ids = ids, ld_store = ld_store, call = call
  )
}
