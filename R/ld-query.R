# The LD query engine: the one pipeline of every LD operation (the call's deadline, variants to ALIDs,
# the LD Store chosen, one Job per piece, absent variants warned about, provenance; a PLINK Reference
# read locally, R/plink.R). Its `spec` is described once, in CONTRIBUTING.md, "Adding a query".

#' Run an LD query (CONTRIBUTING.md, "Adding a query"). `variants` is a character vector of ALIDs
#' and rsIDs, or a data frame of Associations (its `alid`, and its `id` for matching by Assigned
#' Ancestry when the Source has no LD resource). The LD is the Source's (ld_resource()). A method
#' calls it directly and never reassigns an argument first. `spec$sets`, if given, is a vector over
#' the rows of a data frame `variants` (og_clump()'s `id`): the rows of each value are answered on
#' their own, each set in its own LD Store, chosen by its own Assigned Ancestry.
#' @return The value `assemble()` returns with [og_provenance()] attached (`ld`, `ld_choice`,
#'   `absent`); under [og_submit()], the `og_jobs` handle whose collection returns the same.
#' @noRd
ld_query <- function(src, path, variants, spec) {
  method <- parent.frame()
  fn <- sys.function(sys.parent())
  engine_query(src, method, fn, function(deadline) {
    prep <- ld_prepare(src, variants, deadline, method, spec$sets)
    # A PLINK Reference is read on this machine: no Job to run, the same preparation and provenance.
    if (!is.null(prep$plink)) return(ld_query_plink(prep, spec, method))
    sets <- prep$sets
    # One Job per piece of each set's ALIDs, in the set's LD Store. Jobs are submitted grouped by LD
    # Store, as clumping top hits is (association_query()); each set's results keep piece order.
    pieces <- unlist(lapply(seq_along(sets), function(k) {
      n <- length(sets[[k]]$alids)
      lapply(if (n > 0) spec$pieces(n), function(idx) list(set = k, idx = idx))
    }), recursive = FALSE)
    store_of <- vapply(pieces, function(p) sets[[p$set]]$store$ld_store, "")
    pieces <- pieces[order(match(store_of, unique(store_of)))]
    requests <- lapply(pieces, function(p) {
      set <- sets[[p$set]]
      alids <- set$alids[p$idx]
      own <- if (is.function(spec$body)) spec$body(alids, set$store, set$rows) else spec$body
      list(path = path, body = c(list(store = set$store$ld_store, alids = as.list(alids)), own))
    })
    list(requests = requests, releases = NULL, finish = function(results) {
      # The LD Store says which of the variants it doesn't hold, in each Job; a variant it doesn't
      # hold is absent whichever Job asked.
      absent_of <- function(rs) unique(unlist(lapply(rs, function(r) r$job$metadata$absent)))
      in_set <- vapply(pieces, `[[`, 1L, "set")
      for (k in seq_along(sets)) {
        sets[[k]]$results <- results[in_set == k]
        sets[[k]]$absent <- absent_of(sets[[k]]$results)
      }
      absent <- absent_of(results)
      stores <- vapply(sets, function(s) s$store$ld_store, "")
      # A lookup of what the LD Store holds answers with its absences, so it has nothing to warn of.
      # Otherwise one warning for each LD Store (one for every query but og_clump()'s per Analysis).
      if (!isTRUE(spec$absence_is_answer)) {
        for (store in unique(stores)) {
          left_out <- unique(c(unlist(lapply(sets[stores == store], `[[`, "absent")), if (store == stores[[1]]) prep$unresolved))
          if (length(left_out) > 0) warn_ld_absent(left_out, store, method)
        }
      }
      ctx <- list(alids = prep$alids, absent = absent, store = prep$store, choice = prep$choice,
                  variants = variants, rsid_map = prep$rsid_map, unresolved = prep$unresolved, sets = sets)
      value <- spec$assemble(results, ctx)
      ld_jobs <- lapply(results, function(r) { r$job$releases <- character(); r$job })
      # The LD Store is `ld`, not a Store Release.
      list(value = value, jobs = c(lookup_jobs(prep$jobs), ld_jobs),
           provenance = c(list(ld = unique(stores), ld_choice = unique(vapply(sets, `[[`, "", "choice")),
                               ld_from = prep$from, absent = c(absent, prep$unresolved)),
                          if (!is.null(spec$provenance)) spec$provenance(results, ctx)))
    })
  })
}

# The ALIDs to ask the LD Store about, and the LD Store: `list(alids, store, choice, from, unresolved,
# jobs, sets)`. The LD Store is chosen before any Job runs, so a refusal costs none. The LD is the
# Source's LD resource (ld_resource()): only with none is the Assigned Ancestry matched. `sets`
# (ld_sets()) has one set of every variant unless `by` splits a data frame's rows; `store` and
# `choice` are the first set's.
ld_prepare <- function(src, variants, deadline, call, by = NULL) {
  resource <- ld_resource(src, call)
  if (is.data.frame(variants)) {
    if (!is.character(variants$alid)) {  # Kept: an R argument's shape
      og_abort("A data frame of `variants` needs an `alid` column.", class = "opengwasr_argument", call = call)
    }
    kinds <- classify_variants(variants$alid, "variants$alid", call)
    ancestries <- if (resource$from == "ancestry" && "id" %in% names(variants)) associations_ancestries(src, variants, call)
  } else {
    kinds <- classify_variants(variants, call = call)
    ancestries <- NULL
  }
  sets <- ld_sets(src, variants, by, ancestries, call)
  chosen <- sets[[1]]$chosen
  # A PLINK Reference (a Local Source's) is matched by rsID and read locally (R/plink.R). Only the
  # queries with a `local` spec reach here with one: a Local Source refuses the others first.
  if (!is.null(chosen$plink)) {
    return(c(ld_prepare_plink(src, variants, kinds, chosen, deadline, call), list(sets = sets)))
  }
  resolved <- resolve_variants(src, kinds, if (length(kinds$rsids) > 0) resolve_releases(src, NULL, call),
                               deadline, call)
  for (k in seq_along(sets)) {
    rows <- sets[[k]]$rows
    sets[[k]]$alids <- if (is.null(rows)) resolved$alids else resolved$alids[resolved$alids %in% variants$alid[rows]]
  }
  list(alids = resolved$alids, store = chosen$store, choice = chosen$choice, from = chosen$from,
       unresolved = resolved$unresolved, jobs = resolved$jobs, rsid_map = resolved$map, sets = sets)
}

# The sets an LD query answers, each `list(id, rows, chosen, store, choice)`: one set of every
# variant when `by` is NULL (`id` NA; `rows`, every row of a data frame, else NULL), or the rows of
# each value of `by` in order of first appearance, each with the LD Store of its own Assigned
# Ancestry (choose_ld_store_each()). `ancestries` is ld_prepare()'s, named by Analysis id.
ld_sets <- function(src, variants, by, ancestries, call) {
  set <- function(id, rows, chosen) list(id = id, rows = rows, chosen = chosen, store = chosen$store, choice = chosen$choice)
  if (length(by) == 0) {  # NULL, or a data frame with no rows
    rows <- if (is.data.frame(variants)) seq_len(nrow(variants))
    return(list(set(NA_character_, rows, choose_ld_store(src, ancestries, call))))
  }
  ids <- unique(as.character(by))
  key <- match(as.character(by), ids)
  ancestry <- stats::setNames(if (is.null(ancestries)) rep(NA_character_, length(ids)) else unname(ancestries[ids]), ids)
  each <- choose_ld_store_each(src, ancestry, call)
  lapply(seq_along(ids), function(k) set(ids[[k]], which(key == k), each$chosen[[each$of[[k]]]]))
}

# Kept: the API lists only the ALIDs the LD Store doesn't hold; an rsID that names none is as absent.
warn_ld_absent <- function(ids, ld_store, call) {
  n <- length(ids)
  og_warn(
    paste0(n, if (n == 1) " variant is" else " variants are", " not in LD Store ", ld_store,
           ", so ", if (n == 1) "it was" else "they were", " left out: ",
           some_ids(ids, 10), "."),
    class = "opengwasr_ld_absent_variants", count = n, ids = ids, ld_store = ld_store, call = call
  )
}
