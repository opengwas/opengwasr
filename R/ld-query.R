# The LD query engine: the one pipeline of every LD operation (the call's deadline, variants to ALIDs,
# the LD Store chosen, one Job per piece, absent variants warned about, provenance; a PLINK Reference
# read locally, R/plink.R). Its `spec` is described once, in CONTRIBUTING.md, "Adding a query".

#' Run an LD query (CONTRIBUTING.md, "Adding a query"). `variants` is a character vector of ALIDs
#' and rsIDs, or a data frame of Associations (its `alid`, and its `id` for matching `ld = NULL` by
#' Assigned Ancestry). A method calls it directly and never reassigns an argument first.
#' @return The value `assemble()` returns with [og_provenance()] attached (`ld`, `ld_choice`,
#'   `absent`); under [og_submit()], the `og_jobs` handle whose collection returns the same.
#' @noRd
ld_query <- function(src, path, variants, ld, spec) {
  src <- src %||% no_source()  # NULL: a PLINK Reference that looks nothing up (R/source.R)
  method <- parent.frame()
  fn <- sys.function(sys.parent())
  engine_query(src, method, fn, function(deadline) {
    prep <- ld_prepare(src, variants, ld, deadline, method, spec)
    alids <- prep$alids
    # A PLINK Reference is read on this machine: no Job to run, the same preparation and provenance.
    if (!is.null(prep$plink)) return(ld_query_plink(prep, spec, method))
    requests <- lapply(if (length(alids) > 0) spec$pieces(length(alids)), function(idx) {
      own <- if (is.function(spec$body)) spec$body(alids[idx], prep$store) else spec$body
      list(path = path, body = c(list(store = prep$store$ld_store, alids = as.list(alids[idx])), own))
    })
    store <- prep$store
    list(requests = requests, releases = NULL, finish = function(results) {
      # The LD Store says which of the variants it doesn't hold, in each Job; a variant it doesn't
      # hold is absent whichever Job asked.
      absent <- unique(unlist(lapply(results, function(r) r$job$metadata$absent)))
      left_out <- c(absent, prep$unresolved)
      # A lookup of what the LD Store holds answers with its absences, so it has nothing to warn of.
      if (length(left_out) > 0 && !isTRUE(spec$absence_is_answer)) warn_ld_absent(left_out, store$ld_store, method)
      ctx <- list(alids = alids, absent = absent, store = store, choice = prep$choice,
                  variants = variants, rsid_map = prep$rsid_map, unresolved = prep$unresolved)
      value <- spec$assemble(results, ctx)
      ld_jobs <- lapply(results, function(r) { r$job$releases <- character(); r$job })
      # The LD Store is `ld`, not a Store Release.
      list(value = value, jobs = c(lookup_jobs(prep$jobs), ld_jobs),
           provenance = c(list(ld = store$ld_store, ld_choice = prep$choice, absent = left_out),
                          if (!is.null(spec$provenance)) spec$provenance(results, ctx)))
    })
  })
}

# The ALIDs to ask the LD Store about, and the LD Store: `list(alids, store, choice, unresolved,
# jobs)`. The LD Store is chosen before any Job runs, so a refusal costs none.
ld_prepare <- function(src, variants, ld, deadline, call, spec) {
  if (is.data.frame(variants)) {
    if (!is.character(variants$alid)) {  # Kept: an R argument's shape
      og_abort("A data frame of `variants` needs an `alid` column.", class = "opengwasr_argument", call = call)
    }
    kinds <- classify_variants(variants$alid, "variants$alid", call)
    ancestries <- if (is.null(ld) && "id" %in% names(variants)) associations_ancestries(src, variants, call)
  } else {
    kinds <- classify_variants(variants, call = call)
    ancestries <- NULL
  }
  chosen <- choose_ld_store(src, ld, ancestries, call)
  # A PLINK Reference is matched by rsID and read locally (R/plink.R).
  if (!is.null(chosen$plink)) {
    # Refused before anything is matched or looked up.
    if (is.null(spec$local)) refuse_plink(call)
    return(ld_prepare_plink(src, variants, kinds, chosen, deadline, call))
  }
  resolved <- resolve_variants(src, kinds, if (length(kinds$rsids) > 0) resolve_releases(src, NULL, call),
                               deadline, call)
  list(alids = resolved$alids, store = chosen$store, choice = chosen$choice,
       unresolved = resolved$unresolved, jobs = resolved$jobs, rsid_map = resolved$map)
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
