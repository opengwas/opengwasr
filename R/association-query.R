# The Association-query engine: the one pipeline of every query that returns Associations.
#
# A query method checks the arguments only it has, then returns `association_query()` with its
# endpoint and what is specific to it. Everything else happens here, once, for every such query:
# the call's deadline and query depth, Store Releases and Analyses, the original-units check, rsID
# resolution, splitting into Jobs, running them (or submitting them, under og_submit()), combining
# the pieces, the Jobs' metadata, shaping, ordering and provenance. See CONTRIBUTING.md, "Adding a
# query", for a worked example.
#
# What a query may vary is deliberately small. When a later query needs more (#11's 10 Mb interval
# and one-variant PheWAS, #13's per-Analysis clumping), it comes as one spec list argument, not as
# more named arguments.

#' Run a query that returns Associations.
#'
#' A query method returns its value, and must call it itself, not through a helper: provenance
#' records the method's arguments, read from its frame (anything else is refused as an internal
#' error), so a method never reassigns an argument before the call. Request bodies are `variants` (with `variants`), `analyses`, then `body`'s fields,
#' then `observed_only` and `releases`; the API's own defaults are never relied on.
#'
#' @param src The Source.
#' @param path The endpoint, such as `"api/tophits"`.
#' @param analyses,observed_only,original_units,releases The method's arguments of these names, as
#'   given. They are checked here.
#' @param body The endpoint's own fields, the same in every Job, such as `threshold` and `limit`.
#' @param variants `NULL`, or the method's `variants` (rsIDs and ALIDs, checked here): rsIDs are
#'   resolved to ALIDs in the Analyses' Store Releases first, and each Job takes some of the ALIDs as
#'   `variants`. An rsID none of them holds gives `opengwasr_unresolved_variants`.
#' @param analyses_per_job The most Analyses one Job takes, if fewer than the API's limit.
#' @param order `"position"`: Analyses in the order asked, then chromosome, position, ALID.
#'   `"api"`: Analyses in the order asked, each in the order the API returned its rows.
#' @param truncated `NULL` when the endpoint returns every Association it matches (one over the
#'   row limit fails instead): a Job's `total` is its rows. Otherwise the endpoint cuts rows off at a
#'   limit and must say so: every Job's `total` and `truncated` are required and checked against its
#'   rows, and when any Job was cut off the call warns (`opengwasr_truncated`, with `n_analyses`)
#'   with the message `truncated(ids)` gives for the query's Analysis ids.
#' @return The shaped, ordered Associations with [og_provenance()] attached; under [og_submit()],
#'   the `og_jobs` handle whose collection returns the same.
#' @noRd
association_query <- function(src, path, analyses, observed_only, original_units, releases,
                              body = list(), variants = NULL, analyses_per_job = NULL,
                              order = c("position", "api"), truncated = NULL) {
  method <- parent.frame()
  given <- method_arguments(method, sys.function(sys.parent()))
  deadline <- local_query(src)
  order <- match.arg(order)
  check_query_arguments(analyses, observed_only, original_units, method)
  kinds <- if (!is.null(variants)) classify_variants(variants, call = method)
  releases <- resolve_releases(src, releases, method)
  info <- analyses_lookup(src, analyses, releases, method)
  if (original_units) check_original_units(info, method)  # refuse before a Job runs, not after
  releases <- releases_of_analyses(releases, info)

  # Preparation: rsIDs name ALIDs only in these Store Releases. The lookup's Jobs are in provenance;
  # their rows are variants, not Associations, so they count none.
  jobs <- list()
  if (!is.null(kinds)) {
    resolved <- resolve_variants(src, kinds, releases, deadline, method)
    jobs <- lapply(resolved$jobs, utils::modifyList, list(total = 0, truncated = FALSE))
  }
  # One Job takes a limited number of Analyses from a limited number of GWAS Stores, and of variants:
  # a bigger request is split by those (Analyses grouped by GWAS Store) and put back together below.
  limits <- job_limits()
  dims <- list(analyses = split_dim(nrow(info), analyses_per_job %||% limits$analyses,
                                    group = info$store_id, group_limit = limits$releases))
  if (!is.null(kinds)) dims$variants <- split_dim(length(resolved$alids), limits$variants)
  pieces <- if (is.null(kinds) || length(resolved$alids) > 0) split_pieces(dims) else list()
  requests <- lapply(pieces, function(p) {
    list(path = path, body = c(
      if (!is.null(kinds)) list(variants = as.list(resolved$alids[p$variants])),
      list(analyses = as.list(info$id[p$analyses])), body,
      list(observed_only = observed_only, releases = piece_releases(releases, info$store_id, p$analyses))))
  })

  # Everything the answer is shaped from is pinned in this closure now: og_job_result() finishes
  # the same plan later, however long after, and gets the same answer.
  record <- query_record(given, releases)
  plan <- query_plan(requests, finish = function(results) {
    for (i in seq_along(results)) {
      n <- nrow(results[[i]]$rows)
      if (is.null(truncated)) {
        results[[i]]$job <- utils::modifyList(results[[i]]$job, list(total = n, truncated = FALSE))
      } else {
        check_job_metadata(results[[i]]$job, n)
      }
    }
    all_jobs <- c(jobs, lapply(results, `[[`, "job"))
    # Warnings name the user's call while it runs (not when og_job_result() collects it later).
    if (any(vapply(all_jobs, function(j) j$truncated, NA))) {
      og_warn(truncated(info$id), class = "opengwasr_truncated", n_analyses = nrow(info), call = method)
    }
    if (!is.null(kinds) && length(resolved$unresolved) > 0) warn_unresolved(resolved$unresolved, method)
    found <- Filter(function(r) nrow(r) > 0, lapply(results, `[[`, "rows"))
    rows <- if (length(found) == 0) data.frame() else bind_frames(found)
    shaped <- shape_associations(rows, info, original_units)
    shaped <- if (order == "api") order_by_analysis(shaped, info$id) else order_associations(shaped, info$id)
    attach_provenance(shaped, query = record$query, args = record$args, jobs = all_jobs)
  })
  run_query(src, plan, deadline, method)
}

# The query method's generic and arguments, in order (so an argument a method gains is recorded
# without anyone remembering to), read before the engine does anything. Kept: the check that `method`
# is a query method's own frame; read from any other frame they would be another function's,
# silently.
method_arguments <- function(method, fn) {
  if (!exists(".Generic", envir = method, inherits = FALSE)) {
    og_abort(c("`association_query()` must be called directly by a query method (an S3 method).",
               i = "Provenance records the method's arguments from its frame. This is a bug in opengwasr."),
             class = "opengwasr_internal")
  }
  list(generic = get(".Generic", envir = method),
       args = mget(setdiff(names(formals(fn)), c("src", "...")), envir = method))
}

# The call as provenance records it: `releases` as resolved, arguments left NULL dropped. The query
# text leaves out the long ones: `analyses`, `variants` and `releases`.
query_record <- function(given, releases) {
  args <- given$args
  args$releases <- releases
  args <- Filter(Negate(is.null), args)
  shown <- args[setdiff(names(args), c("analyses", "variants", "releases"))]
  list(query = provenance_query(given$generic, shown), args = args)
}

# An endpoint that cuts rows off at a limit says how many it returned and whether it cut any off. An
# answer that doesn't say, or says something the rows contradict, can't be trusted not to be partial.
check_job_metadata <- function(job, rows, call = rlang::caller_env()) {
  total <- job$total
  truncated <- job$truncated
  whole <- is.numeric(total) && length(total) == 1 && !is.na(total) && total >= 0 && total == round(total)
  ok <- whole && is.logical(truncated) && length(truncated) == 1 && !is.na(truncated) &&
    total >= rows && (truncated || total == rows)
  if (!ok) {
    job_invalid("Its `total` and `truncated` are missing or don't fit the rows that were read.",
                job$job_id, call)
  }
}

# The arguments every Association query shares: `analyses`, `observed_only` and `original_units`.
# Kept: R argument types, which the API can't see (they are never sent as typed).
check_query_arguments <- function(analyses, observed_only, original_units, call = rlang::caller_env()) {
  bad <- function(what) og_abort(what, class = "opengwasr_argument", call = call)
  if (!is.character(analyses) || length(analyses) == 0 || anyNA(analyses) || any(!nzchar(analyses))) {
    bad("`analyses` must be a character vector of Analysis identifiers, with no missing values.")
  }
  if (!is_flag(observed_only)) bad("`observed_only` must be `TRUE` or `FALSE`.")
  if (!is_flag(original_units)) bad("`original_units` must be `TRUE` or `FALSE`.")
}

is_flag <- function(x) is.logical(x) && length(x) == 1 && !is.na(x)
