# The Association-query engine: the one pipeline of every query that returns Associations. What a
# query method passes it (`body`, `variants`, `analyses_per_job`, `order`, `truncated`, `spec`) and
# the contributions it builds from them are described once, in CONTRIBUTING.md, "Adding a query".

#' Run a query that returns Associations (CONTRIBUTING.md, "Adding a query").
#'
#' A query method calls it directly and never reassigns an argument first: provenance reads the
#' method's arguments from its frame.
#' @return The shaped, ordered Associations with [og_provenance()] attached; under [og_submit()],
#'   the `og_jobs` handle whose collection returns the same.
#' @noRd
association_query <- function(src, path, analyses, observed_only, original_units, releases,
                              body = list(), variants = NULL, analyses_per_job = NULL,
                              order = c("position", "api", "jobs"), truncated = NULL, spec = list()) {
  method <- parent.frame()
  fn <- sys.function(sys.parent())
  order <- match.arg(order)
  engine_query(src, method, fn, function(deadline) {
    every <- isTRUE(spec$all_analyses) && is.null(analyses)  # no Analyses named: look in all of them
    check_query_arguments(analyses, observed_only, original_units, method, optional = every)
    kinds <- if (!is.null(variants)) classify_variants(variants, call = method)
    limits <- job_limits()
    scope <- query_scope(src, if (!every) analyses, resolve_releases(src, releases, method),
                         original_units, limits, method)
    releases <- scope$releases
    info <- scope$info
    # Clumping chooses its LD Stores now, before any Job runs, so a refusal costs none.
    clump <- if (!is.null(spec$clump)) clump_contribution(src, spec$clump, info, method)
    # Preparation: rsIDs name ALIDs only in these Store Releases.
    resolved <- if (!is.null(kinds)) resolve_variants(src, kinds, releases, deadline, method)
    # A bigger request than one Job takes is split, one contribution per part of the request, and put
    # back together below. A new kind of query adds a contribution, never a branch below.
    contributions <- c(
      list(scope = scope),
      if (!is.null(kinds)) list(variants = variants_contribution(resolved, isTRUE(spec$one_variant), limits)),
      if (!is.null(spec$interval)) list(interval = interval_contribution(spec$interval)),
      if (!is.null(clump)) list(clump = clump)
    )
    # A contribution may take fewer Analyses a Job, and say what a truncation warning says.
    per_job <- min(c(analyses_per_job %||% limits$analyses, unlist(lapply(contributions, `[[`, "analyses_per_job"))))
    contributions$scope$dim <- scope$split(per_job)
    for (contribution in contributions) truncated <- contribution$truncated %||% truncated
    # A contribution that follows another's dimension (`along`) has none of its own.
    dims <- Filter(Negate(is.null), lapply(contributions, `[[`, "dim"))
    pieces <- if (is.null(kinds) || length(resolved$alids) > 0) split_pieces(dims) else list()
    # Jobs are submitted grouped as a contribution asks (clumping: by LD Store); the results are put
    # back in piece order before anything else, so the answer and provenance don't depend on it.
    submitted <- submit_order(contributions, pieces)
    requests <- lapply(pieces[submitted], function(p) {
      list(path = path, body = request_body(contributions, p, body, observed_only))
    })

    # Everything the answer is shaped from is pinned in this closure now: og_job_result() finishes
    # the same plan later, however long after, and gets the same answer.
    list(requests = requests, releases = releases, finish = function(results) {
      results[submitted] <- results
      jobs <- job_counts(results, !is.null(truncated), method)
      rows <- result_rows(results)
      # With no Analyses named, the rows say which there are: one lookup, of those only.
      if (is.null(info)) info <- result_analyses(src, rows, releases, method)
      # Warnings name the user's call while it runs (not when og_job_result() collects it later).
      if (any(vapply(jobs, function(j) j$truncated, NA))) {
        og_warn(truncated(info$id), class = "opengwasr_truncated", n_analyses = nrow(info), call = method)
      }
      if (!is.null(kinds) && length(resolved$unresolved) > 0) warn_unresolved(resolved$unresolved, method)
      shaped <- shape_associations(rows, info, original_units, method)
      ids <- union(info$id, shaped$id)  # Analyses the catalogue doesn't list come after, as they appear
      shaped <- switch(order, api = order_by_analysis(shaped, ids), jobs = shaped,
                       order_associations(shaped, ids))
      # A contribution may finish what it started: columns, provenance fields, warnings, and which
      # Jobs it cut rows off (`truncated`, one flag per Job, in piece order).
      provenance <- list()
      for (contribution in contributions) {
        if (is.null(contribution$finish)) next
        done <- contribution$finish(shaped, results, pieces, method)
        shaped <- done$value
        for (i in which(done$truncated %||% logical())) jobs[[i]]$truncated <- TRUE
        provenance <- c(provenance, done$provenance)
      }
      # The rsID lookup's Jobs are in provenance too.
      list(value = shaped, jobs = c(lookup_jobs(resolved$jobs), jobs), provenance = provenance)
    })
  })
}

# The Analyses are the scope contribution (CONTRIBUTING.md): named ones (grouped by GWAS Store,
# narrowing the Store Releases) or, with `analyses = NULL`, every Analysis of the Store Releases
# (split by Store Release; the metadata comes with the result, `info` is NULL). Its dimension is
# made once every contribution has said how many Analyses a Job takes (`split(per_job)`).
query_scope <- function(src, analyses, releases, original_units, limits, call) {
  if (is.null(analyses)) {
    return(list(split = function(per_job) split_dim(length(releases), limits$releases), info = NULL,
                releases = releases, body = function(chunk) list(releases = releases[chunk])))
  }
  info <- analyses_lookup(src, analyses, releases, call)
  if (original_units) check_original_units(info, call)  # refuse before a Job runs, not after
  releases <- releases_of_analyses(releases, info)
  list(split = function(per_job) split_dim(nrow(info), per_job, group = info$store_id, group_limit = limits$releases),
       info = info, releases = releases,
       body = function(chunk) list(analyses = as.list(info$id[chunk]),
                                   releases = releases_of_analyses(releases, info[chunk, , drop = FALSE])))
}

# The ALIDs to query: `variants` in each Job, or, for a query of one variant, `variant`.
# One ALID a Job is run in the order the variants were given, which is the order of the rows.
variants_contribution <- function(resolved, one_variant, limits) {
  alids <- if (one_variant) resolved$ordered else resolved$alids
  list(dim = split_dim(length(alids), if (one_variant) 1 else limits$variants),
       body = function(chunk) if (one_variant) list(variant = alids[[chunk]]) else list(variants = as.list(alids[chunk])))
}

# A range of one chromosome in windows (`split_interval()`).
interval_contribution <- function(interval) {
  list(dim = split_interval(interval$start, interval$end, interval$limit),
       body = function(chunk) list(chromosome = interval$chromosome, start = as.integer(chunk[[1]]),
                                   end = as.integer(chunk[[2]])))
}

# The order pieces are submitted in: stably grouped by each contribution's `group(chunk)`, groups
# in order of first appearance; piece order when no contribution groups.
submit_order <- function(contributions, pieces) {
  keys <- lapply(names(contributions), function(n) {
    group <- contributions[[n]]$group
    if (is.null(group)) return(NULL)
    key <- vapply(pieces, function(p) as.character(group(p[[contributions[[n]]$along %||% n]])), "")
    match(key, unique(key))
  })
  do.call(order, c(Filter(Negate(is.null), keys), list(seq_along(pieces))))
}

# A Job's body: the contributions' fields, in a fixed order (the order is part of the request, and so
# of what a recording replays): `variants`/`variant`, `analyses`, the endpoint's own `body`, any other
# contributed field, then `observed_only` and `releases`; the API's defaults are never relied on.
request_body <- function(contributions, piece, body, observed_only) {
  fields <- unlist(lapply(names(contributions), function(n) {
    contributions[[n]]$body(piece[[contributions[[n]]$along %||% n]])
  }), recursive = FALSE)
  # A contributed field of the endpoint's own replaces it, in its place (clumping by PLINK fetches
  # more top hits than `limit`).
  own <- intersect(names(body), names(fields))
  body[own] <- fields[own]
  fields <- fields[setdiff(names(fields), own)]
  first <- c("variants", "variant", "analyses")
  last <- c("observed_only", "releases")
  c(fields[intersect(first, names(fields))], body,
    fields[setdiff(names(fields), c(first, last))],
    list(observed_only = observed_only), fields["releases"])
}

# The metadata of the Analyses a result's rows are in, in order of first appearance (with every
# Analysis searched there is no list asked for). Kept: the API's rows carry no sample size or effect
# scale, so shaping needs the Analyses' own metadata. What can be queried is the API's say, not
# opengwasr's: an Analysis the catalogue doesn't list (yet) is left out here, and its rows are
# shaped with that metadata NA (shape_associations()). `original_units` is refused for it there.
result_analyses <- function(src, rows, releases, call) {
  ids <- unique(canonical_ids(rows))
  tryCatch(
    analyses_lookup(src, ids, releases, call),
    opengwasr_analysis_not_found = function(e) {
      known <- setdiff(ids, e$ids)
      if (length(known) == length(ids)) stop(e)  # not about the result's own Analyses
      analyses_lookup(src, known, releases, call)
    }
  )
}

# The query method's generic and arguments, in order (so an argument a method gains is recorded
# without anyone remembering to), read before the engine does anything. Kept: the check that `method`
# is a query method's own frame; read from any other frame they would be another function's,
# silently.
method_arguments <- function(method, fn) {
  if (!exists(".Generic", envir = method, inherits = FALSE)) {
    og_abort(c("A query engine (`association_query()`, `ld_query()`) must be called directly by a query method (an S3 method).",
               i = "Provenance records the method's arguments from its frame. This is a bug in opengwasr."),
             class = "opengwasr_internal")
  }
  list(generic = get(".Generic", envir = method),
       args = mget(setdiff(names(formals(fn)), c("src", "...")), envir = method))
}

# The call as provenance records it: `releases` as resolved, arguments left NULL dropped. The query
# text leaves out the long ones: `analyses`, `variants`, `data`, `releases`, `rsids` and `positions`.
query_record <- function(given, releases) {
  args <- given$args
  args$releases <- releases
  # A data frame of Associations given as an argument (to an LD query) is recorded as the variants
  # and Analyses it names, not the whole frame.
  args <- lapply(args, function(a) {
    if (is.data.frame(a)) list(alids = unique(a$alid), ids = unique(a$id))
    else if (is_plink_reference(a)) a$path  # its path names it: the PLINK it ran is in provenance
    else a
  })
  args <- Filter(Negate(is.null), args)
  shown <- args[setdiff(names(args), c("analyses", "variants", "data", "releases", "rsids", "positions"))]
  list(query = call_text(given$generic, shown), args = args)
}

# Each Job's `total` and `truncated`. An endpoint that cuts rows off at a limit (`cuts_off`) says
# them, and they are checked against the rows; any other's are its rows, none cut off.
job_counts <- function(results, cuts_off, call) {
  lapply(results, function(r) {
    n <- nrow(r$rows)
    if (!cuts_off) return(utils::modifyList(r$job, list(total = n, truncated = FALSE)))
    check_job_metadata(r$job, n, call)
    r$job
  })
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
check_query_arguments <- function(analyses, observed_only, original_units, call = rlang::caller_env(),
                                  optional = FALSE) {
  bad <- function(what) og_abort(what, class = "opengwasr_argument", call = call)
  every <- optional && is.null(analyses)  # `analyses = NULL` was allowed by the query: every Analysis
  if (!every && (!is.character(analyses) || length(analyses) == 0 || anyNA(analyses) ||
                 any(!nzchar(analyses)))) {
    bad("`analyses` must be a character vector of Analysis identifiers, with no missing values.")
  }
  if (!is_flag(observed_only)) bad("`observed_only` must be `TRUE` or `FALSE`.")
  if (!is_flag(original_units)) bad("`original_units` must be `TRUE` or `FALSE`.")
}

is_flag <- function(x) is.logical(x) && length(x) == 1 && !is.na(x)
