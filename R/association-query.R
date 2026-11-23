# The Association-query engine: the one pipeline of every query that returns Associations.
#
# A query method checks the arguments only it has, then returns `association_query()` with its
# endpoint and what is specific to it. Everything else happens here, once, for every such query:
# the call's deadline and query depth, Store Releases and Analyses, the original-units check, rsID
# resolution, splitting into Jobs, running them (or submitting them, under og_submit()), combining
# the pieces, the Jobs' metadata, shaping, ordering and provenance. See CONTRIBUTING.md, "Adding a
# query", for a worked example.
#
# What a query may vary is deliberately small. What a query needs beyond the named arguments comes as
# one `spec` list (#11's one-variant PheWAS, every-Analysis search and 10 Mb interval; #13's
# per-Analysis clumping), never as more named arguments.

#' Run a query that returns Associations.
#'
#' A query method returns its value, and must call it itself, not through a helper: provenance
#' records the method's arguments, read from its frame (anything else is refused as an internal
#' error), so a method never reassigns an argument before the call. Request bodies are `variants` (or
#' `variant`), `analyses`, then `body`'s fields, then any field a spec contributes (`chromosome`,
#' `start`, `end`), then `observed_only` and `releases`; the API's own defaults are never relied on.
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
#'   `"api"`: Analyses in the order asked, each in the order the API returned its rows. `"jobs"`: the
#'   rows as the Jobs returned them, Jobs in request order (for a query with no Analyses asked for).
#' @param spec A list of the hooks a query may add; every field is optional:
#'   * `all_analyses = TRUE`: `analyses` may be `NULL`, meaning every Analysis of the Store Releases
#'     (sent as no `analyses`; Jobs are split by Store Release). The Analyses' metadata is then looked
#'     up once, after the rows, for the Analyses the rows are in (one lookup in `finish`, however many
#'     Jobs; never the catalogue). `original_units` can only be refused after the Jobs ran.
#'   * `one_variant = TRUE`: with `variants`, each Job takes one ALID, sent as `variant` (so an
#'     rsID naming several ALIDs runs one Job per ALID).
#'   * `interval = list(chromosome, start, end, limit)`: the Jobs cover consecutive windows of at most
#'     `limit` positions, sent as `chromosome`, `start` and `end` (integers).
#'   * `clump = list(ld, r2, kb, limit, threshold)`: with a PLINK Reference as `ld`, the Jobs fetch
#'     unclumped top hits and the rows are clumped on this machine (`plink_clump_contribution()`).
#'     Otherwise each Job (one Analysis: `analyses_per_job = 1`) is clumped on the
#'     server in its Analysis's LD Store, sent as `clump` (`clump_contribution()`, R/clump.R); the
#'     rows gain the clumping columns and provenance its `clump`.
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
                              order = c("position", "api", "jobs"), truncated = NULL, spec = list()) {
  method <- parent.frame()
  fn <- sys.function(sys.parent())
  order <- match.arg(order)
  engine_query(src, method, fn, function(deadline) {
    every <- isTRUE(spec$all_analyses) && is.null(analyses)  # no Analyses named: look in all of them
    check_query_arguments(analyses, observed_only, original_units, method, optional = every)
    kinds <- if (!is.null(variants)) classify_variants(variants, call = method)
    releases <- resolve_releases(src, releases, method)
    limits <- job_limits()
    scope <- query_scope(src, if (!every) analyses, releases, analyses_per_job %||% limits$analyses,
                         original_units, limits, method)
    releases <- scope$releases
    info <- scope$info
    # Clumping chooses its LD Stores now, before any Job runs, so a refusal costs none.
    clump <- if (!is.null(spec$clump)) clump_contribution(src, spec$clump, info, method)

    # Preparation: rsIDs name ALIDs only in these Store Releases. The lookup's Jobs are in provenance;
    # their rows are variants, not Associations, so they count none.
    jobs <- list()
    if (!is.null(kinds)) {
      resolved <- resolve_variants(src, kinds, releases, deadline, method)
      jobs <- lapply(resolved$jobs, utils::modifyList, list(total = 0, truncated = FALSE))
    }
    # One Job takes a limited number of Analyses from a limited number of GWAS Stores, and of variants
    # and positions: a bigger request is split by those and put back together below. Each field of the
    # request that can be split is one contribution: its split dimension, and what its piece adds to the
    # Job's body. A new kind of query adds a contribution (and a spec field), never a branch below.
    contributions <- c(
      list(scope = scope),
      if (!is.null(kinds)) list(variants = variants_contribution(resolved, isTRUE(spec$one_variant), limits)),
      if (!is.null(spec$interval)) list(interval = interval_contribution(spec$interval)),
      if (!is.null(clump)) list(clump = clump)
    )
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
      for (i in seq_along(results)) {
        n <- nrow(results[[i]]$rows)
        if (is.null(truncated)) {
          results[[i]]$job <- utils::modifyList(results[[i]]$job, list(total = n, truncated = FALSE))
        } else {
          check_job_metadata(results[[i]]$job, n)
        }
      }
      all_jobs <- c(jobs, lapply(results, `[[`, "job"))
      rows <- result_rows(results)
      # With no Analyses named, the rows say which there are: one lookup, of those only.
      if (is.null(info)) info <- result_analyses(src, rows, releases, method)
      # Warnings name the user's call while it runs (not when og_job_result() collects it later).
      if (any(vapply(all_jobs, function(j) j$truncated, NA))) {
        og_warn(truncated(info$id), class = "opengwasr_truncated", n_analyses = nrow(info), call = method)
      }
      if (!is.null(kinds) && length(resolved$unresolved) > 0) warn_unresolved(resolved$unresolved, method)
      shaped <- shape_associations(rows, info, original_units, method)
      shaped <- switch(order, api = order_by_analysis(shaped, info$id), jobs = shaped,
                       order_associations(shaped, info$id))
      # A contribution may finish what it started: columns, provenance fields, warnings.
      provenance <- list()
      for (contribution in contributions) {
        if (is.null(contribution$finish)) next
        done <- contribution$finish(shaped, results, pieces, method)
        shaped <- done$value
        if (isTRUE(done$truncated) && length(all_jobs) > 0) all_jobs[[length(all_jobs)]]$truncated <- TRUE
        provenance <- c(provenance, done$provenance)
      }
      list(value = shaped, jobs = all_jobs, provenance = provenance)
    })
  })
}

# What a request is made of, as contributions: each is `list(dim, body)`, `dim` the split dimension
# and `body(chunk)` the fields one piece adds to a Job's body. A contribution with `along = "<name>"`
# instead of a `dim` follows that contribution's dimension (its `body` gets that chunk), and one with
# `finish(rows, results, pieces, call)` returns `list(value, provenance)` from the shaped rows, and
# one with `group(chunk)` has its Jobs submitted grouped by that value (clumping, by LD Store:
# `clump_contribution()`, R/clump.R). The Analyses are the scope: named ones
# (grouped by GWAS Store, narrowing the Store Releases) or, with `analyses = NULL`, every Analysis of
# the Store Releases (split by Store Release; the metadata comes with the result, `info` is NULL).
query_scope <- function(src, analyses, releases, per_job, original_units, limits, call) {
  if (is.null(analyses)) {
    return(list(dim = split_dim(length(releases), limits$releases), info = NULL, releases = releases,
                body = function(chunk) list(releases = releases[chunk])))
  }
  info <- analyses_lookup(src, analyses, releases, call)
  if (original_units) check_original_units(info, call)  # refuse before a Job runs, not after
  releases <- releases_of_analyses(releases, info)
  list(dim = split_dim(nrow(info), per_job, group = info$store_id, group_limit = limits$releases),
       info = info, releases = releases,
       body = function(chunk) list(analyses = as.list(info$id[chunk]),
                                   releases = piece_releases(releases, info$store_id, chunk)))
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
  first <- c("variants", "variant", "analyses")
  last <- c("observed_only", "releases")
  c(fields[intersect(first, names(fields))], body,
    fields[setdiff(names(fields), c(first, last))],
    list(observed_only = observed_only), fields["releases"])
}

# The metadata of the Analyses a result's rows are in, in order of first appearance (with every
# Analysis searched there is no list asked for). Kept: the API's rows carry no sample size or effect
# scale, so shaping needs the Analyses' own metadata, and only these may be looked up, never the catalogue.
# `original_units` is refused for them when the rows are shaped (shape_associations()).
result_analyses <- function(src, rows, releases, call) {
  analyses_lookup(src, unique(canonical_ids(rows)), releases, call)
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
