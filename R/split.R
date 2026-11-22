# The splitter: turns a request over one Job's shape limits into several requests of one plan.
#
# Pure functions from the dimensions of a request and the limits of a Job (R/jobs.R) to the pieces,
# as indices, in a stable order. association_query() (or a query of another kind, in preparation)
# builds one request per piece and puts the rows back together in its plan's `finish()`;
# run_query() runs the pieces with at most `src$max_in_flight` Jobs in flight. The splitter never
# looks at a cost refusal: a 413 is never split round (ADR 0002).
#
#   split_dim(n, limit, group = NULL, group_limit = NULL)   one dimension of a request of `n` items
#   split_pieces(dims)        Cartesian product of the dimensions' chunks: a list of pieces, each a
#                             named list of integer indices into each dimension (first dimension
#                             slowest). #9's associations and top hits, #11's og_range() Analyses
#   split_pairs(n, limit)     every pair of n items, in pieces of at most `limit` items: #12's LD matrix
#   split_interval(start, end, limit)   an inclusive one-based interval as a dimension (chunks are
#                             c(start, end)) of at most `limit` positions: #11's og_range() (10 Mb)
#
# A dimension with a `group` (an Analysis's GWAS Store) is packed group by group: a piece takes
# whole groups while it holds at most `group_limit` groups and `limit` items, and a group larger
# than `limit` is cut into several pieces.

# What one Job takes (R/jobs.R), unless the option `opengwasr.job_limits` lowers them: a named list
# such as `list(analyses = 2, variants = 3)`, for tests that would otherwise need 1,001 variants.
job_limits <- function() {
  limits <- list(releases = max_releases_per_job, variants = max_variants_per_job,
                 analyses = max_analyses_per_job, rsids = max_rsids_per_job)
  utils::modifyList(limits, as.list(getOption("opengwasr.job_limits", list())))
}

split_dim <- function(n, limit, group = NULL, group_limit = NULL) {
  ok <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 1
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 0 || !ok(limit) ||
      (!is.null(group) && (!ok(group_limit) || length(group) != n))) {
    og_abort("A dimension to split needs a count, a limit of at least 1 (and a group as long as its items).",
             class = "opengwasr_argument")
  }
  structure(list(n = n, limit = limit, group = group, group_limit = group_limit), class = "og_dim")
}

# The chunks of one dimension: a list of integer index vectors, in order, each within the limits.
dim_chunks <- function(d) {
  if (!is.null(d$start)) {
    starts <- seq(d$start, d$start + d$n - 1, by = d$limit)
    return(unname(Map(function(s) c(s, min(s + d$limit - 1, d$start + d$n - 1)), starts)))
  }
  if (d$n == 0) return(list(integer()))
  if (is.null(d$group)) return(unname(split(seq_len(d$n), (seq_len(d$n) - 1) %/% d$limit)))
  # Items grouped in order of first appearance of their group.
  labels <- unique(d$group)
  by_group <- lapply(labels, function(g) which(d$group == g))
  chunks <- list()
  current <- integer()
  groups_in_current <- 0
  flush <- function() {
    if (length(current) > 0) chunks[[length(chunks) + 1]] <<- current
    current <<- integer()
    groups_in_current <<- 0
  }
  for (idx in by_group) {
    if (length(idx) > d$limit) {
      # A group too big for one piece fills pieces of its own.
      flush()
      for (part in split(idx, (seq_along(idx) - 1) %/% d$limit)) chunks[[length(chunks) + 1]] <- unname(part)
      next
    }
    if (groups_in_current + 1 > d$group_limit || length(current) + length(idx) > d$limit) flush()
    current <- c(current, idx)
    groups_in_current <- groups_in_current + 1
  }
  flush()
  chunks
}

split_pieces <- function(dims) {
  chunks <- lapply(dims, dim_chunks)
  grid <- do.call(expand.grid, c(lapply(chunks, seq_along), list(KEEP.OUT.ATTRS = FALSE)))
  # expand.grid varies the first column fastest; the first dimension is to vary slowest.
  grid <- grid[do.call(order, unname(as.list(grid))), , drop = FALSE]
  lapply(seq_len(nrow(grid)), function(i) {
    stats::setNames(lapply(seq_along(chunks), function(k) chunks[[k]][[grid[i, k]]]), names(dims))
  })
}

# Every pair of the n items is in some piece: a piece holds the items of its two sets `a` and `b`
# (at most `limit` together), and a caller computes every pair within that union. One chunk is one
# piece (`a == b`); with two or more, the pieces are the chunk pairs i < j, which already hold every
# pair inside a chunk.
split_pairs <- function(n, limit) {
  if (!is.numeric(limit) || length(limit) != 1 || is.na(limit) || limit < 2) {
    og_abort("Pairs need a limit of at least 2 items per piece.", class = "opengwasr_argument")
  }
  if (n <= limit) return(list(list(a = seq_len(n), b = seq_len(n))))
  chunks <- unname(split(seq_len(n), (seq_len(n) - 1) %/% (limit %/% 2)))
  pieces <- list()
  for (i in seq_len(length(chunks) - 1)) for (j in (i + 1):length(chunks)) {
    pieces[[length(pieces) + 1]] <- list(a = chunks[[i]], b = chunks[[j]])
  }
  pieces
}

# An inclusive one-based interval as a dimension, cut into consecutive intervals of at most `limit`
# positions: each chunk is `c(start, end)`, so split_pieces() crosses it with other dimensions without
# listing the positions.
split_interval <- function(start, end, limit) {
  if (!is.numeric(start) || !is.numeric(end) || length(start) != 1 || length(end) != 1 ||
      is.na(start) || is.na(end) || start > end) {
    og_abort("An interval needs a `start` that is not after its `end`.", class = "opengwasr_argument")
  }
  d <- split_dim(end - start + 1, limit)
  d$start <- start
  d
}

# The Store Releases a piece reads: those of the GWAS Stores of its Analyses (`analyses`, indices into
# `store_ids`, the GWAS Store of each Analysis), from the query's `releases`.
piece_releases <- function(releases, store_ids, analyses) {
  stores <- unique(store_ids[analyses])
  Filter(function(r) r$store_id %in% stores, releases)
}

# The Analyses one top-hits Job may take: at most `limit` hits per Analysis, and a Job's result holds
# at most `max_rows_per_job` rows, so `limit` x Analyses must fit.
tophits_analyses_per_job <- function(limit, limits = job_limits()) {
  max(1, min(limits$analyses, max_rows_per_job %/% limit))
}
