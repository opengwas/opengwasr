# The local Job: a request the engine would send the API as a Job, answered at once from the Store
# Releases a Local Source holds (notes of 25 November 2026 on #31-#33).
#
# Every query reaches its Jobs through run_window(), which, for a Local Source, hands each request,
# the API endpoint's `path` and the Job's `body` exactly as the API would get them, to
# local_request(). That answers `list(rows, job)` in the shape collect_job() returns for an API
# Job, so splitting, combining, shaping, ordering, truncation and provenance run unchanged. Its
# rows have the API's field names and types, in the API's field order. Each endpoint is one entry
# of `local_endpoints`: a function of the Source, the body, the body's Store Releases and the
# call, returning `list(rows)`, and, for an endpoint that cuts rows off at a limit, `total` and
# `truncated` (and anything else the API reports in a Job's `metadata`, in `metadata`).

local_request <- function(src, path, body, call = rlang::caller_env()) {
  endpoint <- local_endpoints[[path]]
  if (is.null(endpoint)) {
    og_abort(c(paste0("A Local Source can't answer `", path, "`."), i = "This is a bug in opengwasr."),
             class = "opengwasr_internal", path = path, call = call)
  }
  releases <- local_releases_held(src, body$releases, call)
  answer <- endpoint(src, body, releases, call)
  rows <- answer$rows
  rownames(rows) <- NULL
  list(rows = rows, job = list(
    # A Local Source runs no Job, so has no Job id: provenance records none (attach_provenance()).
    job_id = NA_character_,
    existing = FALSE,
    cost_estimate = NA_real_,
    total = answer$total %||% nrow(rows),
    truncated = answer$truncated %||% FALSE,
    metadata = answer$metadata %||% list(),
    releases = release_names(vapply(releases, `[[`, "", "store_id"), vapply(releases, `[[`, "", "release_id"))
  ))
}

# Store Releases as the API takes them (`list(store_id, release_id)` each) that this Source holds;
# one it doesn't is refused as the API refuses a Store Release it doesn't serve. A GWAS Store id
# was resolved to the Release held by resolve_releases(); a pin is checked here, as the API checks
# it, so every local read (each request, and the Analyses) checks what it is asked to read.
local_releases_held <- function(src, releases, call = rlang::caller_env()) {
  held <- release_names(src$releases$store_id, src$releases$release_id)
  asked <- release_names(vapply(releases, `[[`, "", "store_id"), vapply(releases, `[[`, "", "release_id"))
  unknown <- unique(asked[!asked %in% held])
  if (length(unknown) > 0) {
    og_abort(c(paste0("This Local Source holds no Store Release ",
                      some_ids(unknown, length(unknown), quote = "`"), "."),
               i = paste0("It holds ", some_ids(held, length(held), quote = "`"), ".")),
             class = "opengwasr_release_not_found", api_error = "release_not_found", releases = unknown,
             call = call)
  }
  releases
}

# The endpoints a Local Source answers, by the API's path.
local_endpoints <- list(
  # rsIDs to the variants they name in each Store Release: one row per variant, so a multiallelic
  # rsID gives several, and an rsID the release lacks none. As the API (ogdb-api
  # query_operations.py `variants_by_rsid`), an rsID is matched whole, never as an ALID.
  "api/variants/rsid" = function(src, body, releases, call) {
    wanted <- as.character(unlist(body$rsids))
    local_variant_lookup(src, releases, call, function(facade) {
      rows <- local_read_variants(facade, wanted)
      rows[rows$rsid %in% wanted, , drop = FALSE]
    })
  },
  # `chromosome:position` values (a `chr` prefix allowed) to the variants there, in each Store
  # Release, as the API (`variants_by_position`): a value that isn't `chromosome:position` names
  # none, and a chromosome no ALID spells is refused (local_read_variants_at()), as the API fails
  # the Job.
  "api/variants/chrpos" = function(src, body, releases, call) {
    wanted <- sub("^chr", "", as.character(unlist(body$positions)))
    at <- unique(Filter(function(m) length(m) == 3, regmatches(wanted, regexec("^(.+):([0-9]+)$", wanted))))
    # A position past any chromosome's end holds nothing (and isn't an R integer).
    at <- Filter(function(m) as.numeric(m[[3]]) <= .Machine$integer.max, at)
    chromosome <- vapply(at, `[[`, "", 2)
    position <- as.integer(vapply(at, `[[`, "", 3))
    local_variant_lookup(src, releases, call, function(facade) {
      rows <- local_read_variants_at(facade, chromosome, position, call)
      rows[paste0(rows$chromosome, ":", rows$position) %in% wanted, , drop = FALSE]
    })
  },
  # Associations of variants (ALIDs: the engine resolved rsIDs before) in Analyses, as the API
  # (query_operations.py `execute_operation`): ogquery's `lookup()` in each Store Release, of the
  # Analyses it holds. The engine orders them (order_associations()).
  "api/associations" = function(src, body, releases, call) {
    local_require_associations(src, releases, call)
    ogquery <- local_ogquery(call)
    variants <- as.list(as.character(unlist(body$variants)))
    rows <- local_release_rows(src, releases, call, function(facade, release) {
      here <- analyses_held_here(facade, body$analyses, release$store_id)
      if (NROW(here) == 0) return(NULL)
      record <- local_read_associations(facade, "lookup", list(variants, as.list(here$analysis_id),
                                                               observed_only = body$observed_only), call)
      local_resolve_rows(ogquery, facade, record)
    })
    list(rows = local_distinct_rows(rows, call))
  },
  # Every Association of a window of one chromosome, in the Analyses named, as the API
  # (`execute_operation`): ogquery's `range_phewas()`, cut to those Analyses before any row is
  # resolved (`keep_analyses`). A chromosome other than 1-22, X, Y or MT is refused, as the API's
  # body model refuses it (schemas.py `check_chromosome`). The engine orders the rows.
  "api/associations/range" = function(src, body, releases, call) {
    ogquery <- local_ogquery(call)
    if (!local_primary_chromosome(ogquery, body$chromosome)) {
      local_malformed(paste0("chromosome '", body$chromosome, "' is not one of 1-22, X, Y or MT"), call)
    }
    local_require_associations(src, releases, call)
    rows <- local_release_rows(src, releases, call, function(facade, release) {
      here <- analyses_held_here(facade, body$analyses, release$store_id)
      if (!is.null(here) && nrow(here) == 0) return(NULL)
      record <- local_read_associations(facade, "range_phewas", list(
        body$chromosome, as.integer(body$start), as.integer(body$end), observed_only = body$observed_only), call)
      index <- local_record_index(ogquery, record)
      keep <- seq_along(index$z) - 1L
      if (!is.null(here)) keep <- keep[index$analysis_index %in% here$analysis_index]
      local_resolve_rows(ogquery, facade, record, keep)
    })
    list(rows = local_distinct_rows(rows, call))
  },
  # Each Analysis's top hits at p <= `threshold`, at most `limit` of them, as the API
  # (`_top_hits`): ogquery's `top_hits()` at the stored tier at or above `threshold`
  # (top_hits.py `index_tier`), cut to `threshold` (`at_threshold`), and each Analysis's `limit`
  # strongest kept, strongest first (`rank_per_analysis`), which the engine keeps
  # (order_by_analysis()). `truncated` says whether any Analysis had more than `limit`.
  "api/tophits" = function(src, body, releases, call) {
    threshold <- body$threshold
    if (!(threshold > 0 && threshold < 1)) local_malformed("threshold must be greater than 0 and less than 1", call)
    ogquery <- local_ogquery(call)
    # Every Store Release's tier is chosen before any is read, as the API chooses them when the Job is
    # submitted.
    tiers <- lapply(releases, function(r) local_top_hit_tier(ogquery, src, r$store_id, threshold, call))
    names(tiers) <- vapply(releases, `[[`, "", "store_id")
    cut <- local_z_critical(ogquery, threshold)
    capped <- FALSE
    rows <- local_release_rows(src, releases, call, function(facade, release) {
      tier <- tiers[[release$store_id]]
      here <- analyses_held_here(facade, body$analyses, release$store_id)
      found <- lapply(if (is.null(here)) list(NULL) else as.list(here$analysis_id), function(analysis_id) {
        record <- local_read_associations(facade, "top_hits", list(analysis_id = analysis_id, threshold = tier,
                                                                   observed_only = body$observed_only), call)
        index <- local_record_index(ogquery, record)
        keep <- seq_along(index$z) - 1L
        if (threshold != tier) keep <- keep[hit_strength(index$z) >= cut]
        ranked <- rank_per_analysis(index, keep, body$limit)
        capped <<- capped || ranked$cut
        local_resolve_rows(ogquery, facade, record, ranked$keep)
      })
      found <- Filter(Negate(is.null), found)
      if (length(found) > 0) bind_frames(found)
    })
    list(rows = rows, total = nrow(rows), truncated = capped,
         metadata = list(threshold = threshold, observed_only = body$observed_only, total = nrow(rows),
                         truncated = capped))
  },
  # One variant across the Analyses of the Store Releases (or those named) at p <= `threshold`, as
  # the API (`_phewas`): ogquery's `phewas()`, cut to `threshold` (top_hits.py `significant`) and
  # the Analyses before any row is resolved; each Store Release's first `limit`, then all of them
  # in the API's order (local_api_order()), cut to `limit`. `total` counts every Association that
  # passed. The engine keeps the Jobs' order, so this is the one endpoint whose order is its own.
  "api/phewas" = function(src, body, releases, call) {
    threshold <- body$threshold
    limit <- body$limit
    if (!(threshold > 0 && threshold <= 1)) local_malformed("threshold must be greater than 0 and at most 1", call)
    if (!(limit >= 1 && limit <= max_phewas_per_variant)) {
      local_malformed(paste0("limit must be from 1 to ", max_phewas_per_variant), call)
    }
    local_require_associations(src, releases, call, phewas = TRUE)
    ogquery <- local_ogquery(call)
    cut <- if (threshold < 1) local_z_critical(ogquery, threshold)
    total <- 0
    rows <- local_release_rows(src, releases, call, function(facade, release) {
      record <- local_read_associations(facade, "phewas", list(body$variant, observed_only = body$observed_only), call)
      index <- local_record_index(ogquery, record)
      keep <- seq_along(index$z) - 1L
      if (!is.null(cut)) keep <- keep[hit_strength(index$z) >= cut]
      here <- analyses_held_here(facade, body$analyses, release$store_id)
      if (!is.null(here)) keep <- keep[index$analysis_index[keep + 1L] %in% here$analysis_index]
      total <<- total + length(keep)
      local_resolve_rows(ogquery, facade, record, utils::head(keep, limit))
    })
    rows <- utils::head(local_api_order(rows), limit)
    rownames(rows) <- NULL
    list(rows = rows, total = total, truncated = total > nrow(rows),
         metadata = list(variant = body$variant, threshold = threshold, observed_only = body$observed_only,
                         total = total, returned = nrow(rows), truncated = total > nrow(rows)))
  }
)

# A variant lookup over each Store Release in turn: `read(facade)` gives one release's rows, in
# variant-index order, and each row is stamped with its Store Release, as the API stamps it. The
# fields are the API's, in the order it sends them.
local_variant_lookup <- function(src, releases, call, read) {
  found <- lapply(releases, function(r) {
    rows <- read(local_facade(src, r$store_id, call))
    rows$store_id <- rep(r$store_id, nrow(rows))
    rows$release_id <- rep(r$release_id, nrow(rows))
    rows
  })
  rows <- do.call(rbind, c(list(cbind(lookup_fields(), store_id = character(), release_id = character())), found))
  list(rows = rows[c("alid", "chromosome", "effect_allele", "other_allele", "position", "release_id",
                     "rsid", "store_id")])
}

# The fields of ogquery's variant rows that a lookup reads, with none.
lookup_fields <- function() {
  data.frame(variant_index = integer(), alid = character(), chromosome = character(), position = integer(),
             effect_allele = character(), other_allele = character(), rsid = character(),
             stringsAsFactors = FALSE)
}

# Rows read from each Store Release in turn, stamped with it, as the API stamps them
# (query_operations.py `_provenance`): `read(facade, release)` gives one release's rows
# (local_resolve_rows()), or NULL for none. The fields are in name order, as the API sends them
# (`json.dumps(sort_keys=True)`).
local_release_rows <- function(src, releases, call, read) {
  found <- lapply(releases, function(r) {
    rows <- read(local_facade(src, r$store_id, call), r)
    if (is.null(rows) || nrow(rows) == 0) return(NULL)
    rows$store_id <- rep(r$store_id, nrow(rows))
    rows$release_id <- rep(r$release_id, nrow(rows))
    rows
  })
  found <- Filter(Negate(is.null), found)
  rows <- if (length(found) > 0) bind_frames(found)
          else cbind(api_association_rows(list()), store_id = character(), release_id = character())
  rows[sort(names(rows), method = "radix")]
}

# The Analyses of `requested` that a Store Release of GWAS Store `store_id` holds, `analysis_index`
# and `analysis_id`, in the order requested, each once, as the API finds them (releases.py
# `local_analysis_ids`): an Analysis named `<store_id>:<analysis_id>` counts only for its own GWAS
# Store, a bare `analysis_id` for any that holds it. NULL when none are requested (every Analysis).
analyses_held_here <- function(facade, requested, store_id) {
  if (is.null(requested)) return(NULL)
  requested <- as.character(unlist(requested))
  held <- local_read_analysis_ids(facade)
  prefix <- paste0(store_id, ":")
  bare <- ifelse(startsWith(requested, prefix) & substring(requested, nchar(prefix) + 1) %in% held$analysis_id,
                 substring(requested, nchar(prefix) + 1), requested)
  bare <- unique(bare[bare %in% held$analysis_id])
  held[match(bare, held$analysis_id), , drop = FALSE]
}

# Rows `keep` of a record (0-based; NULL for all) as the API sends them (api_association_rows()),
# none when `keep` is empty.
local_resolve_rows <- function(ogquery, facade, record, keep = NULL) {
  if (!is.null(keep) && length(keep) == 0) return(NULL)
  api_association_rows(local_resolve(ogquery, facade, record, keep))
}

# ogquery's resolved rows as the API sends them: the fields its reader gives (ogdb-api pins
# opengwasdb, whose `resolve_rows` gives `log10_p` and no `beta`, `pval` or `source_alid`), each a
# column of the type the API's JSON parses to. A field holding both numbers and "." (`eaf`, `n`)
# is text, as the API Source parses it, with each number written exactly; shape_associations()
# reads "." as NA for both Sources. A missing Association's z, se and log10_p are NaN, the API's
# intended answer (its NDJSON can't carry NaN yet: opengwas/ogdb-api#193).
api_association_rows <- function(fields) {
  n <- length(fields$alid)
  column <- function(name, empty = character(n)) {
    x <- fields[[name]]
    if (is.null(x)) return(empty)
    if (!is.list(x)) return(x)
    vapply(x, function(v) if (is.numeric(v)) sprintf("%.17g", v) else as.character(v), "", USE.NAMES = FALSE)
  }
  rows <- data.frame(alid = column("alid"), analysis_id = column("analysis_id"),
                     analysis_label = column("analysis_label"), association_status = column("association_status"),
                     chromosome = column("chromosome"), eaf = column("eaf", numeric(n)),
                     effect_allele = column("effect_allele"), log10_p = as.numeric(column("log10_p", numeric(n))),
                     other_allele = column("other_allele"), position = as.integer(column("position", integer(n))),
                     rsid = column("rsid"), se = as.numeric(column("se", numeric(n))),
                     z = as.numeric(column("z", numeric(n))), stringsAsFactors = FALSE)
  # A Ragged release with per-Association sample sizes sends `n` (ogquery ADR 0067); others don't.
  if (!is.null(fields$n)) rows$n <- column("n", numeric(n))
  rows
}

# The rows of an endpoint the API's planner answers (`FederatedPlanner.run`, planner.py:255-266),
# each Association once: rows with the same key that agree are one, and rows that disagree are
# refused, as the API refuses them (`ConflictingRowsError`, code `conflicting_rows`).
local_distinct_rows <- function(rows, call) {
  key <- c("store_id", "release_id", "analysis_id", "alid")
  repeated <- duplicated(rows[key])
  if (!any(repeated)) return(rows)
  distinct <- rows[!duplicated(rows), , drop = FALSE]
  clash <- duplicated(distinct[key])
  if (any(clash)) {
    at <- distinct[which(clash)[[1]], key]
    og_abort(paste0("conflicting association rows for (", paste0("'", unlist(at), "'", collapse = ", "), ")"),
             class = api_error_class("conflicting_rows", 409L), api_error = "conflicting_rows",
             store_id = at$store_id, release_id = at$release_id, analysis_id = at$analysis_id, alid = at$alid,
             call = call)
  }
  rownames(distinct) <- NULL
  distinct
}

# PheWAS rows in the API's order (ordering.py `row_key`): by GWAS Store, Store Release and
# Analysis, then genome order (chromosome_rank(), position, ALID).
local_api_order <- function(rows) {
  if (nrow(rows) > 1) {
    rows <- rows[order(rows$store_id, rows$release_id, rows$analysis_id, chromosome_rank(rows$chromosome),
                       rows$position, rows$alid, method = "radix"), , drop = FALSE]
  }
  rownames(rows) <- NULL
  rows
}

# |z|, with a missing z weakest (top_hits.py `strength`).
hit_strength <- function(z) ifelse(is.na(z), -1, abs(z))

# Of the rows `keep` (0-based) of a top-hits record (`index`, local_record_index()), each
# Analysis's `limit` with the largest |z|, ties in Variant Index order, strongest first
# (top_hits.py `rank_per_analysis`); `cut` says whether any Analysis had more.
rank_per_analysis <- function(index, keep, limit) {
  if (length(keep) == 0) return(list(keep = keep, cut = FALSE))
  analysis <- index$analysis_index[keep + 1L]
  keep <- keep[order(analysis, -hit_strength(index$z[keep + 1L]), index$variant_index[keep + 1L], method = "radix")]
  analysis <- index$analysis_index[keep + 1L]
  rank <- stats::ave(seq_along(keep), analysis, FUN = seq_along)
  list(keep = keep[rank <= limit], cut = any(rank > limit))
}

# The stored top-hit tier a Store Release reads for `threshold`: the tightest at or above it
# (top_hits.py `index_tier`), or the API's refusal when none is that loose.
local_top_hit_tier <- function(ogquery, src, store_id, threshold, call) {
  tiers <- local_top_hit_tiers(ogquery, local_release(src, store_id, call))
  if (length(tiers) == 0) {
    local_unsupported(paste0("The Store Release of GWAS Store `", store_id, "` has no Top-Hit Index."),
                      "Query its Associations with `og_associations()` or `og_range()` instead.", call = call)
  }
  looser <- tiers[tiers >= threshold]
  if (length(looser) == 0) {
    og_abort(paste0("threshold must be at most ", formatC(max(tiers), format = "g"),
                    ", the loosest p-value the Top-Hit Index of ", store_id, " holds"),
             class = api_error_class("unsupported_threshold", 400L), api_error = "unsupported_threshold",
             indexed_thresholds = as.list(sort(tiers)), call = call)
  }
  min(looser)
}

# The API serves Associations only from a Store Release that holds them, and a PheWAS only from one
# that can read a variant across its Analyses without a scan (ogdb-api releases.py
# `_capabilities`, checked by query_operations.py `select_operation` before anything is read):
# not a release whose `association_coverage` is `top_hits` or `unknown`; for a PheWAS, not a
# Ragged or Hybrid release without its by-variant index. Every release is checked first, as there.
local_require_associations <- function(src, releases, call, phewas = FALSE) {
  ogquery <- local_ogquery(call)
  for (r in releases) {
    release <- local_release(src, r$store_id, call)
    if (local_association_coverage(release) %in% c("top_hits", "unknown")) {
      local_unsupported(paste0("The Store Release `", r$store_id, "/", r$release_id, "` holds only top hits."),
                        "Use `og_tophits()` on it.", call = call)
    }
    if (phewas && !local_variant_indexed(ogquery, release)) {
      local_unsupported(paste0("The Store Release `", r$store_id, "/", r$release_id,
                               "` has no by-variant index, so it can't answer a PheWAS."),
                        "Query its Associations with `og_associations()` or `og_range()` instead.", call = call)
    }
  }
}

# A request the API's body model refuses, refused the same way. The API's per-endpoint routes
# validate the body as its model (schemas.py: a range's chromosome, a threshold or limit out of
# range) before anything else, and answer 400 `invalid_request`, "Malformed request" (ogdb-api
# main.py:773-784, errors.py:56-64). Its message is the API's exactly, never reworded (ADR 0002);
# `reason` says what was wrong.
local_malformed <- function(reason, call) {
  og_abort("Malformed request", class = api_error_class("invalid_request", 400L),
           api_error = "invalid_request", reason = reason, call = call)
}
