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
    # A Local Source runs no Job: "local" stands where an API Job's id would be.
    job_id = "local",
    existing = FALSE,
    cost_estimate = NA_real_,
    total = answer$total %||% nrow(rows),
    truncated = answer$truncated %||% FALSE,
    metadata = answer$metadata %||% list(),
    releases = vapply(releases, function(r) paste0(r$store_id, "/", r$release_id), "")
  ))
}

# Store Releases as the API takes them (`list(store_id, release_id)` each) that this Source holds;
# one it doesn't is refused as the API refuses a Store Release it doesn't serve. A GWAS Store id
# was resolved to the Release held by resolve_releases(); a pin is checked here, as the API checks
# it, so every local read (each request, and the Analyses) checks what it is asked to read.
local_releases_held <- function(src, releases, call = rlang::caller_env()) {
  held <- paste0(src$releases$store_id, "/", src$releases$release_id)
  asked <- vapply(releases, function(r) paste0(r$store_id, "/", r$release_id), "")
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
  # Release, as the API (`variants_by_position`): a value that isn't `chromosome:position`, or
  # whose chromosome no ALID spells, names none.
  "api/variants/chrpos" = function(src, body, releases, call) {
    wanted <- sub("^chr", "", as.character(unlist(body$positions)))
    at <- regmatches(wanted, regexec("^(.+):([0-9]+)$", wanted))
    at <- unique(Filter(function(m) length(m) == 3, at))
    local_variant_lookup(src, releases, call, function(facade) {
      found <- lapply(at, function(m) {
        rows <- tryCatch(local_read_variants_in_range(facade, m[[2]], as.numeric(m[[3]])),
                         ogquery.identity.UnsupportedChromosome = function(e) lookup_fields())
        rows[paste0(rows$chromosome, ":", rows$position) %in% wanted, , drop = FALSE]
      })
      # Every variant once, in variant-index order, as the API answers.
      rows <- do.call(rbind, c(list(lookup_fields()), found))
      rows <- rows[!duplicated(rows$variant_index), , drop = FALSE]
      rows[order(rows$variant_index), , drop = FALSE]
    })
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
