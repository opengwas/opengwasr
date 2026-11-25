# Store Release selection: the one place that turns a user's `releases` into the API's.
#
# Every query function that takes `releases` calls `resolve_releases()`. A GWAS Store id such as
# "OGS-00011" means the one Store Release the API serves for that GWAS Store (read from
# /api/batches). Omitting `releases` means that rule for every GWAS Store the API serves, sent
# explicitly: the API's own default is "every configured Store Release", which isn't a choice.
# The API gives no recency field, so when a GWAS Store serves several Store Releases opengwasr
# refuses to guess which is latest, and lists them; "<store_id>/<release_id>" pins one. A Local
# Source holds one Store Release of each GWAS Store (ADR 0002, clarification of 25 November 2026):
# its candidates are those (source_releases()), and a pin it doesn't hold is refused where it is
# read, as the API refuses one (local_releases_held()).

#' @param releases `NULL` (the sole served Store Release of every GWAS Store), or a character
#'   vector of GWAS Store ids and `<store_id>/<release_id>` pins.
#' @return The list of `list(store_id, release_id)` the API takes in `releases`.
#' @noRd
resolve_releases <- function(src, releases, call = rlang::caller_env()) {
  # Kept: an R argument's type.
  if (!is.null(releases) &&
      (!is.character(releases) || length(releases) == 0 || anyNA(releases) || any(!nzchar(releases)))) {
    og_abort(
      c("`releases` must be a character vector of GWAS Store ids or `<store_id>/<release_id>`.",
        i = 'For example `"OGS-00011"` or `"OGS-00011/OGS-00011-completed"`.'),
      class = "opengwasr_argument", call = call
    )
  }
  served <- NULL
  serves <- function() served %||% (served <<- source_releases(src, call))
  # Omitted: every GWAS Store the API serves, sent explicitly (the API's own default is every
  # configured Store Release, which isn't a choice).
  releases <- unique(releases %||% vapply(serves(), function(b) b$store_id, ""))
  several <- character()
  out <- lapply(releases, function(item) {
    if (grepl("/", item, fixed = TRUE)) {
      parts <- strsplit(item, "/", fixed = TRUE)[[1]]
      return(list(store_id = parts[[1]], release_id = paste(parts[-1], collapse = "/")))
    }
    match <- Filter(function(b) identical(b$store_id, item), serves())
    # Kept: the API takes only `<store_id>/<release_id>`, so a bare GWAS Store id is resolved here,
    # from the API's list of what it serves (class of the API's code for an unknown release).
    if (length(match) == 0) {
      og_abort(paste0(if (inherits(src, "og_local_source")) "This Local Source holds" else "The API serves",
                      " no Store Release of `", item, "`."),
               class = "opengwasr_release_not_found", api_error = "release_not_found", store_id = item,
               call = call)
    }
    if (length(match) > 1) several <<- c(several, item)
    list(store_id = item, release_id = match[[1]]$release_id)
  })
  # Kept: Store Release ambiguity when no release is pinned; the API has no recency field.
  if (length(several) > 0) {
    candidates <- vapply(Filter(function(b) b$store_id %in% several, serves()),
                         function(b) release_names(b$store_id, b$release_id), "")
    og_abort(
      c(paste0(some_ids(several, length(several), quote = "`"), if (length(several) == 1) " serves" else " serve",
               " more than one Store Release, and the API doesn't say which is latest."),
        i = paste0("Pin one in `releases`: ", some_ids(candidates, length(candidates), quote = "\""), ".")),
      class = "opengwasr_release_selection_required", api_error = "release_selection_required",
      store_id = several, releases = candidates, call = call
    )
  }
  out
}

# Store Releases as `<store_id>/<release_id>`, as pins, provenance and messages name them: the one
# place that string is made. Give the ids, or (`release_id` missing) a list of Store Releases as the
# API takes them, each `list(store_id, release_id, ...)`.
release_names <- function(store_id, release_id) {
  if (missing(release_id)) {
    releases <- store_id
    store_id <- vapply(releases, function(r) as.character(r$store_id), "")
    release_id <- vapply(releases, function(r) as.character(r$release_id), "")
  }
  if (length(store_id) == 0) return(character())
  paste0(store_id, "/", release_id)
}

# The Store Releases a Source answers from, each `list(store_id, release_id, ...)`: the API's, those
# /api/batches says it serves; a Local Source's, those it holds (one of each GWAS Store, so the
# "several" refusal above never applies to it).
source_releases <- function(src, call) {
  if (inherits(src, "og_local_source")) {
    held <- src$releases
    return(lapply(seq_len(nrow(held)), function(i) list(store_id = held$store_id[[i]], release_id = held$release_id[[i]])))
  }
  api_perform(src, "api/batches", call = call)$batches
}

# The Store Releases of the Analyses asked for (`info`, Analysis metadata with `store_id`): a query,
# and each of its Jobs, reads only those. A query over more than `max_releases_per_job` of them is
# split by GWAS Store (R/split.R), so there is no limit here.
releases_of_analyses <- function(releases, info) {
  Filter(function(r) r$store_id %in% unique(info$store_id), releases)
}
