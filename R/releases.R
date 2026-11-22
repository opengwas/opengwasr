# Store Release selection: the one place that turns a user's `releases` into the API's.
#
# Every query function that takes `releases` calls `resolve_releases()`. A GWAS Store id such as
# "OGS-00011" means the one Store Release the API serves for that GWAS Store (read from
# /api/batches). Omitting `releases` means that rule for every GWAS Store the API serves, sent
# explicitly: the API's own default is "every configured Store Release", which isn't a choice.
# The API gives no recency field, so when a GWAS Store serves several Store Releases opengwasr
# refuses to guess which is latest, and lists them; "<store_id>/<release_id>" pins one.

#' @param releases `NULL` (the sole served Store Release of every GWAS Store), or a character
#'   vector of GWAS Store ids and `<store_id>/<release_id>` pins.
#' @return The list of `list(store_id, release_id)` the API takes in `releases`.
#' @noRd
resolve_releases <- function(src, releases, call = rlang::caller_env()) {
  if (is.null(releases)) return(default_releases(src, call))
  if (!is.character(releases) || length(releases) == 0 || anyNA(releases) || any(!nzchar(releases))) {
    og_abort(
      c("`releases` must be a character vector of GWAS Store ids or `<store_id>/<release_id>`.",
        i = 'For example `"OGS-00011"` or `"OGS-00011/OGS-00011-completed"`.'),
      class = "opengwasr_argument", call = call
    )
  }
  releases <- unique(releases)
  pinned <- grepl("/", releases, fixed = TRUE)
  served <- NULL
  out <- lapply(releases, function(item) {
    if (grepl("/", item, fixed = TRUE)) {
      parts <- strsplit(item, "/", fixed = TRUE)[[1]]
      return(list(store_id = parts[[1]], release_id = paste(parts[-1], collapse = "/")))
    }
    if (is.null(served)) served <<- api_perform(src, "api/batches", call = call)$batches
    match <- Filter(function(b) identical(b$store_id, item), served)
    if (length(match) == 0) {
      og_abort(
        c(paste0("The API serves no Store Release of `", item, "`."),
          i = "Check the GWAS Store id, such as \"OGS-00011\"."),
        class = "opengwasr_unknown_release", store_id = item, call = call
      )
    }
    if (length(match) > 1) {
      candidates <- vapply(match, function(b) paste0(b$store_id, "/", b$release_id), "")
      og_abort(
        c(paste0("`", item, "` has more than one Store Release, and the API doesn't say which is latest."),
          i = paste0("Pin one: ", paste0("\"", candidates, "\"", collapse = ", "), ".")),
        class = "opengwasr_release_selection_required", store_id = item, releases = candidates,
        call = call
      )
    }
    list(store_id = item, release_id = match[[1]]$release_id)
  })
  out
}

# The sole served Store Release of every GWAS Store; refuses if any serves several.
default_releases <- function(src, call) {
  served <- api_perform(src, "api/batches", call = call)$batches
  store_ids <- vapply(served, function(b) b$store_id, "")
  many <- unique(store_ids[duplicated(store_ids)])
  if (length(many) > 0) {
    candidates <- vapply(Filter(function(b) b$store_id %in% many, served),
                         function(b) paste0(b$store_id, "/", b$release_id), "")
    og_abort(
      c(paste0("Without `releases`, opengwasr uses each GWAS Store's one Store Release, but ",
               paste0("`", many, "`", collapse = ", "), " serve",
               if (length(many) == 1) "s" else "", " several, and the API doesn't say which is latest."),
        i = paste0("Pin them in `releases`: ", paste0("\"", candidates, "\"", collapse = ", "), ".")),
      class = "opengwasr_release_selection_required", store_id = many, releases = candidates, call = call
    )
  }
  lapply(served, function(b) list(store_id = b$store_id, release_id = b$release_id))
}

# The Store Releases of the Analyses asked for: a Job reads only those. A query over more than
# `max_releases_per_job` of them is split by GWAS Store (R/split.R), so there is no limit here.
# `info` is Analysis metadata with `store_id`.
releases_of_analyses <- function(releases, info) {
  Filter(function(r) r$store_id %in% unique(info$store_id), releases)
}
