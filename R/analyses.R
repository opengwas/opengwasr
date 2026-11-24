# Finding Analyses, and the one owner of Analysis identifiers.
#
# An Analysis is named `<store_id>:<analysis_id>` (its canonical id), or by a bare analysis id, or by
# a legacy OpenGWAS id. `resolve_analysis_ids()` turns what the user typed into canonical ids, with
# the successor warning and the ambiguous, not-found and retired errors. Later tickets that take
# Analyses resolve them through `og_analyses()`, never on their own.

#' Find Analyses
#'
#' Returns Analysis metadata, one row per identifier given, or the whole catalogue.
#'
#' An Analysis is named `<store_id>:<analysis_id>`, such as `OGS-00011:GCST90029007`. A bare
#' `analysis_id`, or an identifier from Legacy OpenGWAS, is accepted where it names exactly one
#' Analysis. The `requested_id` column shows what you asked for and `id` the canonical id.
#'
#' * A legacy identifier whose analysis has a later release is answered by the successor, with a
#'   warning of class `opengwasr_legacy_successor` carrying `requested_id` and `id`.
#' * An identifier that could name more than one Analysis raises `opengwasr_ambiguous_analysis`,
#'   with the candidates in `candidates`.
#' * An identifier that names no Analysis, or one that has been retired, raises
#'   `opengwasr_analysis_not_found` (`opengwasr_analysis_retired` too when retired), the classes of
#'   the API's own codes. The condition carries the identifiers in `ids`, the API's reason for each
#'   where it gave one in `api_reasons` (`NA` otherwise), and, for a retired Analysis, every
#'   successor the API named in `superseded_by`.
#'
#' @param src A Source, from [og_api()].
#' @param ids Analysis identifiers, or `NULL` for the whole catalogue.
#' @param releases Store Releases to use. `NULL` uses the one Store Release each GWAS Store serves,
#'   as does a GWAS Store id such as `"OGS-00011"`. The API doesn't say which Store Release of a GWAS
#'   Store is latest, so if a GWAS Store serves several, opengwasr stops with
#'   `opengwasr_release_selection_required` and lists them: pin one as `"<store_id>/<release_id>"`.
#' @return A data frame: `requested_id`, `id`, then the API's metadata columns (`store_id`,
#'   `analysis_id`, `analysis_label`, `assigned_ancestry`, `sample_size`, `release_id`, ...).
#' @examples
#' \dontrun{
#' src <- og_api()
#' og_analyses(src, c("OGS-00011:GCST90029007", "ukb-b-19953"))
#' catalogue <- og_analyses(src)
#' }
#' @export
og_analyses <- function(src, ids = NULL, releases = NULL) UseMethod("og_analyses")

#' @export
og_analyses.og_api_source <- function(src, ids = NULL, releases = NULL) {
  # Kept: an R argument's type, checked before any request.
  if (!is.null(ids) && (!is.character(ids) || anyNA(ids) || any(!nzchar(ids)))) {
    og_abort("`ids` must be a character vector of Analysis identifiers, with no missing values.",
             class = "opengwasr_argument")
  }
  releases <- resolve_releases(src, releases)
  if (is.null(ids)) return(analyses_catalogue(src, releases))
  analyses_lookup(src, ids, releases)
}

# The API's limit on ids in one lookup, and on analyses in one catalogue page.
lookup_limit <- 1000L

analyses_lookup <- function(src, ids, releases, call = rlang::caller_env()) {
  if (length(ids) == 0) return(finish_analyses(analyses_frame(NULL), character()))
  chunks <- lapply(dim_chunks(split_dim(length(ids), lookup_limit)), function(at) ids[at])
  resolved <- lapply(chunks, function(chunk) {
    body <- c(list(id = as.list(unique(chunk))), if (!is.null(releases)) list(releases = releases))
    resp <- api_perform(src, "api/gwasinfo", body = body, simplify = TRUE, call = call)
    check_analyses_field(resp, call)
    resolve_analysis_ids(src, chunk, resp)
  })
  raise_analysis_problems(resolved, call)
  for (r in resolved) {
    for (w in r$successors) {
      og_warn(
        c(paste0("`", w$requested_id, "` now points to a later release of that analysis, \"", w$id,
                 "\", whose numbers differ from the ones Legacy OpenGWAS served."),
          i = paste0("Ask for \"", w$id, "\" by name to silence this warning.")),
        class = "opengwasr_legacy_successor", requested_id = w$requested_id, id = w$id, call = call
      )
    }
  }
  bind_frames(lapply(resolved, `[[`, "rows"))
}

# Turn one lookup response into rows for `ids`, in order, plus what went wrong. `resp` is the parsed
# body of POST /api/gwasinfo: `analyses` (one row per Analysis however many ids name it), `opengwas`
# (how each legacy id resolved, or the API's reason it didn't), `retired` and `not_found`.
#
# Kept client-side: the lookup answers 200 and lists these ids rather than refusing them, and a query
# sent with explicit `releases` (as opengwasr always sends) neither refuses an id held by two of them
# (it would read both) nor says an Analysis is retired (ogdb-api `validate_analysis_membership()`).
resolve_analysis_ids <- function(src, ids, resp) {
  analyses <- analyses_frame(resp$analyses)
  canonical <- canonical_ids(analyses)
  legacy <- resp$opengwas
  retired <- resp$retired
  field <- function(df, name) if (is.data.frame(df) && name %in% names(df)) df[[name]] else rep(NA, NROW(df))

  index <- rep(NA_integer_, length(ids))
  unknown <- character()
  api_reasons <- character()
  superseded_by <- list()
  ambiguous <- list()
  successors <- list()
  for (i in seq_along(ids)) {
    id <- ids[[i]]
    hit_retired <- which(field(retired, "id") == id)
    hit_legacy <- which(field(legacy, "id") == id)
    if (length(hit_retired) > 0) {
      # `superseded_by` may be one id or several; keep every one.
      successor <- as.character(unlist(field(retired, "superseded_by")[[hit_retired[[1]]]]))
      unknown <- c(unknown, id)
      superseded_by[[id]] <- api_redact(src, if (length(successor) == 0) NA_character_ else successor)
      next
    }
    candidates <- if (length(hit_legacy) > 0) {
      unlist(legacy$catalog_ids[[hit_legacy[[1]]]])
    } else if (grepl(":", id, fixed = TRUE)) {
      id
    } else {
      canonical[analyses$analysis_id == id]
    }
    candidates <- intersect(candidates, canonical)
    if (length(candidates) == 0) {
      unknown <- c(unknown, id)
      reason <- api_redact(src, field(legacy, "reason")[hit_legacy])
      api_reasons[[id]] <- if (length(reason) == 1) reason else NA_character_
    } else if (length(candidates) > 1) {
      ambiguous[[id]] <- candidates
    } else {
      index[[i]] <- match(candidates, canonical)
      if (length(hit_legacy) > 0 && identical(field(legacy, "match")[[hit_legacy[[1]]]], "successor")) {
        successors[[length(successors) + 1]] <- list(requested_id = id, id = candidates)
      }
    }
  }
  rows <- if (anyNA(index)) analyses_frame(NULL) else finish_analyses(analyses[index, , drop = FALSE], ids)
  list(rows = rows, unknown = unknown, api_reasons = api_reasons, superseded_by = superseded_by,
       ambiguous = ambiguous, successors = successors)
}

# The classes, and `api_error`, are the API's codes for the same answers (`ambiguous_analysis`,
# `analysis_not_found`, `analysis_retired`), so a handler reads them the same way whichever path
# raised them. The message is opengwasr's; the API's own reason, where it gave one, is quoted.
raise_analysis_problems <- function(resolved, call) {
  ambiguous <- do.call(c, lapply(resolved, `[[`, "ambiguous"))
  if (length(ambiguous) > 0) {
    ids <- names(ambiguous)
    og_abort(c("Identifiers that name more than one Analysis:",
               stats::setNames(paste0(ids, ": ", vapply(ambiguous, paste, "", collapse = ", ")), rep("x", length(ids)))),
             class = "opengwasr_ambiguous_analysis", api_error = "ambiguous_analysis", ids = ids,
             candidates = ambiguous, call = call)
  }
  unknown <- unlist(lapply(resolved, `[[`, "unknown"))
  if (length(unknown) > 0) {
    superseded_by <- do.call(c, lapply(resolved, `[[`, "superseded_by"))
    api_reasons <- unlist(lapply(resolved, `[[`, "api_reasons"))[unknown]
    names(api_reasons) <- unknown
    said <- ifelse(unknown %in% names(superseded_by),
                   paste0("retired, superseded by ", vapply(unknown, function(id) paste(superseded_by[[id]] %||% NA, collapse = ", "), "")),
                   api_reasons)
    retired <- length(superseded_by) > 0
    og_abort(c("Identifiers that name no Analysis:",
               stats::setNames(ifelse(is.na(said), unknown, paste0(unknown, ": ", said)), rep("x", length(unknown)))),
             class = c(if (retired) "opengwasr_analysis_retired", "opengwasr_analysis_not_found"),
             api_error = if (retired) "analysis_retired" else "analysis_not_found",
             ids = unknown, api_reasons = api_reasons, superseded_by = superseded_by, call = call)
  }
}

# The catalogue, page by page. The API pages by cursor (`next_after`); a page that has `total` but
# no cursor is followed by `offset`. Rows fewer than `total` at the end are an error, never a
# shorter catalogue.
analyses_catalogue <- function(src, releases, call = rlang::caller_env()) {
  pages <- list()
  collected <- 0L
  after <- NULL
  total <- NULL
  bar <- NULL
  repeat {
    body <- c(list(limit = lookup_limit),
              if (!is.null(after)) list(after = after) else if (collected > 0) list(offset = collected),
              if (!is.null(releases)) list(releases = releases))
    page <- api_perform(src, "api/gwasinfo", body = body, simplify = TRUE, call = call)
    check_analyses_field(page, call)
    frame <- analyses_frame(page$analyses)
    total <- page$total %||% total
    if (is.null(bar) && progress_enabled() && !is.null(total)) {
      bar <- cli::cli_progress_bar("Reading the catalogue", total = ceiling(total / lookup_limit))
    }
    if (!is.null(bar)) cli::cli_progress_update(id = bar)
    pages[[length(pages) + 1]] <- frame
    collected <- collected + nrow(frame)
    after <- page$next_after
    if (is.null(after) && (nrow(frame) == 0 || is.null(total) || collected >= total)) break
  }
  if (!is.null(bar)) cli::cli_progress_done(id = bar)
  if (!is.null(total) && collected < total) {
    og_abort(
      c("The OpenGWAS API returned fewer Analyses than it reports.",
        x = paste0("Read ", collected, " of ", total, ".")),
      class = "opengwasr_api_error", status = 200L, api_error = NA_character_,
      collected = collected, total = total, call = call
    )
  }
  analyses <- bind_frames(pages)
  finish_analyses(analyses, canonical_ids(analyses))
}

# A successful answer with no `analyses` field is malformed; only an explicit empty list is empty.
check_analyses_field <- function(resp, call) {
  if (!is.list(resp) || !"analyses" %in% names(resp)) {
    og_abort("The OpenGWAS API answered without an `analyses` field.",
             class = "opengwasr_api_error", status = 200L, api_error = NA_character_, call = call)
  }
}

# The qualified ids an `ambiguous_analysis` message names (the API gives them only in `message`).
analysis_candidates <- function(message) {
  regmatches(message, gregexpr("OGS-[0-9]+:[^[:space:],;\"')]+", message))[[1]]
}

analyses_frame <- function(analyses) {
  if (is.data.frame(analyses)) return(analyses)
  data.frame(store_id = character(), analysis_id = character(), stringsAsFactors = FALSE)
}

# Add the canonical `id` (assembled here: rows carry the bare `analysis_id` beside `store_id`) and
# `requested_id`, leading the columns.
finish_analyses <- function(analyses, requested) {
  analyses$id <- canonical_ids(analyses)
  analyses$requested_id <- as.character(requested)
  analyses <- analyses[, c("requested_id", "id", setdiff(names(analyses), c("requested_id", "id"))), drop = FALSE]
  rownames(analyses) <- NULL
  analyses
}

# `<store_id>:<analysis_id>` for each row.
canonical_ids <- function(analyses) {
  if (nrow(analyses) == 0) character() else paste0(analyses$store_id, ":", analyses$analysis_id)
}

# Stack data frames, filling a column one of them lacks with NA.
bind_frames <- function(frames) {
  columns <- unique(unlist(lapply(frames, names)))
  frames <- lapply(frames, function(f) {
    for (name in setdiff(columns, names(f))) f[[name]] <- rep(NA, nrow(f))
    f[columns]
  })
  do.call(rbind, frames)
}
