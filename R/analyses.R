# Finding Analyses, and the one owner of Analysis identifiers.
#
# An Analysis is named `<store_id>:<analysis_id>` (its canonical id), or by a bare analysis id, or by
# a legacy OpenGWAS id. `resolve_analysis_ids()` turns what the user typed into canonical ids, with
# the successor warning and the ambiguous, unknown and retired errors. Later tickets that take
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
#'   `opengwasr_unknown_analysis` (`opengwasr_retired_analysis` too when retired). The condition
#'   carries opengwasr's sentence for each identifier in `reasons`, the API's own reason where it
#'   gave one in `api_reasons` (`NA` otherwise), and, for a retired Analysis, every successor the
#'   API named in `superseded_by`.
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
  releases <- resolve_releases(src, releases)
  if (is.null(ids)) return(analyses_catalogue(src, releases))
  if (!is.character(ids) || anyNA(ids) || any(!nzchar(ids))) {
    og_abort("`ids` must be a character vector of Analysis identifiers, with no missing values.",
             class = "opengwasr_argument")
  }
  analyses_lookup(src, ids, releases)
}

# The API's limit on ids in one lookup, and on analyses in one catalogue page.
lookup_limit <- 1000L

analyses_lookup <- function(src, ids, releases, call = rlang::caller_env()) {
  if (length(ids) == 0) return(finish_analyses(analyses_frame(NULL), character()))
  chunks <- split(ids, ceiling(seq_along(ids) / lookup_limit))
  resolved <- lapply(unname(chunks), function(chunk) {
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
# (how each legacy id resolved), `retired` and `not_found`.
resolve_analysis_ids <- function(src, ids, resp) {
  analyses <- analyses_frame(resp$analyses)
  canonical <- canonical_ids(analyses)
  legacy <- resp$opengwas
  retired <- resp$retired
  field <- function(df, name) if (is.data.frame(df) && name %in% names(df)) df[[name]] else rep(NA, NROW(df))

  index <- rep(NA_integer_, length(ids))
  unknown <- character()
  reasons <- character()
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
      successor <- api_redact(src, successor[!is.na(successor)])
      unknown <- c(unknown, id)
      reasons[[id]] <- paste0("It belongs to a retired GWAS Store", if (length(successor) == 0)
        " and has no successor." else paste0("; use ", paste0("\"", successor, "\"", collapse = " or "), " instead."))
      superseded_by[[id]] <- if (length(successor) == 0) NA_character_ else successor
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
      api_reasons[[id]] <- if (length(reason) == 1 && !is.na(reason)) reason else NA_character_
      reasons[[id]] <- if (length(reason) == 1 && !is.na(reason)) paste0(reason, ".")
        else "No Analysis with this identifier is held by the GWAS Stores selected."
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
  list(rows = rows, unknown = unknown, reasons = reasons, api_reasons = api_reasons, superseded_by = superseded_by,
       ambiguous = ambiguous, successors = successors)
}

raise_analysis_problems <- function(resolved, call) {
  ambiguous <- do.call(c, lapply(resolved, `[[`, "ambiguous"))
  if (length(ambiguous) > 0) {
    ids <- names(ambiguous)
    og_abort(
      c(paste0(length(ids), if (length(ids) == 1) " identifier names" else " identifiers name",
               " more than one Analysis."),
        stats::setNames(
          vapply(ids, function(id) paste0("`", id, "`: use one of ",
                                          paste0("\"", ambiguous[[id]], "\"", collapse = ", "), "."), ""),
          rep("x", length(ids)))),
      class = "opengwasr_ambiguous_analysis", ids = ids, candidates = ambiguous, call = call
    )
  }
  unknown <- unlist(lapply(resolved, `[[`, "unknown"))
  if (length(unknown) > 0) {
    reasons <- unlist(lapply(resolved, `[[`, "reasons"))
    superseded_by <- do.call(c, lapply(resolved, `[[`, "superseded_by"))
    api_reasons <- unlist(lapply(resolved, `[[`, "api_reasons"))
    api_reasons <- c(api_reasons, stats::setNames(rep(NA_character_, sum(!unknown %in% names(api_reasons))),
                                                  setdiff(unknown, names(api_reasons))))
    og_abort(
      c(paste0(length(unknown), if (length(unknown) == 1) " identifier names" else " identifiers name",
               " no Analysis:"),
        stats::setNames(paste0(unknown, ": ", reasons[unknown]), rep("x", length(unknown)))),
      class = c(if (length(superseded_by) > 0) "opengwasr_retired_analysis", "opengwasr_unknown_analysis"),
      ids = unknown, reasons = reasons[unknown], api_reasons = api_reasons[unknown], superseded_by = superseded_by, call = call
    )
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
    if (is.null(bar) && isTRUE(getOption("opengwasr.progress", interactive())) && !is.null(total)) {
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
