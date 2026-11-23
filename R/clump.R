# Clumping: one variant per independent signal, in an LD Store.
#
# Two ways in, as ADR 0004's clarification note says. `og_tophits()` clumps on the server
# (`/api/tophits` with `clump`), one Job per Analysis, through one more contribution to the
# Association-query engine (`clump_contribution()`). `og_clump()` clumps data the user already has,
# as one set, through `/api/ld/clump` on the LD query engine. Both choose the LD Store with
# `choose_ld_store()`, and both always send the LD Store, `r2` and `kb` explicitly: by default the LD
# Store's own `t_far` and far window (`far_bp`), never the API's defaults (EUR, r² 0.001, 3 Mb).
# Assigned Ancestry matching and its refusals stay on the client, before any Job runs.

#' Clump Associations you already have
#'
#' `og_clump()` clumps a data frame of Associations as one set, such as the top hits of several
#' Analyses together, and returns the rows of the index variants (see `vignette("ld")`).
#'
#' Variants are ranked by |z|; a variant listed several times (in several Analyses) is ranked by its
#' largest |z|, and every Association of an index variant is returned. Starting from the strongest,
#' each index variant absorbs every weaker variant within `kb` kilobases whose r² with it is above
#' `r2` (the rule `plink --clump` uses). Variants the LD Store doesn't hold can't be clumped: they
#' are left out, with a warning of class `opengwasr_ld_absent_variants`.
#'
#' `r2` and `kb` default to the LD Store's Far Threshold (`t_far`) and its far window (`far_bp`,
#' 10,000 kb), which it can always answer exactly. A pair it can't answer exactly is refused by the
#' API (`opengwasr_ld_refusal`, with `reason`, `t_far`, `near_bp` and `far_bp`).
#'
#' @param src A Source, from [og_api()].
#' @param data A data frame of Associations, with `alid` and `z`, and `id` for the LD Store to be
#'   matched by the Analyses' Assigned Ancestry.
#' @param ld An LD Store's name, an ancestry code (its LD Store), or `NULL` to match the Assigned
#'   Ancestry of the Analyses in `data` (refused, with class `opengwasr_ld_unmatched`, when they
#'   differ, when one is Unassigned, or when `data` has no `id`).
#' @param r2 The r² above which a variant joins an index variant's clump; `NULL` for the LD Store's
#'   `t_far`.
#' @param kb The window, in kilobases; `NULL` for the LD Store's far window.
#' @return The rows of `data` whose variant is an index variant, in their order in `data`, with
#'   columns `clump_ld`, `clump_ld_choice`, `clump_r2`, `clump_kb` and `clump_size` (the variants
#'   the index variant stands for, itself included), and [og_provenance()] attached: `ld`,
#'   `ld_choice`, `absent` and `clump`.
#' @examples
#' \dontrun{
#' src <- og_api()
#' top <- og_tophits(src, c("OGS-00011:GCST90029007", "OGS-00011:GCST90002412"), clump = FALSE)
#' joint <- og_clump(src, top)
#' table(joint$id)
#' }
#' @export
og_clump <- function(src, data, ld = NULL, r2 = NULL, kb = NULL) UseMethod("og_clump")

#' @export
og_clump.og_api_source <- function(src, data, ld = NULL, r2 = NULL, kb = NULL) {
  check_clump_settings(r2, kb)
  # Kept: an R argument's type (the scores sent are |z|).
  if (!is.data.frame(data) || !is.numeric(data$z)) {
    og_abort("`data` must be a data frame of Associations, with `alid` and `z` columns.", class = "opengwasr_argument")
  }
  ld_query(src, "api/ld/clump", data, ld, list(
    pieces = function(n) list(seq_len(n)),  # one set, one Job
    body = function(alids, store) {
      score <- tapply(abs(data$z), data$alid, max)
      c(list(scores = as.list(unname(score[alids]))), clump_settings(store, r2, kb))
    },
    assemble = function(results, ctx) {
      rows <- result_rows(results)
      size <- table(rows$lead)
      out <- data[data$alid %in% names(size), , drop = FALSE]
      attr(out, "og_provenance") <- NULL
      rownames(out) <- NULL
      settings <- clump_settings(ctx$store, r2, kb)
      clump_columns(out, ctx$store$ld_store, ctx$choice, settings$r2, settings$kb, as.integer(size[out$alid]))
    },
    provenance = function(results, ctx) {
      settings <- clump_settings(ctx$store, r2, kb)
      leads <- length(unique(result_rows(results)$lead))
      list(clump = clump_summary(NA_character_, ctx$store$ld_store, ctx$choice, settings$r2, settings$kb,
                                 length(ctx$alids), length(ctx$absent), leads))
    }))
}

# Kept: R argument types. Which `r2` and `kb` an LD Store can answer is the API's to say.
check_clump_settings <- function(r2, kb, call = rlang::caller_env()) {
  one_number <- function(x) is.null(x) || (is.numeric(x) && length(x) == 1 && !is.na(x))
  if (!one_number(r2)) og_abort("`r2` must be one number, or `NULL`.", class = "opengwasr_argument", call = call)
  if (!one_number(kb)) og_abort("`kb` must be one number, or `NULL`.", class = "opengwasr_argument", call = call)
}

# The r² and window sent: the user's, else the LD Store's own `t_far` and far window.
clump_settings <- function(store, r2, kb) {
  list(r2 = r2 %||% store$t_far, kb = kb %||% (store$far_bp / 1000))
}

# The clumping provenance columns, after the Association columns (vignette: Clumping provenance).
clump_columns <- function(rows, ld, choice, r2, kb, size) {
  n <- nrow(rows)
  rows$clump_size <- NULL
  rows$clump_ld <- rep_len(as.character(ld), n)
  rows$clump_ld_choice <- rep_len(as.character(choice), n)
  rows$clump_r2 <- rep_len(as.numeric(r2), n)
  rows$clump_kb <- rep_len(as.numeric(kb), n)
  rows$clump_size <- as.integer(size)
  rows
}

# `og_provenance(x)$clump`: one row per Analysis clumped (one, with `id` NA, for one set).
clump_summary <- function(id, ld, choice, r2, kb, hits, absent, leads) {
  data.frame(id = id, ld = ld, ld_choice = choice, r2 = r2, kb = kb, hits = as.numeric(hits),
             absent = as.numeric(absent), leads = as.numeric(leads), stringsAsFactors = FALSE)
}

# The engine contribution that clumps each Analysis's top hits on the server (`og_tophits()`), made
# before any Job runs, so a refusal costs none. One LD Store per Analysis: `ld` for all of them, or
# the one matching its Assigned Ancestry (an Unassigned one is refused first). It has no split
# dimension of its own: it follows the Analyses (`along = "scope"`, one per Job), adding `clump`
# to each Job's body; its Jobs are submitted grouped by LD Store (`group`); its `finish` adds the clumping columns and provenance and warns about
# top hits the LD Store doesn't hold.
clump_contribution <- function(src, clump, info, call) {
  ancestry <- stats::setNames(as.character(info$assigned_ancestry), info$id)
  group <- if (is.null(clump$ld)) ancestry else rep("ld", length(ancestry))
  groups <- unique(group)
  groups <- groups[order(!is.na(groups))]  # Unassigned first: refused before anything else is chosen
  per <- data.frame(id = info$id, ld = NA_character_, ld_choice = NA_character_, r2 = NA_real_, kb = NA_real_,
                    stringsAsFactors = FALSE)
  for (g in groups) {
    members <- which(group %in% g)
    chosen <- tryCatch(choose_ld_store(src, clump$ld, ancestry[members], call),
                       opengwasr_ld_unmatched = function(e) {
                         advice <- c(i = "Or pass `clump = FALSE` for unclumped top hits.")
                         e$message <- paste0(e$message, "\n", rlang::format_error_bullets(advice))
                         rlang::cnd_signal(e)
                       })
    settings <- clump_settings(chosen$store, clump$r2, clump$kb)
    per$ld[members] <- chosen$store$ld_store
    per$ld_choice[members] <- chosen$choice
    per$r2[members] <- settings$r2
    per$kb[members] <- settings$kb
    inform_clumping(info$id[members], chosen, g, settings)
  }
  list(
    along = "scope",
    group = function(chunk) per$ld[[chunk[[1]]]],
    body = function(chunk) {
      if (length(chunk) != 1) {
        og_abort("Clumped top hits are one Analysis per Job. This is a bug in opengwasr.", class = "opengwasr_internal")
      }
      list(clump = list(store = per$ld[[chunk]], r2 = per$r2[[chunk]], kb = per$kb[[chunk]]))
    },
    finish = function(rows, results, pieces, call) {
      at <- vapply(pieces, function(p) as.integer(p$scope[[1]]), 1L)
      count <- function(name) vapply(results, function(r) as.numeric(r$job$metadata$clump[[name]] %||% NA), 0)
      summary <- clump_summary(per$id[at], per$ld[at], per$ld_choice[at], per$r2[at], per$kb[at],
                               count("hits"), count("absent"), count("leads"))
      summary <- summary[order(match(summary$id, per$id)), , drop = FALSE]
      rownames(summary) <- NULL
      warn_clump_absent(summary, call)
      by <- match(rows$id, per$id)
      size <- rows$clump_size %||% integer()
      list(value = clump_columns(rows, per$ld[by], per$ld_choice[by], per$r2[by], per$kb[by], size),
           provenance = list(clump = summary))
    }
  )
}

# What is about to be clumped, where and how: one message per LD Store chosen, with progress on
# (`opengwasr.progress`), so scripts stay quiet.
inform_clumping <- function(ids, chosen, ancestry, settings) {
  if (!progress_enabled()) return(invisible())
  who <- if (length(ids) == 1) ids else paste0(length(ids), " Analyses (", paste(utils::head(ids, 3), collapse = ", "),
                                               if (length(ids) > 3) ", ...", ")")
  their <- if (length(ids) == 1) "its" else "their"
  how <- switch(chosen$choice,
                matched = paste0("matched to ", their, " Assigned Ancestry (", ancestry, ")"),
                # `ld` named an ancestry, or the Assigned Ancestry has several LD Stores and one is declared its default.
                ancestry_default = if (identical(ancestry, "ld")) {
                  paste0("the default LD Store of ancestry ", chosen$store$ancestry, ", as `ld` asks")
                } else {
                  paste0("the declared default LD Store of ", their, " Assigned Ancestry (", ancestry, ")")
                },
                user = "as `ld` names it")
  rlang::inform(c(i = paste0("Clumping ", who, " in LD Store ", chosen$store$ld_store, ", ", how, ": ",
                             clump_setting_text(settings$r2, settings$kb), ".")),
                class = "opengwasr_clumping")
}

clump_setting_text <- function(r2, kb) {
  paste0("r\u00b2 ", signif(r2, 3), " within ", big_number(kb), " kb")
}

big_number <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)

# Top hits the LD Store doesn't hold can't be clumped: the API leaves them out, and counts them.
warn_clump_absent <- function(summary, call) {
  absent <- summary[!is.na(summary$absent) & summary$absent > 0, , drop = FALSE]
  if (nrow(absent) == 0) return(invisible())
  n <- sum(absent$absent)
  hits <- sum(absent$hits)
  big <- big_number
  message <- if (nrow(absent) == 1) {
    paste0(big(n), " of the ", big(hits), " top hits of ", absent$id, " aren't in LD Store ", absent$ld,
           ", so they can't be clumped and were left out.")
  } else {
    c(paste0(big(n), " of the ", big(hits), " top hits of ", nrow(absent),
             " Analyses aren't in their LD Stores, so they can't be clumped and were left out."),
      i = "`og_provenance(x)$clump` gives the count for each Analysis.")
  }
  og_warn(message, class = "opengwasr_clump_absent", count = n, ids = absent$id, call = call)
}

# The provenance lines of a clumped result.
format_clump <- function(clump) {
  big <- big_number
  shown <- utils::head(clump, 5)
  one_set <- is.na(shown$id)
  lines <- paste0("\u2022 Clumped: ", ifelse(one_set, "one set", shown$id), " in ", shown$ld,
                  " (", shown$ld_choice, "), ", clump_setting_text(shown$r2, shown$kb), "; ",
                  big(shown$hits), ifelse(one_set, " variants, ", " top hits, "), big(shown$absent),
                  " not in the LD Store, ", big(shown$leads), " index variants")
  c(lines, if (nrow(clump) > 5) paste0("  ... and ", nrow(clump) - 5, " more Analyses: see `og_provenance(x)$clump`"))
}
