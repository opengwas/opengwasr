# Clumping: one variant per independent signal, in an LD Store or a PLINK Reference: the Source's
# LD resource (ld_resource(), R/ld-stores.R).
#
# Two ways in, as ADR 0004's clarification note says. `og_tophits()` clumps on the server
# (`/api/tophits` with `clump`), one Job per Analysis, through one more contribution to the
# Association-query engine (`clump_contribution()`). `og_clump()` clumps data the user already has
# through `/api/ld/clump` on the LD query engine: each Analysis's rows on their own (one set per
# Analysis, one Job each), or all as one set. Both choose an LD Store per Analysis with
# `choose_ld_store_each()` (or one for the one set, `choose_ld_store()`), and both always send the LD
# Store, `r2` and `kb` explicitly: by default the LD Store's own `t_far` and far window (`far_bp`),
# never the API's defaults (EUR, r² 0.001, 3 Mb). Assigned Ancestry matching and its refusals stay
# on the client, before any Job runs.

#' Clump Associations you already have
#'
#' `og_clump()` clumps a data frame of Associations, such as top hits fetched unclumped from another
#' Source, and returns the rows of the index variants (see `vignette("ld")`). By default it clumps
#' each Analysis's rows on their own, as [og_tophits()] does, so `og_clump(src, og_tophits(src, ids,
#' clump = FALSE, limit = 10000))` gives each Analysis the same index variants as `og_tophits(src,
#' ids)`. `by_analysis = FALSE` clumps all the rows as one set, such as the instruments of several
#' exposures together, so that their index variants compete.
#'
#' Variants are ranked by |z|; a variant listed several times in a set is ranked by its largest |z|,
#' and every Association of an index variant in that set is returned. Starting from the strongest,
#' each index variant absorbs every weaker variant within `kb` kilobases whose r² with it is above
#' `r2` (the rule `plink --clump` uses). Variants the LD Store doesn't hold can't be clumped: they
#' are left out, with a warning of class `opengwasr_ld_absent_variants` (one for each LD Store).
#'
#' `r2` and `kb` default to the LD Store's Far Threshold (`t_far`) and its far window (`far_bp`,
#' 10,000 kb), which it can always answer exactly. A pair it can't answer exactly is refused by the
#' API (`opengwasr_ld_refusal`, with `reason`, `t_far`, `near_bp` and `far_bp`).
#'
#' The LD is the Source's LD resource. On the API Source that is its `ld_store`
#' (`og_api(ld_store = )`), else, for each Analysis, the LD Store matching its Assigned Ancestry, one
#' Job per Analysis. An Unassigned Analysis is refused, with class `opengwasr_ld_unmatched`, before
#' any Job runs; so is `data` with no `id` (one set, with no ancestry to match), and, with
#' `by_analysis = FALSE`, Analyses whose Assigned Ancestries differ. For other LD, pass another
#' Source, such as `og_api(inherit = src, ld_store = "EUR")`.
#'
#' On a Local Source, PLINK clumps on your machine with its PLINK Reference (`og_local(bfile = )`),
#' one run per Analysis. Its LD isn't adjusted for principal components, so the first call in a session to read each PLINK Reference warns, with class `opengwasr_plink_unadjusted` (see [og_local()]). `r2` and `kb` are required (there are no defaults to take, so without them
#' it stops, with class `opengwasr_plink_settings_required`). Variants are matched by rsID, from
#' `data`'s `rsid` column if it has one, else the Source's Store Releases' variant table; those with
#' no rsID match are left out, with a message (class `opengwasr_plink_unmatched`, `count`). An
#' LD-only Local Source (`og_local(bfile = )` with no `gwas`) has nothing to look an ALID up in, so
#' `data` without an `rsid` column is refused (class `opengwasr_gwas_store_required`, with the ALIDs
#' in `alids`, and the ways round in `forms`: see [og_local()]). A Local Source with no LD resource
#' is refused (`opengwasr_ld_resource_required`). Provenance's `clump` names the reference's path as
#' `ld`, and `clump_ld` is the same path.
#'
#' @param src A Source, from [og_api()] or [og_local()], which carries the LD resource.
#' @param data A data frame of Associations, with `alid` and `z`, and `id`: the Analysis of each row,
#'   by which the rows are clumped and the LD Store is matched to the Analysis's Assigned Ancestry.
#' @param r2 The r² above which a variant joins an index variant's clump; `NULL` for the LD Store's
#'   `t_far`. Required with a PLINK Reference.
#' @param kb The window, in kilobases; `NULL` for the LD Store's far window. Required with a PLINK
#'   Reference.
#' @param by_analysis `TRUE` (the default) clumps the rows of each Analysis (`id`) on their own;
#'   `FALSE` clumps all the rows as one set. Rows with no `id` column are one set either way.
#' @return The rows of `data` whose variant is an index variant of their set, in their order in
#'   `data`, with columns `clump_ld`, `clump_ld_choice`, `clump_r2`, `clump_kb` and `clump_size`
#'   (the variants the index variant stands for, itself included), and [og_provenance()] attached:
#'   `ld` and `ld_choice` (each LD Store used, and how they were chosen), `absent` and `clump`, one
#'   row per Analysis clumped (one, with `id` `NA`, for one set), whose `clumps` is `"computed"`.
#' @examples
#' \dontrun{
#' src <- og_api()
#' ids <- c("OGS-00011:GCST90029007", "OGS-00011:GCST90002412")
#' top <- og_tophits(src, ids, clump = FALSE, limit = 10000)
#' each <- og_clump(src, top)                        # each Analysis on its own
#' joint <- og_clump(src, top, by_analysis = FALSE)  # all as one set
#' table(joint$id)
#' }
#' @export
og_clump <- function(src, data, r2 = NULL, kb = NULL, by_analysis = TRUE) UseMethod("og_clump")

#' @export
og_clump.og_api_source <- function(src, data, r2 = NULL, kb = NULL, by_analysis = TRUE) {
  check_clump_settings(r2, kb)
  check_plink_settings(ld_resource(src)$ld, r2, kb)
  # Kept: an R argument's type (the scores sent are |z|).
  if (!is.data.frame(data) || !is.numeric(data$z)) {
    og_abort("`data` must be a data frame of Associations, with `alid` and `z` columns.", class = "opengwasr_argument")
  }
  if (!rlang::is_bool(by_analysis)) {
    og_abort("`by_analysis` must be `TRUE` or `FALSE`.", class = "opengwasr_argument")
  }
  ld_query(src, "api/ld/clump", data, list(
    # Each Analysis's rows a set, or all of them one; each set one Job.
    sets = if (by_analysis && !is.null(data$id)) data$id,
    pieces = function(n) list(seq_len(n)),
    body = function(alids, store, rows) {
      score <- tapply(abs(data$z[rows]), data$alid[rows], max)
      c(list(scores = as.list(unname(score[alids]))), clump_settings(store, r2, kb))
    },
    local = function(ctx) local_clump(data, ctx, r2, kb),
    assemble = function(results, ctx) {
      # Each set's index variants: its rows of them, kept in `data`'s order.
      keep <- rep(FALSE, nrow(data))
      size <- rep(NA_integer_, nrow(data))
      set_of <- rep(NA_integer_, nrow(data))
      for (k in seq_along(ctx$sets)) {
        set <- ctx$sets[[k]]
        leads <- table(result_rows(set$results)$lead)
        mine <- set$rows[data$alid[set$rows] %in% names(leads)]
        keep[mine] <- TRUE
        size[mine] <- as.integer(leads[data$alid[mine]])
        set_of[mine] <- k
      }
      clump_rows(data, keep, size, set_of, ctx$sets, r2, kb)
    },
    provenance = function(results, ctx) {
      list(clump = bind_frames(lapply(ctx$sets, function(set) {
        settings <- clump_settings(set$store, r2, kb)
        leads <- length(unique(result_rows(set$results)$lead))
        clump_summary(set$id, set$store$ld_store, set$choice, settings$r2, settings$kb,
                      length(set$alids), length(set$absent), leads)
      })))
    }))
}

# A Local Source clumps with its PLINK Reference, its rsIDs looked up in its own Store Releases (an
# LD-only one has none to look in). The same method: the engine reads the call from the frame
# UseMethod() dispatched to.
#' @export
og_clump.og_local_source <- og_clump.og_api_source

# `og_clump()` over a PLINK Reference: each set's variants (each Analysis's, or all as one) matched by
# rsID, scored by their largest |z| in the set, and clumped by PLINK, one run per set; the rows of
# `data` of each set's index variants, as for an LD Store, with the same columns.
local_clump <- function(data, ctx, r2, kb) {
  # Each row's rsID: its own, from an `rsid` column (as og_tophits() matches top hits), else its
  # ALID's, looked up. NA: not in the reference, so not clumped.
  rsid <- if (is.character(data$rsid)) {
    own <- rsid_or_na(tolower(data$rsid))
    ifelse(own %in% plink_bim(ctx$plink)$rsid, own, NA_character_)
  } else {
    ctx$matched$rsid[match(data$alid, ctx$matched$alid)]
  }
  keep <- rep(FALSE, nrow(data))
  size <- rep(NA_integer_, nrow(data))
  set_of <- rep(NA_integer_, nrow(data))
  summary <- vector("list", length(ctx$sets))
  for (k in seq_along(ctx$sets)) {
    set <- ctx$sets[[k]]
    at <- set$rows[!is.na(rsid[set$rows])]
    clumped <- plink_clump(ctx$plink, rsid[at], data$z[at], r2, kb, ctx$call)
    lead <- rsid[at] %in% clumped$leads
    keep[at[lead]] <- TRUE
    size[at[lead]] <- clumped$size[lead]
    set_of[at[lead]] <- k
    alids <- unique(data$alid[set$rows])
    summary[[k]] <- clump_summary(set$id, set$store$ld_store, set$choice, r2, kb, length(alids),
                                  sum(!alids %in% data$alid[at]), length(clumped$leads))
  }
  list(value = clump_rows(data, keep, size, set_of, ctx$sets, r2, kb),
       provenance = list(clump = bind_frames(summary)))
}

# The rows of `data` kept (`keep`), in its order, with the clumping columns of the set each is in
# (`set_of`, an index into `sets`) and its clump's `size`.
clump_rows <- function(data, keep, size, set_of, sets, r2, kb) {
  out <- data[keep, , drop = FALSE]
  attr(out, "og_provenance") <- NULL
  rownames(out) <- NULL
  per <- lapply(sets, function(set) {
    settings <- clump_settings(set$store, r2, kb)
    list(ld = set$store$ld_store, choice = set$choice, r2 = settings$r2, kb = settings$kb)
  })
  field <- function(name, type) vapply(per, function(p) as.vector(p[[name]], type), vector(type, 1))[set_of[keep]]
  clump_columns(out, field("ld", "character"), field("choice", "character"), field("r2", "numeric"),
                field("kb", "numeric"), size[keep])
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

# `og_provenance(x)$clump`: one row per Analysis clumped (one, with `id` NA, for one set). `clumps`
# says how the Clumps were made (ADR 0006): "computed" on the fly, as every clumping is until Store
# Releases carry stored Clumps, which will be "stored".
clump_summary <- function(id, ld, choice, r2, kb, hits, absent, leads, clumps = "computed") {
  data.frame(id = id, ld = ld, ld_choice = choice, r2 = r2, kb = kb, hits = as.numeric(hits),
             absent = as.numeric(absent), leads = as.numeric(leads), clumps = clumps, stringsAsFactors = FALSE)
}

# The contribution (CONTRIBUTING.md) that clumps each Analysis's top hits on the server, one Job and
# one LD Store per Analysis: the Source's for all, or the one matching its Assigned Ancestry (an
# Unassigned one is refused first, before any Job runs). Its `finish` adds the clumping columns and provenance
# and warns about top hits the LD Store doesn't hold.
clump_contribution <- function(src, clump, info, call) {
  # The Source's LD resource; with none, each Analysis's Assigned Ancestry.
  resource <- ld_resource(src, call, clumping = TRUE)
  if (is_plink_reference(resource$ld)) {
    return(plink_clump_contribution(choose_ld_store(src, call = call), clump, info, call))
  }
  ancestry <- stats::setNames(as.character(info$assigned_ancestry), info$id)
  # One LD Store per ancestry when matching, else one for all ("source").
  each <- tryCatch(choose_ld_store_each(src, ancestry, call),
                   opengwasr_ld_unmatched = function(e) {
                     advice <- c(i = "Or pass `clump = FALSE` for unclumped top hits.")
                     e$message <- paste0(e$message, "\n", rlang::format_error_bullets(advice))
                     rlang::cnd_signal(e)
                   })
  per <- data.frame(id = info$id, ld = NA_character_, ld_choice = NA_character_, r2 = NA_real_, kb = NA_real_,
                    stringsAsFactors = FALSE)
  for (k in seq_along(each$groups)) {
    members <- which(each$of == k)
    chosen <- each$chosen[[k]]
    g <- each$groups[[k]]
    settings <- clump_settings(chosen$store, clump$r2, clump$kb)
    per$ld[members] <- chosen$store$ld_store
    per$ld_choice[members] <- chosen$choice
    per$r2[members] <- settings$r2
    per$kb[members] <- settings$kb
    inform_clumping(info$id[members], chosen, g, settings)
  }
  list(
    along = "scope",
    analyses_per_job = 1L,  # each Analysis in its own LD Store, so `limit` counts its index variants
    truncated = function(ids) truncated_message(analyses_who(ids), clump$limit, "index variants", clump$threshold),
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
           provenance = list(clump = summary, ld_from = resource$from))
    }
  )
}

# What is about to be clumped, where and how: one message per LD Store chosen, with progress on
# (`opengwasr.progress`), so scripts stay quiet. `group` is the Assigned Ancestry matched, or where
# the LD came from ("source"); the message carries `ld_from`.
inform_clumping <- function(ids, chosen, group, settings) {
  if (!progress_enabled()) return(invisible())
  who <- if (length(ids) == 1) ids else paste0(length(ids), " Analyses (", some_ids(ids, 3), ")")
  their <- if (length(ids) == 1) "its" else "their"
  how <- switch(chosen$choice,
                matched = paste0("matched to ", their, " Assigned Ancestry (", group, ")"),
                # The Source's LD resource named an ancestry, or the Assigned Ancestry has several
                # LD Stores and one is declared its default.
                ancestry_default = if (chosen$from != "ancestry") {
                  paste0("the default LD Store of ancestry ", chosen$store$ancestry, ", as the Source's LD resource asks")
                } else {
                  paste0("the declared default LD Store of ", their, " Assigned Ancestry (", group, ")")
                },
                user = "as the Source's LD resource names it")
  rlang::inform(c(i = paste0("Clumping ", who, " in LD Store ", chosen$store$ld_store, ", ", how, ": ",
                             clump_setting_text(settings$r2, settings$kb), ".")),
                class = "opengwasr_clumping", ld_from = chosen$from)
}

clump_setting_text <- function(r2, kb) {
  paste0("r\u00b2 ", signif(r2, 3), " within ", big_number(kb), " kb")
}

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
                  " not in the LD Store, ", big(shown$leads), " index variants, ",
                  shown$clumps %||% "computed")
  c(lines, if (nrow(clump) > 5) paste0("  ... and ", nrow(clump) - 5, " more Analyses: see `og_provenance(x)$clump`"))
}
