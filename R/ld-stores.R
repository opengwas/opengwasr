# LD Stores: what the API serves, and the one owner of choosing one.
#
# An LD Store holds the LD of one reference panel of one ancestry. Every LD operation (the matrix and
# proxies here, clumping in R/clump.R) names exactly one, and gets it from `choose_ld_store()`: never
# write a second chooser.

#' List the LD Stores
#'
#' `og_ld_stores()` lists the LD Stores the API serves, one row each, with what each holds: see
#' `vignette("ld")`.
#'
#' @param src A Source, from [og_api()].
#' @return A data frame: `ld_store` (the LD Store's name), `ancestry`, `n` (people in the reference
#'   panel), `t_far` (the Far Threshold), `near_bp`, `far_bp`, `n_variants`, `default`, which will
#'   mark each ancestry's default LD Store once the API declares one (`NA` until then), and `ready`.
#'   An LD Store that isn't ready can't answer: choosing it is refused.
#' @examples
#' \dontrun{
#' og_ld_stores(og_api())
#' }
#' @export
og_ld_stores <- function(src) UseMethod("og_ld_stores")

#' @export
og_ld_stores.og_api_source <- function(src) {
  ld_stores(src)
}

ld_store_columns <- c("ld_store", "ancestry", "n", "t_far", "near_bp", "far_bp", "n_variants", "default", "ready")

# GET /api/ld/stores as a data frame (`ld_store`, not the API's `store`).
ld_stores <- function(src, call = rlang::caller_env()) {
  stores <- api_perform(src, "api/ld/stores", simplify = TRUE, call = call)$stores
  if (!is.data.frame(stores)) stores <- data.frame(store = character())
  stores$ld_store <- stores$store
  stores$ready <- if (is.null(stores$ready)) rep(TRUE, nrow(stores)) else stores$ready %in% TRUE
  # The API has no default flag yet: NA says "not declared", never FALSE.
  stores$default <- if (is.null(stores$default)) rep(NA, nrow(stores)) else as.logical(stores$default)
  for (col in setdiff(ld_store_columns, names(stores))) stores[[col]] <- rep(NA, nrow(stores))
  stores <- stores[ld_store_columns]
  rownames(stores) <- NULL
  stores
}

#' Choose the one LD Store an LD operation uses.
#'
#' @param src The Source.
#' @param ld A PLINK Reference from [og_plink_reference()] (`"user"`; `store` is then `list(ld_store =
#'   <its path>)` and `plink` the reference), an LD Store's name (wins: `"user"`), an ancestry code (that ancestry's LD Store:
#'   `"ancestry_default"`), or `NULL` (matched from `ancestries`: `"matched"`, or
#'   `"ancestry_default"` when several LD Stores serve that ancestry and its declared default chose).
#' @param ancestries `NULL` when the operation has no Analyses behind its variants, else a character
#'   vector of the Assigned Ancestry of each Analysis (`NA` for Unassigned), named by Analysis id.
#'   Only read when `ld` is `NULL`.
#' @return `list(store, choice)` (and `plink` for a PLINK Reference): `store` is the LD Store's row of [og_ld_stores()] (one-row data
#'   frame), `choice` is how it was chosen.
#'
#' Refuses with `opengwasr_ld_unmatched` (a refusal, with the LD Stores in `e$ld_stores`) when `ld`
#' is `NULL` and no single ancestry follows from `ancestries`: none given, an Unassigned Analysis, or
#' several ancestries. Kept: the API would answer a request that names no LD Store from European LD.
#' @noRd
choose_ld_store <- function(src, ld, ancestries = NULL, call = rlang::caller_env()) {
  # A PLINK Reference is the user's own choice, read locally: no LD Store row, and no API call.
  # `store$ld_store` is its path, which is what every result records as the LD used.
  if (is_plink_reference(ld)) {
    return(list(store = list(ld_store = ld$path), choice = "user", plink = ld))
  }
  # Kept: an R argument's type.
  if (!is.null(ld) && (!is.character(ld) || length(ld) != 1 || is.na(ld) || !nzchar(ld))) {
    og_abort("`ld` must be one LD Store name or ancestry code, or `NULL`.", class = "opengwasr_argument", call = call)
  }
  if (inherits(src, "og_no_source")) {
    source_required(call, "An LD Store is named, or chosen by ancestry, from the API's list of LD Stores.")
  }
  stores <- ld_stores(src, call)
  choices <- function() paste0("\"", unique(stores$ancestry), "\"", collapse = ", ")
  refuse <- function(what, ...) {
    og_abort(c(what, ..., i = paste0("Choose the LD with `ld`, from: ", choices(), " (see `og_ld_stores()`).")),
             class = "opengwasr_ld_unmatched", refusal = TRUE, ld_stores = stores, call = call)
  }
  not_ready <- function(names) {
    og_abort(c(paste0("LD Store ", paste(names, collapse = ", "), " isn't ready, so it can't answer yet."),
               i = "See `ready` in `og_ld_stores()`; try again later or choose another LD Store."),
             class = "opengwasr_ld_not_ready", refusal = TRUE, ld_stores = stores, call = call)
  }
  of_ancestry <- function(code) {
    own <- stores[stores$ancestry %in% code, , drop = FALSE]
    if (nrow(own) > 0 && !any(own$ready)) not_ready(own$ld_store)
    own <- own[own$ready, , drop = FALSE]
    if (nrow(own) > 1 && any(own$default %in% TRUE)) own <- own[own$default %in% TRUE, , drop = FALSE]
    own
  }
  if (!is.null(ld)) {
    # Kept: the API takes a store or an ancestry, and only the list says which `ld` is; and the
    # LD Store's row (`far_bp`, `t_far`) is needed here for filling and provenance anyway.
    if (ld %in% stores$ld_store) {
      named <- stores[stores$ld_store == ld, , drop = FALSE]
      if (!named$ready) not_ready(ld)
      return(list(store = named, choice = "user"))
    }
    own <- of_ancestry(ld)
    if (nrow(own) == 0) {
      og_abort(c(paste0("`ld` = \"", ld, "\" is neither an LD Store's name nor the ancestry of one."),
                 i = paste0("Choose an ancestry from: ", choices(), "; or one of the names in `og_ld_stores()`.")),
               class = "opengwasr_argument", call = call)
    }
    if (nrow(own) > 1) {
      refuse(paste0("Several LD Stores serve ancestry ", ld, ", and none is declared its default."),
             i = paste0("Name one of: ", paste0("\"", own$ld_store, "\"", collapse = ", "), "."))
    }
    return(list(store = own, choice = "ancestry_default"))
  }
  if (is.null(ancestries)) {
    refuse("Can't choose an LD Store: these variants come with no Analyses, so there is no Assigned Ancestry to match.")
  }
  unassigned <- is.na(ancestries)
  if (any(unassigned)) {
    who <- names(ancestries)[unassigned]
    refuse(paste0("Can't choose an LD Store for ", some_ids(who, 5), ": its ancestry is Unassigned."),
           i = "Its allele frequencies match no single ancestry confidently.")
  }
  if (length(unique(ancestries)) > 1) {
    by <- vapply(split(names(ancestries), ancestries), some_ids, "", n = 3)
    refuse("Can't choose one LD Store for these Analyses: their Assigned Ancestries differ.",
           stats::setNames(paste0(names(by), ": ", by), rep("*", length(by))))
  }
  own <- of_ancestry(ancestries[[1]])
  if (nrow(own) != 1) {
    refuse(paste0("No single LD Store serves ancestry ", ancestries[[1]], "."))
  }
  # Several LD Stores serve the ancestry, and its declared default broke the tie.
  several <- sum(stores$ready & stores$ancestry %in% ancestries[[1]]) > 1
  list(store = own, choice = if (several) "ancestry_default" else "matched")
}

# The Assigned Ancestry of each Analysis in a data frame of Associations (its `id` column), named by
# Analysis id; `NA` for Unassigned. The Store Releases are the ones its rows came from, when it says.
associations_ancestries <- function(src, data, call = rlang::caller_env()) {
  ids <- unique(data$id)
  if (length(ids) == 0) return(NULL)
  releases <- if (all(c("store_id", "release_id") %in% names(data))) {
    unique(paste0(data$store_id, "/", data$release_id))
  }
  info <- analyses_lookup(src, ids, resolve_releases(src, releases, call), call)
  stats::setNames(as.character(info$assigned_ancestry), info$id)
}
