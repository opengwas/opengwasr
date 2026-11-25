# LD Stores: what the API serves, and the one owner of choosing the LD a call uses.
#
# An LD Store holds the LD of one reference panel of one ancestry. Every LD operation (the matrix and
# proxies, clumping in R/clump.R) names exactly one LD resource, and gets it from `choose_ld_store()`,
# which takes it from `ld_resource()`: the Source's LD resource, the only place LD is chosen (ADR 0006
# and its clarification of 25 November 2026). Never write a second chooser.

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

#' The LD resource a Source carries
#'
#' `og_ld_resource()` returns the LD resource a Source carries (ADR 0006; see `vignette("ld")`,
#' "The Source's LD resource"): the one every verb that needs LD uses. Packages built on opengwasr,
#' such as TwoSampleMR, read it to honour the Source's LD resource.
#'
#' @param src A Source, from [og_api()] or [og_local()].
#' @return For an API Source, its `ld_store` (an LD Store's name or an ancestry code), as given to
#'   `og_api(ld_store = )`. For a Local Source, its PLINK Reference, from `og_local(bfile = , plink =
#'   )`: an `og_plink_reference`, a list of `path` (absolute, with forward slashes on every platform),
#'   `plink_bin` (the binary's absolute path) and `plink_version` (as PLINK reports it, such as
#'   `"1.90b6.21"` or `"2.0.0-b.1.4"`). `NULL` when the Source carries none:
#'   the API Source then matches each Analysis's Assigned Ancestry, and a Local Source refuses
#'   anything that needs LD. Anything that isn't a Source is refused as every query function refuses
#'   it (class `opengwasr_source_required` for `NULL`, else `opengwasr_argument`).
#' @examples
#' \dontrun{
#' og_ld_resource(og_api())                    # NULL
#' og_ld_resource(og_api(ld_store = "EUR"))    # "EUR"
#' }
#' @export
og_ld_resource <- function(src) {
  if (!is_source(src)) check_source(src)
  src$ld
}

is_source <- function(x) inherits(x, c("og_api_source", "og_local_source"))

#' Which LD resource answers this call (ADR 0006), and where it came from: the Source's own
#' (`og_api(ld_store = )`, `og_local(bfile = )`; `from = "source"`), else none (`"ancestry"`), which on
#' the API Source means each Analysis's Assigned Ancestry chooses its LD Store (choose_ld_store()).
#' A Local Source with none is refused: it never reads the API's LD. The LD engine, clumping and
#' choose_ld_store() call it. `clumping`: the refusal also offers `clump = FALSE`.
#' @return `list(ld, from)`.
#' @noRd
ld_resource <- function(src, call = rlang::caller_env(), clumping = FALSE) {
  own <- og_ld_resource(src)
  from <- if (!is.null(own)) "source" else "ancestry"
  if (from == "ancestry" && inherits(src, "og_local_source")) {
    og_abort(c(if (clumping) {
                 paste0("A Local Source with no LD resource can't clump: Store Releases have no stored Clumps yet, ",
                        "so they would be computed, which needs LD.")
               } else {
                 "This needs LD, and this Local Source has no LD resource."
               },
               local_ld_forms,
               i = "Downloaded LD Stores (`ld_store =`) and Genotype Stores (`geno_store =`) can't be read yet.",
               if (clumping) c(i = "Or pass `clump = FALSE` for every top hit, unclumped.")),
             class = "opengwasr_ld_resource_required", refusal = TRUE,
             forms = c("bfile", "api_two_step"), clump_false = clumping, call = call)
  }
  list(ld = own, from = from)
}

# The LD a Local Source can use, as a refusal names them (`forms`: "bfile", "api_two_step";
# vignette("local-source"), "Clumping and LD").
local_ld_forms <- c(
  i = paste0("Give the Source a PLINK Reference: `og_local(inherit = src, bfile = \"<prefix>\")` keeps its Store Releases, ",
             "and `plink =` names the PLINK binary if it isn't found."),
  i = "Or, for the API's LD Stores, clump in two steps on the API Source: `og_clump(og_api(), og_tophits(src, ids, clump = FALSE))`."
)

# An LD resource whose kind opengwasr can't use yet (ADR 0006): refused, saying so, rather than left
# out of the interface. A Local Source's are also `opengwasr_local_unsupported`. `resource`: the kind
# ("ld_store", "geno_store"); `forms`: what the advice names, if it names them.
ld_resource_unavailable <- function(message, advice, resource, local = FALSE, forms = NULL, call = rlang::caller_env()) {
  og_abort(c(message, advice),
           class = c("opengwasr_ld_resource_unavailable", if (local) "opengwasr_local_unsupported"),
           refusal = TRUE, resource = resource, forms = forms, call = call)
}

# Kept: an R argument's type. One LD Store name or ancestry code, as `og_api(ld_store = )` takes it;
# which exist is the API's list to say.
is_ld_store_value <- function(x) is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)

#' Choose the one LD Store an LD operation uses, from the Source's LD resource (ld_resource()).
#'
#' @param src The Source. Its LD resource is a PLINK Reference (`choice = "user"`; `store` is then
#'   `list(ld_store = <its path>)` and `plink` the reference), an LD Store's name (`"user"`), an
#'   ancestry code (that ancestry's LD Store: `"ancestry_default"`), or `NULL` (matched from
#'   `ancestries`: `"matched"`, or `"ancestry_default"` when several LD Stores serve that ancestry and
#'   its declared default chose).
#' @param ancestries `NULL` when the operation has no Analyses behind its variants, else a character
#'   vector of the Assigned Ancestry of each Analysis (`NA` for Unassigned), named by Analysis id.
#'   Only read when the Source has no LD resource.
#' @return `list(store, choice, from)` (and `plink` for a PLINK Reference): `store` is the LD Store's
#'   row of [og_ld_stores()] (one-row data frame), `choice` is how it was chosen, `from` where the LD
#'   came from (ld_resource()).
#'
#' Refuses with `opengwasr_ld_unmatched` (a refusal, with the LD Stores in `e$ld_stores`) when the
#' Source has no LD resource and no single ancestry follows from `ancestries`: none given, an
#' Unassigned Analysis, or several ancestries. Kept: the API would answer a request that names no
#' LD Store from European LD.
#' @noRd
choose_ld_store <- function(src, ancestries = NULL, call = rlang::caller_env()) {
  resource <- ld_resource(src, call)
  chosen <- choose_from(src, resource$ld, ancestries, call)
  chosen$from <- resource$from  # where the LD came from: "source" or "ancestry"
  chosen
}

# choose_ld_store() once the LD resource is known (`ld`: a PLINK Reference, an LD Store name or
# ancestry code, or NULL to match `ancestries`). A Local Source's is always a PLINK Reference
# (ld_resource() refused one with none), so everything after the first branch is the API Source's.
choose_from <- function(src, ld, ancestries, call) {
  # A PLINK Reference is the user's own choice, read locally: no LD Store row, and no API call.
  # `store$ld_store` is its path, which is what every result records as the LD used.
  if (is_plink_reference(ld)) {
    return(list(store = list(ld_store = ld$path), choice = "user", plink = ld))
  }
  stores <- ld_stores(src, call)
  choices <- function() paste0("\"", unique(stores$ancestry), "\"", collapse = ", ")
  refuse <- function(what, ...) {
    og_abort(c(what, ..., i = paste0("Give the Source an LD Store, such as `og_api(inherit = src, ld_store = \"EUR\")`, from: ",
                                     choices(), " (see `og_ld_stores()`).")),
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
    # Kept: the API takes a store or an ancestry, and only the list says which it names; and the
    # LD Store's row (`far_bp`, `t_far`) is needed here for filling and provenance anyway.
    if (ld %in% stores$ld_store) {
      named <- stores[stores$ld_store == ld, , drop = FALSE]
      if (!named$ready) not_ready(ld)
      return(list(store = named, choice = "user"))
    }
    own <- of_ancestry(ld)
    if (nrow(own) == 0) {
      og_abort(c(paste0("The Source's `ld_store` = \"", ld, "\" is neither an LD Store's name nor the ancestry of one."),
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
    unique(release_names(data$store_id, data$release_id))
  }
  info <- analyses_lookup(src, ids, resolve_releases(src, releases, call), call)
  stats::setNames(as.character(info$assigned_ancestry), info$id)
}
