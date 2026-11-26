#' Create the API Source
#'
#' The API Source answers queries through the OpenGWAS API. Every query function takes a Source as
#' its first argument.
#'
#' Credentials come from the environment: `OPENGWAS_URL` and `OPENGWAS_DEV_KEY`, with
#' `OPENGWAS_DEV_USER` naming the user (default `opengwasr-dev`). This developer access reaches the
#' Data API directly and is for developing opengwasr, not for analyses. The key is never printed.
#'
#' @param url The API's base URL. Defaults to the `opengwasr.url` option, then `OPENGWAS_URL`.
#' @param timeout Seconds to wait for any one call before giving up. Defaults to the
#'   `opengwasr.timeout` option, then 3600.
#' @param max_in_flight How many Jobs one call may run at once. Defaults to the
#'   `opengwasr.max_in_flight` option, then 4; at most 100, the Jobs one user may have queued.
#' @param caller A package or tool calling opengwasr on its users' behalf, to name itself in the
#'   user agent of every request this Source makes, so the API's logs can tell which tool made it:
#'   `og_api(caller = "TwoSampleMR/0.8.0")` sends `opengwasr/<version> TwoSampleMR/0.8.0`. One
#'   short string of printable characters (at most 100), `name/version` by convention; `NULL`
#'   (the default) adds nothing.
#' @param ld_store The Source's LD resource: an LD Store's name, such as `"hgdp1kgp-v2-EUR-k10"`, or
#'   an ancestry code, such as `"EUR"`, for that ancestry's LD Store (an exact name wins). Every verb
#'   that needs LD ([og_tophits()] when it clumps, [og_clump()], [og_ld_matrix()], [og_ld_proxies()],
#'   [og_ld_reflookup()]) uses it: the LD is chosen on the Source, and nowhere else. `NULL` (the
#'   default): each Analysis's Assigned Ancestry chooses its LD Store. It is checked against
#'   [og_ld_stores()] when a call first needs it. See `vignette("ld")`.
#' @param geno_store A Genotype Store, from which LD would be computed when asked. Not available
#'   yet: anything but `NULL` is refused (class `opengwasr_ld_resource_unavailable`) until the API
#'   serves Genotype Stores.
#' @param inherit An API Source to build this one like: every argument given replaces the inherited
#'   one, and the rest (`url` and the key it was made with, `timeout`, `max_in_flight`, `caller`, and
#'   the LD resource) come from `inherit`. Giving `ld_store` or `geno_store` replaces the inherited LD
#'   resource whole. So other LD for one call is `og_api(inherit = src, ld_store = "AFR")`. The
#'   arguments are checked as for any other Source. Anything but an API Source is refused (class
#'   `opengwasr_argument`).
#' @return An `og_api_source`.
#' @examples
#' \dontrun{
#' src <- og_api()
#' src
#' og_api(ld_store = "EUR")
#' og_api(inherit = src, ld_store = "AFR")  # src's URL, key, timeout; AFR LD
#' }
#' @export
og_api <- function(url = getOption("opengwasr.url"),
                   timeout = default_timeout(),
                   max_in_flight = default_max_in_flight(),
                   caller = NULL,
                   ld_store = NULL,
                   geno_store = NULL,
                   inherit = NULL) {
  # What isn't given comes from `inherit`; then everything is checked as for any other Source.
  if (!is.null(inherit)) {
    check_inherit(inherit, "og_api_source", "og_api")
    if (missing(timeout)) timeout <- inherit$timeout
    if (missing(max_in_flight)) max_in_flight <- inherit$max_in_flight
    if (missing(caller)) caller <- inherit$caller
    if (missing(ld_store) && missing(geno_store)) ld_store <- og_ld_resource(inherit)
  }
  # The LD resource first: a refusal needs no credentials.
  if (!is.null(geno_store)) {
    ld_resource_unavailable("A Genotype Store on the API isn't available yet: the API doesn't serve Genotype Stores.",
                            c(i = "Give an LD Store as `ld_store`, or leave both out to match each Analysis's Assigned Ancestry."),
                            resource = "geno_store")
  }
  # Which names and ancestries exist is the API's list to say, when a call needs it.
  if (!is.null(ld_store) && !is_ld_store_value(ld_store)) {
    og_abort(c("`ld_store` must be one LD Store name or ancestry code, such as \"EUR\", or `NULL`.",
               if (is_plink_reference(ld_store)) c(i = "A PLINK Reference is a Local Source's LD resource: `og_local(bfile = )`.")),
             class = "opengwasr_argument")
  }
  # The key the inherited Source was made with, unless another URL is given.
  creds <- if (!is.null(inherit) && missing(url)) {
    c(list(url = inherit$url, scheme = inherit$signed_in_with), inherit$credentials())
  } else {
    api_credentials(url)
  }
  # Kept: it goes into a header, so it has to be one printable line.
  if (!is.null(caller) && (!is.character(caller) || length(caller) != 1 || is.na(caller) ||
                           !grepl("^[[:print:]]{1,100}$", caller) || grepl("[^ -~]", caller))) {
    og_abort("`caller` must be one short string of printable characters, such as \"TwoSampleMR/0.8.0\", or `NULL`.",
             class = "opengwasr_argument")
  }
  if (!is.numeric(timeout) || length(timeout) != 1 || is.na(timeout) || !is.finite(timeout) ||
      timeout <= 0) {
    og_abort("`timeout` must be one positive number of seconds.", class = "opengwasr_argument")
  }
  if (!is.numeric(max_in_flight) || length(max_in_flight) != 1 || is.na(max_in_flight) ||
      !is.finite(max_in_flight) || max_in_flight < 1 || max_in_flight > max_queued_jobs_per_user ||
      max_in_flight != round(max_in_flight)) {
    og_abort("`max_in_flight` must be one whole number from 1 to 100 (the Jobs one user may have queued).", class = "opengwasr_argument")
  }
  structure(
    list(
      url = creds$url,
      timeout = timeout,
      max_in_flight = as.integer(max_in_flight),
      caller = caller,
      # The Source's LD resource (ADR 0006): read by og_ld_resource() (R/ld-stores.R), never elsewhere.
      ld = ld_store,
      signed_in_with = creds$scheme,
      user = creds$user,
      # A closure, so the key is not a visible field: str(), dput() and print() never show it.
      credentials = local({
        key <- creds$key
        function() list(key = key, user = creds$user)
      })
    ),
    class = "og_api_source"
  )
}

#' @export
format.og_api_source <- function(x, ...) {
  c(
    "<og_api_source>",
    paste0("\u2022 URL: ", x$url),
    paste0("\u2022 Signed in with: ", x$signed_in_with, " (as ", x$user, ")"),
    paste0("\u2022 Timeout: ", format(x$timeout), " s"),
    paste0("\u2022 Jobs in flight: at most ", x$max_in_flight),
    if (!is.null(x$caller)) paste0("\u2022 Caller: ", x$caller),
    if (!is.null(og_ld_resource(x))) paste0("\u2022 LD resource: LD Store \"", og_ld_resource(x), "\"")
  )
}

#' @export
print.og_api_source <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

# The defaults of og_api()'s `timeout` and `max_in_flight`: read here, once.
default_timeout <- function() getOption("opengwasr.timeout", 3600)
default_max_in_flight <- function() getOption("opengwasr.max_in_flight", 4)

# `inherit` of a constructor (`fn`, "og_api" or "og_local"): a Source of the same kind (`class`).
# Kept: an R argument's type.
check_inherit <- function(inherit, class, fn, call = rlang::caller_env()) {
  if (inherits(inherit, class)) return(invisible())
  kind <- c(og_api_source = "an API Source, from `og_api()`", og_local_source = "a Local Source, from `og_local()`")
  got <- if (is_source(inherit)) kind[[class(inherit)[[1]]]] else paste0("a ", class(inherit)[[1]])
  og_abort(c(paste0("`inherit` must be ", kind[[class]], ", not ", got, "."),
             i = paste0("`", fn, "(inherit = )` builds a Source like another of the same kind.")),
           class = "opengwasr_argument", arg = "inherit", call = call)
}

# With no Source at all (`src = NULL`).
source_required <- function(call) {
  og_abort(c("This needs a Source, but none was given (`src = NULL`).",
             i = "Pass a Source from `og_api()`, or from `og_local()` for Store Releases or a PLINK Reference on this machine.",
             i = "For LD from a PLINK Reference alone, with no API: `og_local(bfile = \"<prefix>\")`."),
           class = "opengwasr_source_required", call = call)
}

#' Every query function takes a Source. `src = NULL` (and anything else that isn't one) gets a
#' classed error instead of R's "no applicable method".
#'
#' @param src Whatever the caller passed.
#' @param ... Ignored.
#' @exportS3Method og_analyses default
#' @exportS3Method og_associations default
#' @exportS3Method og_phewas default
#' @exportS3Method og_range default
#' @exportS3Method og_tophits default
#' @exportS3Method og_variants_rsid default
#' @exportS3Method og_variants_chrpos default
#' @exportS3Method og_ld_stores default
#' @exportS3Method og_ld_proxies default
#' @exportS3Method og_ld_reflookup default
#' @exportS3Method og_clump default
#' @exportS3Method og_ld_matrix default
#' @exportS3Method og_status default
#' @exportS3Method og_user default
#' @noRd
source_not_given <- function(src, ...) check_source(src, sys.call(sys.parent()))

# Kept: an R argument's type; without a Source there is no API to ask. The one Source check, of
# every query function (through its default method) and every Job function.
check_source <- function(src, call = rlang::caller_env()) {
  if (inherits(src, "og_api_source")) return(invisible())
  if (is.null(src)) source_required(call)
  # A query with a Local Source method never gets here; one without refuses it.
  if (inherits(src, "og_local_source")) {
    local_unsupported("This isn't available for a Local Source.", "Use the API Source, `og_api()`, for it.", call = call)
  }
  og_abort(c(paste0("`src` must be a Source from `og_api()` or `og_local()`, not ", class(src)[[1]], "."),
             i = "Create one with `og_api()`, or with `og_local()` for Store Releases on this machine."),
           class = "opengwasr_argument", call = call)
}
og_analyses.default <- og_associations.default <- og_phewas.default <- og_range.default <-
  og_tophits.default <- og_variants_rsid.default <- og_variants_chrpos.default <- og_ld_stores.default <-
  og_ld_proxies.default <- og_ld_reflookup.default <- og_clump.default <- og_ld_matrix.default <-
  og_status.default <- og_user.default <- source_not_given
