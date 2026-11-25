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
#' @param ld_store The Source's LD resource: an LD Store's name, or an ancestry code for that
#'   ancestry's LD Store, as `ld` takes them. Every verb that needs LD ([og_tophits()] when it
#'   clumps, [og_clump()], [og_ld_matrix()], [og_ld_proxies()], [og_ld_reflookup()]) uses it, and
#'   their `ld` overrides it for one call. `NULL` (the default): each Analysis's Assigned Ancestry
#'   chooses its LD Store. It is checked against [og_ld_stores()] when a call first needs it. See
#'   `vignette("ld")`.
#' @param geno_store A Genotype Store, from which LD would be computed when asked. Not available
#'   yet: anything but `NULL` is refused (class `opengwasr_ld_resource_unavailable`) until the API
#'   serves Genotype Stores.
#' @return An `og_api_source`.
#' @examples
#' \dontrun{
#' src <- og_api()
#' src
#' og_api(ld_store = "EUR")
#' }
#' @export
og_api <- function(url = getOption("opengwasr.url"),
                   timeout = default_timeout(),
                   max_in_flight = default_max_in_flight(),
                   caller = NULL,
                   ld_store = NULL,
                   geno_store = NULL) {
  # The LD resource first: a refusal needs no credentials.
  if (!is.null(geno_store)) {
    ld_resource_unavailable("A Genotype Store on the API isn't available yet: the API doesn't serve Genotype Stores.",
                            c(i = "Give an LD Store as `ld_store`, or leave both out to match each Analysis's Assigned Ancestry."),
                            resource = "geno_store")
  }
  # Which names and ancestries exist is the API's list to say, when a call needs it.
  if (!is.null(ld_store) && !is_ld_store_value(ld_store)) {
    og_abort(c("`ld_store` must be one LD Store name or ancestry code, such as \"EUR\", or `NULL`.",
               if (is_plink_reference(ld_store)) c(i = "A PLINK Reference is given to one call, as `ld`, or to a Local Source, `og_local(gwas, bfile = )`.")),
             class = "opengwasr_argument")
  }
  creds <- api_credentials(url)
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

# The defaults of og_api()'s `timeout` and `max_in_flight`: read here, once, for the Source and for
# no_source() alike.
default_timeout <- function() getOption("opengwasr.timeout", 3600)
default_max_in_flight <- function() getOption("opengwasr.max_in_flight", 4)

# What a query runs on when the user gave `src = NULL`: a PLINK Reference needs no API when nothing
# is looked up (ALID -> rsID is the one lookup it can need). It has the Source's timeout and
# concurrency, and no URL or credentials; the first request any code path makes (api_response() is
# where every request goes) stops with `opengwasr_source_required`, so nothing is looked up without
# a Source.
no_source <- function() {
  structure(list(url = NULL, timeout = default_timeout(), max_in_flight = as.integer(default_max_in_flight())),
            class = "og_no_source")
}

# `what`: what needed the API, when the caller knows ("3 ALIDs have no rsID: ...").
source_required <- function(call, what = NULL) {
  og_abort(c("This needs the OpenGWAS API, but no Source was given (`src = NULL`).",
             if (!is.null(what)) c(x = what),
             i = "Pass a Source from `og_api()`.",
             i = paste0("`src = NULL` is for a PLINK Reference when nothing is looked up: ",
                        "variants given as rsIDs, or as a data frame with an `rsid` column.")),
           class = "opengwasr_source_required", call = call)
}

#' Every query function takes a Source. `src = NULL` (and anything else that isn't one) gets a
#' classed error instead of R's "no applicable method"; `og_clump()` and `og_ld_matrix()` have their
#' own `NULL` methods, for a PLINK Reference.
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
    og_abort(c("This isn't available for a Local Source.",
               i = "Use the API Source, `og_api()`, for it."),
             class = "opengwasr_local_unsupported", refusal = TRUE, call = call)
  }
  og_abort(c(paste0("`src` must be a Source from `og_api()` or `og_local()`, not ", class(src)[[1]], "."),
             i = "Create one with `og_api()`, or with `og_local()` for Store Releases on this machine."),
           class = "opengwasr_argument", call = call)
}
og_analyses.default <- og_associations.default <- og_phewas.default <- og_range.default <-
  og_tophits.default <- og_variants_rsid.default <- og_variants_chrpos.default <- og_ld_stores.default <-
  og_ld_proxies.default <- og_ld_reflookup.default <- og_clump.default <- og_ld_matrix.default <-
  og_status.default <- og_user.default <- source_not_given
