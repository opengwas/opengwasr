# The Local Source: Store Releases on this machine, read through ogquery (the Store Reader) by way
# of reticulate (ADR 0003, clarification of 25 November 2026).
#
# reticulate is in Suggests and loads only here, on the first local call: nothing an API Source
# does reaches this file. Every call into reticulate, and every touch of a Python object, goes
# through the small `local_*` functions under "The Python boundary" at the end. They are
# primitives (has Python started, reticulate's config, py_require(), import ogquery, open a
# release, probe it, load a manifest); the rules built on them (which Python the user chose,
# whether reticulate swapped it, what a Store Release's problem is) are plain R above them, which
# the tests drive with the boundary mocked.

#' Create a Local Source
#'
#' A Local Source answers queries from Store Releases downloaded to this machine, through
#' ogquery, the Store Reader. Give it the directory of each GWAS Store, one element per GWAS Store.
#' A GWAS Store directory holds one Store Release, one level down: the directory there holding a
#' `manifest.json`. It is searched one level down and never deeper, so the Dense Component inside a
#' Hybrid Store Release (`<release>/dense/manifest.json`) is never taken for a Store Release, and
#' hidden directories (whose names start with `.`), such as a build in progress, are skipped. A
#' Store Release's own directory (the one holding its `manifest.json`) works too. One that has lost
#' its `manifest.json` (it still holds `data.zarr` or `analyses.tsv`) is refused, never searched
#' as a GWAS Store directory. Each Store
#' Release names itself by its manifest (`store_id`, `release_id`, `format_version`), and a Local
#' Source holds at most one Store Release of each GWAS Store, so no query ever chooses between
#' Store Releases.
#'
#' `og_local()` reads no Associations. It checks that each path is, or holds, a Store Release that
#' the installed ogquery can read, and refuses the Source otherwise:
#'
#' * a path that doesn't exist, or holds no Store Release (no manifest in it or one level down, or
#'   one ogquery can't parse): `opengwasr_not_store_release`, with `e$paths` and why each was
#'   refused, `e$reasons`;
#' * a GWAS Store directory holding several Store Releases: `opengwasr_several_releases`, with one
#'   element per Store Release found in `e$directory` (the GWAS Store directory given),
#'   `e$store_id`, `e$release_id`, `e$releases` (`<store_id>/<release_id>`) and `e$paths`, so you
#'   can give the one you want;
#' * a Store Release whose format version ogquery can't read: `opengwasr_format_unsupported`,
#'   with `e$paths`, each one's version (`e$format_version`) and the format series ogquery reads
#'   (`e$readable`);
#' * two Store Releases of one GWAS Store: `opengwasr_duplicate_store`.
#'
#' @section Python:
#' The first local call finds Python and ogquery. A Python you chose always wins, in the order
#' reticulate ranks them: `RETICULATE_PYTHON` (a Python, or its directory), `RETICULATE_PYTHON_ENV`,
#' an environment you selected with `reticulate::use_virtualenv()`, `reticulate::use_condaenv()` or
#' `reticulate::use_python()`, or an activated virtualenv (`VIRTUAL_ENV`); or a Python reticulate
#' has already started in this session. If it lacks ogquery, the error says how to install it
#' there. opengwasr never swaps it for another: if reticulate can't start the Python you chose and
#' starts a lower choice, its automatic one or one it found itself instead, `og_local()` refuses.
#' Otherwise (and with `RETICULATE_PYTHON=managed` or `RETICULATE_USE_MANAGED_VENV=true`, which ask
#' for it) reticulate sets one up automatically with uv
#' (`reticulate::py_require("ogquery>=0.1.0,<1")`), which needs the internet the first time.
#'
#' opengwasr reads ogquery `>=0.1.0,<1`. An ogquery outside that range, or without
#' `ogquery.release` (such as the placeholder 0.0.0 on PyPI), is refused before anything is read.
#'
#' One gap: a `use_virtualenv()`, `use_condaenv()` or `use_python()` call can't be seen before
#' Python starts, so opengwasr declares ogquery with `py_require()` as for no choice. Once Python
#' has started, reticulate's `py_config()` names the call (`forced`), and opengwasr reports it
#' (`chosen_by = "use_python"`). But if reticulate can't start that environment and starts its
#' automatic one, `py_config()` names only the automatic one, and opengwasr can't tell. Set
#' `RETICULATE_PYTHON` to close the gap.
#'
#' Without reticulate, `og_local()` offers to install it, or, when R isn't interactive, stops
#' with `opengwasr_local_unavailable`, as it does whenever no Python with ogquery can be found.
#' That error carries `e$need` (`"reticulate"`, `"python"`, `"ogquery"`, `"ogquery_version"` or
#' `"chosen_python"`), `e$found` (for `"ogquery_version"`, the ogquery version found, or `NA` for
#' none), `e$required` (the ogquery versions opengwasr supports, `">=0.1.0,<1"`),
#' `e$python`, `e$chosen_by` (how the Python was chosen: `"RETICULATE_PYTHON"`,
#' `"RETICULATE_PYTHON_ENV"`, `"use_python"`, `"VIRTUAL_ENV"`, `"automatic"` or `"reticulate"`, one
#' reticulate found itself), `e$started_by` (for `"chosen_python"`, how the Python reticulate
#' started instead was chosen), `e$install` (the command that installs what is missing, when there
#' is one; for an unsupported ogquery in reticulate's automatic Python, the command that clears
#' uv's cached ogquery, after which restarting R sets up a supported one) and `e$setting` (the
#' setting that names a Python). [og_local_check()] reports what
#' was found.
#'
#' @section LD resource:
#' A Local Source carries the LD resource that answers LD for it (ADR 0006; see
#' `vignette("local-source")`, "Clumping and LD"), and the LD is chosen there and nowhere else. Today
#' that is a **PLINK Reference**: `bfile` names a PLINK binary fileset (`.bed`, `.bim`, `.fam`) on
#' your machine, and `plink` the PLINK binary that reads it. `og_local()` checks the fileset (class
#' `opengwasr_plink_reference_missing`, with `e$files`) and finds PLINK. [og_tophits()] then clumps on
#' the fly with it (`r2` and `kb` are required: a PLINK Reference has no recorded defaults, class
#' `opengwasr_plink_settings_required`), and [og_clump()] and [og_ld_matrix()] read it. Without an LD
#' resource, anything that needs LD is refused (class `opengwasr_ld_resource_required`), naming the
#' LD resources in `e$forms` (`"bfile"` and `"api_two_step"`: the API's LD Stores, by clumping on an
#' API Source you name, `og_clump(og_api(), og_tophits(src, ids, clump = FALSE))`) and, when
#' clumping, `clump = FALSE` (`e$clump_false`): a Local Source never calls the API itself.
#'
#' Variants are matched to a PLINK Reference by rsID, because PLINK References are usually on GRCh37
#' while ALIDs are on GRCh38: from a data frame's `rsid` column, else looked up in the Source's Store
#' Releases. A variant with no rsID, or whose rsID isn't in the `.bim`, is left out, and a message
#' (class `opengwasr_plink_unmatched`, with `count`) says how many. PLINK 1.9 and PLINK 2
#' (2.0.0-a.6.0 or later) both work, and give the same answer: each is asked for the same thing in its
#' own words (an LD matrix of r with the reference's allele order kept: `--r square
#' --keep-allele-order` in 1.9, `--r-unphased square ref-based` in 2; clumping with `--clump`, read
#' from `.clumped` or `.clumps`). Its version is read once, here (`--version`), and kept on the PLINK
#' Reference ([og_ld_resource()]) and in the provenance of what it computes (`plink_version`, beside
#' `plink_bin`). A `plink` that is any other PLINK, such as 1.07 or an earlier PLINK 2, or a program
#' that isn't PLINK, is refused (class `opengwasr_plink_unsupported`, with `version`, the version
#' found or `NA`, `supported`, and `suggest`, a supported `"plink"` or `"plink2"` on your `PATH`, or
#' `NULL`).
#'
#' **An LD-only Local Source.** `og_local(bfile = )` with no `gwas` holds no Store Releases and
#' needs no Python. It answers [og_clump()] and [og_ld_matrix()] for data you already have, such as
#' top hits from the API Source, when PLINK can match the variants by rsID: a data frame with an
#' `rsid` column, or rsIDs. ALIDs would need a GWAS Store to look up their rsIDs, so they are refused
#' (class `opengwasr_gwas_store_required`, with the ALIDs in `e$alids`), naming `og_local(gwas = ,
#' bfile = )`. Every verb that reads Associations or Analyses refuses it with the same class.
#'
#' `ld_store` (a downloaded LD Store) and `geno_store` (a Genotype Store) are refused until
#' opengwasr can read them (classes `opengwasr_ld_resource_unavailable` and
#' `opengwasr_local_unsupported`, with `e$resource`). Giving more than one kind of LD resource is
#' refused (class `opengwasr_ld_resource_mixed`, with `e$given`), and so is `plink` without `bfile`.
#' An error about `bfile` or `plink` names it in `e$arg`.
#'
#' @section Inheriting:
#' `og_local(inherit = src, ...)` builds a Local Source like `src`: every argument given replaces the
#' inherited one, and the rest come from `src`. Without `gwas`, its Store Releases are `src`'s, not
#' opened again. Giving any of `plink`, `bfile`, `ld_store` and `geno_store` replaces `src`'s LD
#' resource whole, never mixed with it: `og_local(inherit = src, bfile = ref)` gives `src`'s Store
#' Releases a PLINK Reference, and `og_local(inherit = src, bfile = NULL)` drops it. What is given is
#' checked as for any other Local Source. Anything but a Local Source as `inherit` is refused (class
#' `opengwasr_argument`).
#'
#' @param gwas The directory of each GWAS Store, or of each Store Release: a character vector,
#'   one element per GWAS Store. `NULL` (the default) for none: an LD-only Local Source, which needs
#'   `bfile`. A Local Source with neither Store Releases nor an LD resource is refused (class
#'   `opengwasr_argument`).
#' @param plink The PLINK 1.9 or 2 binary, for `bfile`: its path, or a command on your `PATH`, such
#'   as `"plink2"` (as in a shell, a name without a path separator is a command). `NULL` takes the
#'   first supported PLINK of the genetics.binaRies package, if it is installed, then `plink`, then
#'   `plink2`, on your `PATH`, skipping any that isn't supported; if there is none, it stops (class
#'   `opengwasr_plink_not_found`, with `skipped`: `plink_bin` and `version` of each one skipped) and
#'   says how to install it.
#' @param bfile A PLINK binary fileset's prefix (without `.bed`, `.bim` or `.fam`): the Source's LD
#'   resource, a PLINK Reference.
#' @param ld_store A downloaded LD Store. Not available yet.
#' @param geno_store A downloaded Genotype Store. Not available yet.
#' @param inherit A Local Source to build this one like, as described under "Inheriting".
#' @return An `og_local_source`. Its `releases` is a data frame with one row per Store Release:
#'   `store_id`, `release_id`, `format_version` and `path` (the Store Release's own directory), with no
#'   rows for an LD-only Local Source. Its LD resource is read with [og_ld_resource()]: an
#'   `og_plink_reference`, or `NULL`.
#' @seealso [og_local_check()], [og_api()].
#' @examplesIf nzchar(Sys.getenv("RETICULATE_PYTHON")) && rlang::is_installed("reticulate")
#' stores <- system.file("stores", package = "opengwasr")
#' # GWAS Store directories, and, for hyb, which holds two, a Store Release directory
#' src <- og_local(c(file.path(stores, c("dense", "ragged")), file.path(stores, "hyb", "v1")))
#' src
#' # With a PLINK Reference as its LD resource, where PLINK is installed:
#' bfile <- file.path(system.file("plink", package = "opengwasr"), "ref")
#' ld_only <- tryCatch(og_local(bfile = bfile), error = function(e) NULL)
#' if (!is.null(ld_only)) {
#'   og_ld_matrix(ld_only, c("rs5000", "rs1000"))
#'   src <- og_local(inherit = src, bfile = bfile)
#'   og_tophits(src, "s:bmi_irn", r2 = 0.001, kb = 10000)
#' }
#' @export
og_local <- function(gwas = NULL, plink = NULL, bfile = NULL, ld_store = NULL, geno_store = NULL,
                     inherit = NULL) {
  if (!is.null(inherit)) check_inherit(inherit, "og_local_source", "og_local")
  # The LD resource first: it needs no Python, so a refusal costs nothing. Inherited unless any LD
  # argument is given, which replaces it whole.
  ld_given <- !missing(plink) || !missing(bfile) || !missing(ld_store) || !missing(geno_store)
  ld <- if (!is.null(inherit) && !ld_given) og_ld_resource(inherit) else local_ld_resource(plink, bfile, ld_store, geno_store)
  if (!is.null(inherit) && missing(gwas)) {
    # The inherited Store Releases, as opened: nothing is read again.
    return(new_local_source(inherit$releases, inherit$handles, ld))
  }
  if (is.null(gwas)) {
    # An LD-only Local Source: no Store Releases, and no Python.
    rows <- data.frame(store_id = character(), release_id = character(), format_version = character(),
                       path = character(), stringsAsFactors = FALSE)
    return(new_local_source(rows, new.env(parent = emptyenv()), ld))
  }
  gwas <- check_release_paths(gwas, arg = "gwas")
  ogquery <- local_ogquery()
  found <- lapply(gwas, find_release, ogquery = ogquery)
  refuse_release_problems(found, ogquery$info)
  rows <- release_rows(found)
  # Every path now names one Store Release, which the Source keeps by its own directory.
  rows$path <- rows$release_path
  duplicate <- duplicate_stores(rows)
  if (length(duplicate)) refuse_duplicate_store(rows, duplicate[[1]])

  handles <- new.env(parent = emptyenv())
  for (i in seq_along(found)) assign(rows$path[[i]], found[[i]]$release, envir = handles)
  new_local_source(rows[c("store_id", "release_id", "format_version", "path")], handles, ld)
}

# The Local Source: its Store Releases (`releases`, opened in `handles`) and its LD resource. One with
# neither would answer nothing, and is refused.
new_local_source <- function(releases, handles, ld, call = rlang::caller_env()) {
  if (nrow(releases) == 0 && is.null(ld)) {
    og_abort(c("A Local Source needs Store Releases (`gwas`), a PLINK Reference (`bfile`), or both.",
               i = "For example `og_local(\"/data/opengwas/OGS-00011\")`, or `og_local(bfile = \"/data/ld/1kg/EUR\")` for LD only."),
             class = "opengwasr_argument", arg = "gwas", call = call)
  }
  structure(
    list(releases = releases, handles = handles,
         # A local query waits for nothing: the deadline a call sets (local_query()) is never reached.
         timeout = Inf,
         # The Source's LD resource (ADR 0006): read by og_ld_resource() (R/ld-stores.R), never elsewhere.
         ld = ld),
    class = "og_local_source"
  )
}

#' @export
format.og_local_source <- function(x, ...) {
  r <- x$releases
  c(
    "<og_local_source>",
    paste0("\u2022 Store Releases: ", if (nrow(r)) nrow(r) else "none (LD only)"),
    if (nrow(r)) paste0("\u2022 ", release_names(r$store_id, r$release_id), ": ", r$path),
    if (is.null(ref <- og_ld_resource(x))) {
      "\u2022 LD resource: none"
    } else {
      c(paste0("\u2022 LD resource: PLINK Reference ", ref$path), plink_binary_line(ref))
    }
  )
}

# The LD resource of `og_local()` (ADR 0006): a PLINK Reference from `bfile` (and `plink`), built by
# plink_reference(), which checks both and names them as `og_local()` does; the kinds that can't be read yet are refused,
# as is more than one kind (`given`: "plink", "ld_store", "geno_store").
local_ld_resource <- function(plink, bfile, ld_store, geno_store, call = rlang::caller_env()) {
  given <- c(plink = !is.null(plink) || !is.null(bfile), ld_store = !is.null(ld_store), geno_store = !is.null(geno_store))
  if (sum(given) > 1) {
    named <- c(plink = "`plink`/`bfile` (a PLINK Reference)", ld_store = "`ld_store` (an LD Store)",
               geno_store = "`geno_store` (a Genotype Store)")[given]
    og_abort(c(paste0("A Local Source carries one LD resource, but ", paste(named, collapse = " and "), " were given."),
               i = "Give one of them: a Source carries one LD resource, and other LD is another Source."),
             class = c("opengwasr_ld_resource_mixed", "opengwasr_argument"), given = names(given)[given], call = call)
  }
  instead <- c(i = "Give a PLINK Reference instead: `og_local(gwas, bfile = \"<prefix>\")`, or `og_local(inherit = src, bfile = \"<prefix>\")`.")
  if (given[["ld_store"]]) {
    ld_resource_unavailable("Reading a downloaded LD Store isn't available yet.", instead,
                            resource = "ld_store", local = TRUE, call = call)
  }
  if (given[["geno_store"]]) {
    ld_resource_unavailable("Reading a downloaded Genotype Store isn't available yet.", instead,
                            resource = "geno_store", local = TRUE, call = call)
  }
  if (is.null(bfile)) {
    if (!is.null(plink)) {
      og_abort(c("`plink` is the PLINK binary that reads `bfile`, but no `bfile` was given.",
                 i = "Give the PLINK fileset's prefix as `bfile`."),
               class = "opengwasr_argument", arg = "bfile", call = call)
    }
    return(NULL)
  }
  plink_reference(bfile, plink, call = call)
}

# print() for the Local Source's objects: their format(), one line each.
print_formatted <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @export
print.og_local_source <- print_formatted

#' Check the setup of a Local Source
#'
#' Reports the Python a Local Source uses and how it was chosen, the reticulate and ogquery
#' versions, the format series ogquery reads, and, for each path, the Store Release found there
#' (for a GWAS Store directory, the one it resolved to), its `store_id`, `release_id` and
#' `format_version`, and whether ogquery can read it. It reports problems instead
#' of raising them, so it is the place to start when [og_local()] refuses: only a `path` that
#' isn't a character vector of paths, or a Local Source, is an error.
#'
#' Like [og_local()], it finds Python on first use, so it sets Python up automatically when you
#' haven't chosen one.
#'
#' @param path The directory of each GWAS Store or Store Release, as for [og_local()], or a Local
#'   Source.
#' @return An `og_local_check`: a list with
#'   * `python` (the Python's path) and `chosen_by` (how it was chosen, as for [og_local()]'s
#'     errors: `"RETICULATE_PYTHON"`, `"use_python"`, `"automatic"`, ...);
#'   * `reticulate` and `ogquery`, their versions (`ogquery` is the one found, even when opengwasr
#'     doesn't support it);
#'   * `ogquery_required`, the ogquery versions opengwasr supports (`">=0.1.0,<1"`), and
#'     `ogquery_in_range`, whether the ogquery found is one of them (`NA` when none is found);
#'   * `reads`, the format series ogquery reads, such as `"0.3"` (any 0.3.x);
#'   * `releases`, a data frame with one row per path: `path` (as given), `release_path` (the
#'     Store Release directory it resolved to: itself for a Store Release directory, the one a GWAS
#'     Store directory holds, or `NA` when it holds none or several, or the path doesn't exist or
#'     isn't a directory), `store_id`, `release_id`,
#'     `format_version`, `readable`, `problem` (`NA`, `"not_release"`, `"several"`, `"format"` or
#'     `"unavailable"`) and `reason`;
#'   * `duplicates`, the GWAS Stores given more than one Store Release;
#'   * `unavailable`, the `opengwasr_local_unavailable` condition [og_local()] would raise, or
#'     `NULL`;
#'   * `problems`, all of that as sentences, for printing.
#'
#'   Missing values are `NA`.
#' @examplesIf nzchar(Sys.getenv("RETICULATE_PYTHON")) && rlang::is_installed("reticulate")
#' stores <- system.file("stores", package = "opengwasr")
#' og_local_check(file.path(stores, c("dense", "hyb")))
#' @export
og_local_check <- function(path) {
  if (inherits(path, "og_local_source")) {
    path <- path$releases$path
  } else {
    path <- check_release_paths(path, exist = FALSE, arg = "path")
  }
  ogquery <- tryCatch(local_ogquery(), opengwasr_local_unavailable = identity)
  if (inherits(ogquery, "error")) {
    unavailable <- ogquery
    # Which Store Release each path names needs no Python: only its manifest does.
    release_path <- vapply(path, function(p) release_dir(p)$release_path, character(1), USE.NAMES = FALSE)
    rows <- data.frame(path = path, release_path = release_path, store_id = NA_character_,
                       release_id = NA_character_, format_version = NA_character_,
                       problem = problem$unavailable,
                       reason = unavailable_reason[[unavailable$need]], stringsAsFactors = FALSE)
    info <- list(python = unavailable$python, chosen_by = unavailable$chosen_by,
                 ogquery = unavailable$found, reads = NA_character_)
    problems <- conditionMessage(unavailable)
  } else {
    unavailable <- NULL
    rows <- release_rows(lapply(path, find_release, ogquery = ogquery))
    info <- ogquery$info
    problems <- character()
  }
  rows$readable <- is.na(rows$problem)
  duplicates <- duplicate_stores(rows)
  problems <- c(problems, vapply(duplicates, function(store) {
    paste0("GWAS Store ", store, " has ", sum(rows$store_id %in% store), " Store Releases here (",
           paste0(rows$release_id[rows$store_id %in% store], collapse = ", "),
           "); a Local Source holds one of each GWAS Store.")
  }, character(1)))
  structure(
    list(python = info$python, chosen_by = info$chosen_by, reticulate = local_reticulate_version(),
         ogquery = info$ogquery, ogquery_required = ogquery_required,
         ogquery_in_range = if (is.na(info$ogquery) && !identical(unavailable$need, "ogquery_version")) NA else ogquery_in_range(info$ogquery),
         reads = info$reads,
         releases = rows[c("path", "release_path", "store_id", "release_id", "format_version", "readable",
                           "problem", "reason")],
         duplicates = duplicates, unavailable = unavailable, problems = unname(problems)),
    class = "og_local_check"
  )
}

#' @export
format.og_local_check <- function(x, ...) {
  r <- x$releases
  # Each Store Release on two lines, its path and then what was found there: paths are long.
  found <- vapply(seq_len(nrow(r)), function(i) {
    name <- if (!is.na(r$store_id[[i]])) release_names(r$store_id[[i]], r$release_id[[i]])
    fmt <- if (!is.na(r$format_version[[i]])) paste0("format ", r$format_version[[i]])
    says <- if (r$readable[[i]]) "readable" else paste0("not readable: ", r$reason[[i]])
    label <- paste(c(name, fmt), collapse = ", ")
    if (nzchar(label)) paste0(label, ": ", says) else says
  }, character(1))
  # A GWAS Store directory's line is followed by the Store Release it resolved to.
  resolved <- ifelse(!is.na(r$release_path) & r$release_path != r$path,
                     paste0("    resolved to ", r$release_path), NA_character_)
  lines <- unlist(lapply(seq_len(nrow(r)), function(i) {
    c(paste0("  ", r$path[[i]]), if (!is.na(resolved[[i]])) resolved[[i]], paste0("    ", found[[i]]))
  }))
  ogquery <- if (isFALSE(x$ogquery_in_range)) {
    paste0(na_as(x$ogquery, "no version"), ", not supported (opengwasr needs ogquery ", x$ogquery_required, ")")
  } else {
    na_as(x$ogquery, "not found")
  }
  c(
    "<og_local_check>",
    paste0("Python:     ", na_as(x$python, "none found"), if (!is.na(x$chosen_by)) paste0(" (chosen by ", x$chosen_by, ")")),
    paste0("reticulate: ", na_as(x$reticulate, "not installed")),
    paste0("ogquery:    ", ogquery, if (!all(is.na(x$reads))) paste0(", reads format ", series_text(x$reads))),
    "",
    "Store Releases:",
    lines,
    if (length(x$problems)) c("", paste0("! ", x$problems))
  )
}

#' @export
print.og_local_check <- print_formatted

# A Store Release a Local Source holds, opened: the one `og_local()` opened, or, when that handle no
# longer reaches Python (a Source restored from disk in a new session), opened again the way
# og_local() opened it, refused the same way if it can no longer be read.
local_release <- function(src, store_id, call = rlang::caller_env()) {
  path <- local_release_path(src, store_id, call)
  release <- get0(path, envir = src$handles, inherits = FALSE)
  if (is.null(release) || local_is_stale(release)) {
    ogquery <- local_ogquery(call)
    found <- read_release(path, ogquery)
    refuse_release_problems(list(found), ogquery$info, call = call)
    release <- found$release
    assign(path, release, envir = src$handles)
  }
  release
}

# The path of the Store Release of GWAS Store `store_id` this Source holds, which keys what is
# kept of it in `src$handles`.
local_release_path <- function(src, store_id, call = rlang::caller_env()) {
  path <- src$releases$path[src$releases$store_id == store_id]
  if (length(path) != 1) {
    og_abort(paste0("This Local Source holds no Store Release of GWAS Store `", store_id, "`."),
             class = "opengwasr_internal", call = call)
  }
  path
}

# The query facade over a Store Release this Source holds (ogquery's `StoreRelease.query()`), kept
# beside its handle: making one reads the release's indexes, so it is made once, and again only
# when it no longer reaches Python.
local_facade <- function(src, store_id, call = rlang::caller_env()) {
  release <- local_release(src, store_id, call)
  key <- paste0("query:", local_release_path(src, store_id, call))
  facade <- get0(key, envir = src$handles, inherits = FALSE)
  if (is.null(facade) || local_is_stale(facade)) {
    facade <- local_open_facade(release)
    # ogquery refuses a Ragged or Hybrid release without its by-variant index, which the Data API
    # wouldn't serve either: refused as a release a Local Source can't read.
    if (inherits(facade, "error")) {
      local_unsupported(c("This Store Release has no by-variant index, which ogquery needs to read it.",
                          x = conditionMessage(facade)),
                        "Use a copy of the Store Release that has its index.", call = call)
    }
    assign(key, facade, envir = src$handles)
  }
  facade
}

# The variants at each `chromosome[i]:position[i]`, in variant-index order, each once. A chromosome
# ogquery can't normalise, one no ALID spells, such as "27" or "NA", is refused, naming each, as the
# API fails the Job.
variants_at_positions <- function(facade, chromosome, position, call = rlang::caller_env()) {
  found <- lapply(seq_along(chromosome), function(i) local_read_variants_at(facade, chromosome[[i]], position[[i]]))
  bad <- unique(chromosome[vapply(found, is.null, NA)])
  if (length(bad) > 0) {
    og_abort(c(paste0(if (length(bad) == 1) "No variant can be on chromosome " else "No variant can be on chromosomes ",
                      some_ids(bad, 10, quote = "`"), "."),
               i = "Give positions as `chromosome:position`, such as \"16:53767042\" or \"X:2500\"."),
             class = "opengwasr_argument", chromosomes = bad, call = call)
  }
  rows <- do.call(rbind, c(list(lookup_fields()), found))
  rows <- rows[!duplicated(rows$variant_index), , drop = FALSE]
  rows[order(rows$variant_index), , drop = FALSE]
}

# What a Local Source refuses ---------------------------------------------------------------------
# The verbs with no local meaning (#32, recorded on the epic): a Local Source has no API to ask, no
# queue, and no LD Store. A query without a Local Source method refuses through check_source().
# Clumping and LD matrices read its LD resource (#34), through the API Source's methods.

local_unsupported <- function(message, advice, call = rlang::caller_env()) {
  og_abort(c(message, i = advice), class = "opengwasr_local_unsupported", refusal = TRUE, call = call)
}

# An LD-only Local Source (`og_local(bfile = )`, #43) holds no Store Releases: every verb that reads
# them refuses it (resolve_releases(), the one place every such verb names its Store Releases, and
# og_tophits() before its clumping checks), and so does matching ALIDs to a PLINK Reference, which
# looks their rsIDs up there (`alids`, those that needed it). Any other Source passes.
check_gwas_source <- function(src, call = rlang::caller_env(), alids = NULL) {
  if (!inherits(src, "og_local_source") || nrow(src$releases) > 0) return(invisible())
  what <- if (is.null(alids)) {
    "This Local Source holds no Store Releases: it was made with only a PLINK Reference (`og_local(bfile = )`), for LD."
  } else {
    paste0("This Local Source holds no Store Releases to look up the rsIDs of ",
           big_number(length(alids)), if (length(alids) == 1) " ALID" else " ALIDs",
           " (", some_ids(alids, 3), "), which its PLINK Reference matches by rsID.")
  }
  og_abort(c(what,
             i = "Give it the Store Releases too: `og_local(gwas = , bfile = )`, or `og_local(inherit = src, gwas = )`.",
             if (!is.null(alids)) c(i = "Or pass a data frame with an `rsid` column, such as top hits or Associations, or rsIDs.")),
           class = "opengwasr_gwas_store_required", refusal = TRUE, alids = alids, arg = "gwas", call = call)
}

#' @export
og_status.og_local_source <- function(src) {
  local_unsupported("A Local Source has no API whose status to report.",
                    "Check its Python, ogquery and Store Releases with `og_local_check(src)`.")
}

#' @export
og_user.og_local_source <- function(src) {
  local_unsupported("A Local Source has no API to sign in to, so no user or allowance.",
                    "Check its Python, ogquery and Store Releases with `og_local_check(src)`.")
}

# A local query is answered at once, so there is no Job to submit, look at, collect or cancel.
refuse_local_jobs <- function(src, call = rlang::caller_env()) {
  if (!inherits(src, "og_local_source")) return(invisible())
  local_unsupported("A Local Source answers each query at once, so it has no Jobs.",
                    "Call the query function directly, such as `og_variants_rsid(src, ...)` or `og_analyses(src, ...)`.",
                    call = call)
}

# Reading downloaded LD Stores isn't available yet (ADR 0006), and a local query never reads the
# API's: the operations that read LD Stores only say what does answer them. og_ld_matrix() and
# og_clump() read the Source's PLINK Reference (R/ld-matrix.R, R/clump.R).
ld_stores_on_api <- "Use the API Source, `og_api()`, as `src`: it reads the LD Stores."

#' @export
og_ld_stores.og_local_source <- function(src) {
  local_unsupported("A Local Source holds no LD Stores.", ld_stores_on_api)
}

#' @export
og_ld_proxies.og_local_source <- function(src, variants, r2 = 0.8, kb = 10000) {
  local_unsupported("`og_ld_proxies()` reads LD Stores only, and a Local Source can't read one yet, nor a PLINK Reference for this.",
                    ld_stores_on_api)
}

#' @export
og_ld_reflookup.og_local_source <- function(src, variants) {
  local_unsupported("`og_ld_reflookup()` reads LD Stores only, and a Local Source can't read one yet, nor a PLINK Reference for this.",
                    ld_stores_on_api)
}

# Paths, before any Python ------------------------------------------------------------------------

# Kept: an R argument's type and shape, and whether each path exists, which needs no Python.
# `exist = FALSE` (og_local_check()) reports a missing path as a row rather than refusing it. `arg`
# is the caller's name for the paths: og_local()'s `gwas`, og_local_check()'s `path`.
check_release_paths <- function(path, exist = TRUE, arg, call = rlang::caller_env()) {
  if (!is.character(path) || !length(path) || anyNA(path) || !all(nzchar(path))) {
    og_abort(c(paste0("`", arg, "` must be the directory of each GWAS Store, or of each Store Release: a character vector, one element per GWAS Store."),
               i = "For example `og_local(c(\"/data/opengwas/OGS-00009\", \"/data/opengwas/OGS-00011\"))`."),
             class = "opengwasr_argument", arg = arg, call = call)
  }
  path <- normalise_path(path)
  missing <- path[!dir.exists(path)]
  if (exist && length(missing)) {
    refuse_not_store_release(missing, rep("no such directory", length(missing)), call = call)
  }
  path
}

# A path as og_local() and og_local_check() store, report and compare it, normalised once: absolute,
# and with forward slashes on every platform, so a path from system.file() or file.path() (which
# join with "/") and one from normalizePath() or an environment variable (which use "\\" on
# Windows) are the same string. `resolve = FALSE` changes only the separators: a Python is reported
# as named, since resolving a virtualenv's `python` link would name the interpreter it links to,
# not the virtualenv. A path that doesn't exist isn't resolved, but its separators still change.
normalise_path <- function(path, resolve = TRUE) {
  if (resolve) path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (on_windows()) path <- gsub("\\", "/", path, fixed = TRUE)
  path
}

on_windows <- function() .Platform$OS.type == "windows"

# Reading each Store Release through ogquery ----------------------------------------------------

# Why a Store Release can't be read, as `problem` holds it (NA when it can).
problem <- list(not_release = "not_release", several = "several", format = "format", unavailable = "unavailable")

# The one rule for which Store Release directory a path names, used with Python and without:
# `release_path`, that directory or NA, and `candidates`, the Store Release directories found.
# * A directory holding a manifest.json is a Store Release directory, and names itself.
# * A directory that looks like a Store Release (it holds data.zarr or analyses.tsv) but has no
#   manifest.json names none: its manifest is missing. It is never searched as a GWAS Store
#   directory, so a Hybrid's Dense Component (<release>/dense/manifest.json) is never found there.
# * Any other directory is a GWAS Store directory: each directory one level down holding a
#   manifest.json is a candidate, never deeper, and never a hidden one (its name starts with "."),
#   such as a build in progress. One candidate is its Store Release; none or several name none.
# * A path that doesn't exist, or is a file, names none.
# `reason` says why none is named, except for several, which find_release() describes from their
# manifests. Candidates are in C-locale order, the same on every machine.
release_dir <- function(path) {
  none <- function(reason, candidates = character()) {
    list(release_path = NA_character_, candidates = candidates, reason = reason)
  }
  if (!file.exists(path)) return(none("no such directory"))
  if (!dir.exists(path)) return(none("not a directory"))
  if (has_manifest(path)) return(list(release_path = path, candidates = path, reason = NA_character_))
  if (any(file.exists(file.path(path, c("data.zarr", "analyses.tsv"))))) {
    return(none("a Store Release without its manifest.json"))
  }
  inside <- list.dirs(path, full.names = TRUE, recursive = FALSE)
  inside <- inside[!startsWith(basename(inside), ".") & has_manifest(inside)]
  candidates <- normalise_path(sort(inside, method = "radix"))
  if (length(candidates) == 1) return(list(release_path = candidates, candidates = candidates, reason = NA_character_))
  none(if (!length(candidates)) "no Store Release in it or one level down (no manifest.json)" else NA_character_,
       candidates)
}

has_manifest <- function(dir) utils::file_test("-f", file.path(dir, "manifest.json"))

# What ogquery makes of one path given (`path`): read_release() of the one Store Release it names,
# at `release_path`; or, for a GWAS Store directory holding none or several, the problem, with each
# one found in `candidates` (its manifest's identity, read as read_release() reads it, and path).
find_release <- function(path, ogquery) {
  named <- release_dir(path)
  if (!is.na(named$release_path)) {
    found <- read_release(named$release_path, ogquery)
    found$path <- path
    return(found)
  }
  row <- list(path = path, release_path = NA_character_, store_id = NA_character_, release_id = NA_character_,
              format_version = NA_character_, release = NULL)
  dirs <- named$candidates
  if (length(dirs) < 2) return(c(row, list(problem = problem$not_release, reason = named$reason)))
  ids <- lapply(dirs, function(dir) manifest_identity(dir, ogquery)$identity)
  candidates <- data.frame(store_id = vapply(ids, `[[`, "", "store_id"),
                           release_id = vapply(ids, `[[`, "", "release_id"), path = dirs,
                           stringsAsFactors = FALSE)
  names <- release_names(na_as(candidates$store_id, "?"), na_as(candidates$release_id, "?"))
  c(row, list(problem = problem$several, candidates = candidates,
              reason = paste0(length(dirs), " Store Releases one level down (",
                              paste0(names, " in ", basename(dirs), collapse = ", "),
                              "); a GWAS Store directory holds one")))
}

# A Store Release's identity from its manifest: through ogquery's StoreManifest.load(), or, for a
# manifest it refuses, what the file declares (NA for each field it lacks). The one reader of a
# manifest: `identity`, and `error`, what StoreManifest.load() raised (NULL when it read it).
manifest_identity <- function(dir, ogquery) {
  loaded <- tryCatch(local_load_manifest(ogquery, dir), error = identity)
  if (!inherits(loaded, "error")) return(list(identity = loaded, error = NULL))
  list(identity = local_declared_manifest(ogquery, dir), error = loaded)
}

# What ogquery makes of one path: its manifest's identity, the opened release, and its `problem`
# and `reason`. The manifest is read first, with ogquery's StoreManifest.load(): a path with none
# isn't a Store Release, nor is one whose manifest ogquery can't parse. A format ogquery can't
# read is found the ways ogquery finds it: the manifest declares a series ogquery doesn't
# implement (`UnsupportedFormatVersion`), or the Variant Axis refuses a 0.2.x release opened
# without a reader for its Store Variant Table (`VariantTableReaderRequired`), which opengwasr
# doesn't have.
read_release <- function(path, ogquery) {
  row <- function(manifest = list(), release = NULL, problem = NA_character_, reason = NA_character_) {
    list(path = path, release_path = path, store_id = manifest$store_id %||% NA_character_,
         release_id = manifest$release_id %||% NA_character_,
         format_version = manifest$format_version %||% NA_character_,
         release = release, problem = problem, reason = reason)
  }
  if (!dir.exists(path)) return(row(problem = problem$not_release, reason = "no such directory"))
  read <- manifest_identity(path, ogquery)
  manifest <- read$identity
  if (inherits(read$error, "ogquery.format.UnsupportedFormatVersion")) {
    # ogquery's refusal carries no version, and its wording is no contract: the identity and the
    # version are read from the manifest itself.
    return(row(manifest, problem = problem$format, reason = format_reason(ogquery$info)))
  }
  if (inherits(read$error, "python.builtin.FileNotFoundError")) {
    return(row(problem = problem$not_release, reason = "not a Store Release (no manifest)"))
  }
  if (!is.null(read$error)) {
    return(row(problem = problem$not_release,
               reason = paste0("not a Store Release (its manifest can't be read: ", python_message(read$error), ")")))
  }
  release <- tryCatch(local_open_release(ogquery, path), error = identity)
  if (!inherits(release, "error")) {
    probed <- tryCatch(local_probe_release(release), error = identity)
    if (inherits(probed, "error")) {
      failed <- probed
    } else {
      return(row(manifest, release = release))
    }
  } else {
    failed <- release
  }
  if (inherits(failed, "ogquery.release.VariantTableReaderRequired") ||
      inherits(failed, "ogquery.format.UnsupportedFormatVersion")) {
    return(row(manifest, problem = problem$format, reason = format_reason(ogquery$info)))
  }
  row(manifest, problem = problem$not_release,
      reason = paste0("not a Store Release ogquery can read: ", python_message(failed)))
}

release_rows <- function(found) {
  field <- function(name) vapply(found, function(f) as.character(f[[name]]), character(1))
  data.frame(store_id = field("store_id"), release_id = field("release_id"),
             format_version = field("format_version"), path = field("path"), release_path = field("release_path"),
             problem = field("problem"), reason = field("reason"), stringsAsFactors = FALSE)
}

# Refuse the paths whose Store Release can't be read (`found`, from find_release() or
# read_release()): first those that aren't or hold no Store Release, then GWAS Store directories
# holding several, then Store Releases whose format ogquery can't read. Each is named by the Store
# Release directory when there is one, and otherwise by the path given.
refuse_release_problems <- function(found, info, call = rlang::caller_env()) {
  rows <- release_rows(found)
  where <- ifelse(is.na(rows$release_path), rows$path, rows$release_path)
  not_release <- which(rows$problem %in% problem$not_release)
  if (length(not_release)) refuse_not_store_release(where[not_release], rows$reason[not_release], call = call)
  several <- which(rows$problem %in% problem$several)
  if (length(several)) refuse_several_releases(found[several], call = call)
  unreadable <- which(rows$problem %in% problem$format)
  if (length(unreadable)) refuse_format(where[unreadable], rows$format_version[unreadable], info, call = call)
  invisible()
}

refuse_several_releases <- function(found, call = rlang::caller_env()) {
  candidates <- do.call(rbind, lapply(found, `[[`, "candidates"))
  directory <- rep(vapply(found, `[[`, "", "path"), vapply(found, function(f) nrow(f$candidates), integer(1)))
  releases <- release_names(candidates$store_id, candidates$release_id)
  og_abort(c(paste0("A GWAS Store directory holds one Store Release, and ",
                    if (length(found) == 1) "this one holds several: " else "these hold several: ",
                    paste0(unique(directory), collapse = ", ")),
             stats::setNames(paste0(releases, ": ", candidates$path), rep("x", nrow(candidates))),
             i = "Give `og_local()` the directory of the Store Release you want instead, one of those listed."),
           class = "opengwasr_several_releases", refusal = TRUE, directory = directory,
           store_id = candidates$store_id, release_id = candidates$release_id, releases = releases,
           paths = candidates$path, call = call)
}

refuse_not_store_release <- function(paths, reasons, call = rlang::caller_env()) {
  og_abort(c(paste0(if (length(paths) == 1) "This path isn't a Store Release:" else "These paths aren't Store Releases:"),
             stats::setNames(paste0(paths, " (", reasons, ")"), rep("x", length(paths))),
             i = paste("Give `og_local()` the directory of each GWAS Store, holding its Store Release one level down,",
                       "or of each Store Release, the one holding its `manifest.json`.")),
           class = "opengwasr_not_store_release", paths = paths, reasons = reasons, call = call)
}

series_text <- function(series) paste0(series, ".x", collapse = ", ")

format_reason <- function(info) paste0("ogquery ", info$ogquery, " reads format ", series_text(info$reads))

refuse_format <- function(paths, format_versions, info, call = rlang::caller_env()) {
  og_abort(c(paste0("ogquery ", info$ogquery, " can't read the format of ",
                    if (length(paths) == 1) "this Store Release:" else "these Store Releases:"),
             stats::setNames(paste0(paths, " (format ", na_as(format_versions, "unknown"), ")"), rep("x", length(paths))),
             i = paste0(format_reason(info), ". Use Store Releases of that format, or a newer ogquery that reads theirs.")),
           class = "opengwasr_format_unsupported", refusal = TRUE, format_version = format_versions,
           readable = info$reads, ogquery = info$ogquery, paths = paths, call = call)
}

duplicate_stores <- function(rows) {
  ids <- rows$store_id[!is.na(rows$store_id)]
  unique(ids[duplicated(ids)])
}

refuse_duplicate_store <- function(rows, store_id, call = rlang::caller_env()) {
  these <- rows[rows$store_id %in% store_id, , drop = FALSE]
  releases <- release_names(these$store_id, these$release_id)
  og_abort(c(paste0("A Local Source holds one Store Release of each GWAS Store, and these are both of GWAS Store `",
                    store_id, "`:"),
             stats::setNames(paste0(releases, ": ", these$path), rep("x", nrow(these))),
             i = "Give `og_local()` the path of only the one you want to query."),
           class = "opengwasr_duplicate_store", refusal = TRUE, store_id = store_id,
           releases = releases, paths = these$path, call = call)
}

# Finding Python and ogquery ----------------------------------------------------------------------

# What the first local call found, kept for the session: Python starts once per R process, and
# can't be swapped once it has.
local_state <- new.env(parent = emptyenv())

# The ogquery releases opengwasr reads, as one pip requirement: from 0.1.0, the oldest release
# tested (0.3.0 passes too), up to, not including, 1.0. ogquery 0.0.0, an early placeholder on PyPI,
# has no `ogquery.release`. Everything that names the range uses these: py_require(), the version
# check, the install advice, og_local_check(); and, as text, DESCRIPTION, the CI workflows and
# vignette("local-source"), which say it once each.
ogquery_range <- c(lower = "0.1.0", upper = "1")
ogquery_required <- paste0(">=", ogquery_range[["lower"]], ",<", ogquery_range[["upper"]])
ogquery_requirement <- paste0("ogquery", ogquery_required)

# Whether an ogquery version is in ogquery_range. Only its release numbers are compared, so a
# pre-release or development build counts as its release: "1.0.0rc1" is outside, "0.3.1.dev2" in.
# No version (NA or NULL), or one that doesn't start with a number, is outside.
ogquery_in_range <- function(version) {
  if (!length(version) || is.na(version)) return(FALSE)
  release <- regmatches(version, regexpr("^[0-9]+(\\.[0-9]+)*", version))
  if (!length(release)) return(FALSE)
  release <- numeric_version(release)
  release >= ogquery_range[["lower"]] && release < ogquery_range[["upper"]]
}

# ogquery's modules, and `info`: the Python, how it was chosen, the ogquery version and the format
# series it reads. A Python the user chose wins (python_choice()), and py_require() then isn't
# called: reticulate's own discovery (py_discover_config() in reticulate 1.47) ranks that choice
# above its automatic environment, and a requirement declared for the automatic one would only
# matter if reticulate fell back to it, which opengwasr refuses (python_swapped()). Without a
# choice, py_require() declares ogquery in ogquery_range, before Python starts or, for
# reticulate's automatic environment already running, after (reticulate adds it then). How the
# Python was chosen is what reticulate says once it has started it (started_by()). What the Python
# has of ogquery is checked before anything is imported from it: none, or a version outside
# ogquery_range, or one without `ogquery.release`, is refused, so no read meets an ogquery
# opengwasr doesn't support.
local_ogquery <- function(call = rlang::caller_env()) {
  if (!is.null(local_state$ogquery)) return(local_state$ogquery)
  require_reticulate(call)
  choice <- python_choice(if (local_py_started()) local_py_config())
  if (is.null(choice)) local_py_require(ogquery_requirement)
  found <- tryCatch(local_find_ogquery(), error = identity)
  config <- tryCatch(local_py_config(), error = function(e) NULL)
  python <- normalise_path(config$python %||% choice$python %||% NA_character_, resolve = FALSE)
  chosen_by <- if (!is.null(config)) started_by(config) else choice$chosen_by %||% NA_character_
  if (python_swapped(choice, config)) {
    local_unavailable(c(paste0("You chose a Python with `", choice$chosen_by, "`, but reticulate started ",
                               python, " (", chosen_by, ") instead."),
                        i = paste0("Check that `", choice$chosen_by, "` names a working Python 3.12 or later with ogquery, then restart R.")),
                      need = "chosen_python", python = python, chosen_by = choice$chosen_by,
                      started_by = chosen_by, setting = choice$chosen_by, call = call)
  }
  # No `forced`, and the paths matched (python_swapped()): the Python is the one the user chose.
  if (!is.null(choice) && !is.null(config) && is.null(config$forced) && !isTRUE(config$ephemeral)) {
    chosen_by <- choice$chosen_by
  }
  if (inherits(found, "error")) python_unavailable(found, python, chosen_by, choice, call = call)
  if (!isTRUE(found$installed)) ogquery_missing(python, chosen_by, choice, found$pip, call)
  version <- found$version %||% NA_character_
  if (!isTRUE(found$release) || !ogquery_in_range(version)) {
    ogquery_unsupported(version, python, chosen_by, found$pip, call)
  }
  modules <- tryCatch(local_import_ogquery(), error = identity)
  if (inherits(modules, "error")) python_unavailable(modules, python, chosen_by, choice, found$pip, call)
  local_state$ogquery <- c(modules, list(info = list(python = python, chosen_by = chosen_by,
                                                      ogquery = version, reads = local_ogquery_reads(modules))))
  local_state$ogquery
}

# The ways a user chooses a Python, in the order reticulate's discovery ranks them, each by the
# code opengwasr reports it as (`chosen_by`): RETICULATE_PYTHON, RETICULATE_PYTHON_ENV, a call of
# use_python(), use_virtualenv() or use_condaenv() ("use_python"), an activated virtualenv
# (VIRTUAL_ENV). The other codes are "automatic" (reticulate's uv environment) and "reticulate" (a
# Python reticulate found itself: on the PATH, a ./.venv, its r-reticulate virtualenv).
user_choices <- c("RETICULATE_PYTHON", "RETICULATE_PYTHON_ENV", "use_python", "VIRTUAL_ENV")

# How reticulate chose the Python it started, from its config (`forced`), as one of those codes.
started_by <- function(config) {
  if (isTRUE(config$ephemeral)) return("automatic")
  switch(config$forced %||% "",
         RETICULATE_PYTHON = "RETICULATE_PYTHON",
         RETICULATE_PYTHON_ENV = "RETICULATE_PYTHON_ENV",
         VIRTUAL_ENV = "VIRTUAL_ENV",
         "use_python() function" = "use_python",
         "reticulate")
}

# The Python the user chose, seen in reticulate's settings before Python starts (`config` NULL),
# or the Python already running (`config`): `list(python, chosen_by, started)`, or NULL for no
# choice. `python` is NULL when the setting names an environment, not a Python. The settings are
# read as reticulate reads them, in its order: RETICULATE_PYTHON (`managed` asks for its automatic
# environment), RETICULATE_PYTHON_ENV, RETICULATE_USE_MANAGED_VENV (a request for the automatic
# environment over an activated virtualenv), then VIRTUAL_ENV. A use_*() call ranks between
# RETICULATE_PYTHON_ENV and RETICULATE_USE_MANAGED_VENV but can't be seen before Python starts.
python_choice <- function(config = NULL) {
  if (!is.null(config)) {
    if (isTRUE(config$ephemeral)) return(NULL)
    return(list(python = config$python, chosen_by = started_by(config), started = TRUE))
  }
  python <- Sys.getenv("RETICULATE_PYTHON")
  if (nzchar(python)) {
    if (python == "managed") return(NULL)
    return(list(python = normalise_path(python, resolve = FALSE), chosen_by = "RETICULATE_PYTHON", started = FALSE))
  }
  if (nzchar(Sys.getenv("RETICULATE_PYTHON_ENV"))) {
    return(list(python = NULL, chosen_by = "RETICULATE_PYTHON_ENV", started = FALSE))
  }
  if (tolower(Sys.getenv("RETICULATE_USE_MANAGED_VENV")) %in% c("true", "1", "yes")) return(NULL)
  if (nzchar(Sys.getenv("VIRTUAL_ENV"))) return(list(python = NULL, chosen_by = "VIRTUAL_ENV", started = FALSE))
  NULL
}

# Whether reticulate started a Python other than the one the user chose. reticulate tries each
# choice and, if it can't start it, goes on to the next, and its `forced` says which it started.
# The choice seen is honoured when `forced` names it, or names a choice reticulate ranks higher
# (a use_*() call, which can't be seen beforehand). A lower choice, reticulate's automatic
# environment, or a Python it found itself is a swap. Only with no `forced` at all are the paths
# compared.
python_swapped <- function(choice, config) {
  if (is.null(choice) || isTRUE(choice$started) || is.null(config)) return(FALSE)
  if (is.null(config$forced) && !isTRUE(config$ephemeral)) return(!same_python(choice$python, config$python))
  by <- started_by(config)
  if (identical(by, choice$chosen_by)) return(FALSE)
  !(by %in% user_choices && match(by, user_choices) < match(choice$chosen_by, user_choices))
}

# Whether `started` is the Python `named` names: the same file, or the Python in the directory it
# names (reticulate takes a directory, its bin/ or, on Windows, its Scripts/, for RETICULATE_PYTHON).
# Both are normalised and resolved, so links and separators don't matter.
same_python <- function(named, started) {
  if (is.null(named) || is.null(started)) return(FALSE)
  candidates <- c(named, file.path(named, c("python", "python.exe", "bin/python", "Scripts/python.exe")))
  normalise_path(started) %in% normalise_path(candidates)
}

require_reticulate <- function(call) {
  if (local_reticulate_installed()) return(invisible())
  installed <- tryCatch({
    local_install_reticulate()
    local_reticulate_installed()
  }, error = function(e) FALSE)
  if (!installed) {
    install <- "install.packages(\"reticulate\")"
    local_unavailable(c("Reading Store Releases needs the R package reticulate, which isn't installed.",
                        i = paste0("Install it with `", install, "`, then call `og_local()` again.")),
                      need = "reticulate", install = install, call = call)
  }
}

python_unavailable <- function(e, python, chosen_by, choice, pip = NULL, call) {
  if (inherits(e, "python.builtin.ModuleNotFoundError") || inherits(e, "python.builtin.ImportError")) {
    ogquery_missing(python, chosen_by, choice, pip, call, why = python_message(e))
  }
  install <- paste0("pip install ", shell_word(ogquery_requirement))
  local_unavailable(c("Can't find Python with ogquery.",
                      x = python_message(e),
                      i = paste0("Set `RETICULATE_PYTHON` to a Python 3.12 or later that has ogquery (`", install, "`), ",
                                 "or leave Python unset to let reticulate set one up with uv, which needs the internet the first time.")),
                    need = "python", python = python, chosen_by = chosen_by, install = install,
                    setting = "RETICULATE_PYTHON", call = call)
}

# A Python without ogquery, or that can't import a part of it opengwasr uses (`why`, Python's
# error).
ogquery_missing <- function(python, chosen_by, choice, pip, call, why = NULL) {
  install <- ogquery_install(python, pip)
  local_unavailable(c(paste0("The Python in use, ", python, " (", chosen_by, "), can't import ogquery."),
                      x = why,
                      i = paste0("Install ogquery into that Python: `", install, "`, then restart R."),
                      if (is.null(choice)) c(i = "Or set `RETICULATE_PYTHON` to a Python that has ogquery.")),
                    need = "ogquery", python = python, chosen_by = chosen_by, install = install,
                    setting = if (is.null(choice)) "RETICULATE_PYTHON" else NA_character_, call = call)
}

# An ogquery opengwasr doesn't support: a version (`found`, or NA for none) outside ogquery_range,
# or one without `ogquery.release`. For a Python the user chose, the advice installs a supported
# ogquery there. For reticulate's automatic environment, which py_require() asked for a supported
# ogquery, an unsupported one came from uv's cache or from a session whose Python started before
# that requirement: the advice clears uv's cached ogquery, with the uv reticulate uses, and
# restarts R, so reticulate resolves ogquery afresh.
ogquery_unsupported <- function(found, python, chosen_by, pip, call) {
  has <- if (is.na(found)) "an ogquery with no version" else paste0("ogquery ", found)
  needs <- paste0("opengwasr needs ogquery ", ogquery_required, ".")
  if (identical(chosen_by, "automatic")) {
    install <- local_uv_cache_clean()
    advice <- c(i = paste0("reticulate set this Python up automatically, with uv. Clear uv's cached ogquery with `",
                           install, "`, then restart R, and reticulate sets up a supported ogquery."),
                i = paste0("Or set `RETICULATE_PYTHON` to a Python that has a supported ogquery (`",
                           ogquery_install("<python>", TRUE), "`)."))
  } else {
    install <- ogquery_install(python, pip)
    advice <- c(i = paste0("Install a supported ogquery into that Python: `", install, "`, then restart R."))
  }
  local_unavailable(c(paste0("The Python in use, ", python, " (", chosen_by, "), has ", has, ". ", needs), advice),
                    need = "ogquery_version", found = found, python = python, chosen_by = chosen_by,
                    install = install, call = call)
}

# The command that installs a supported ogquery into `python`: with its pip, or, for a Python
# without pip (`pip` FALSE, such as one uv made), with uv. `pip` NULL (not known) assumes pip.
ogquery_install <- function(python, pip) {
  if (isFALSE(pip)) {
    paste0("uv pip install --python ", shell_word(python), " ", shell_word(ogquery_requirement))
  } else {
    paste0(shell_word(python), " -m pip install ", shell_word(ogquery_requirement))
  }
}

# A word of a shell command, quoted when it holds what a shell would split or redirect on.
shell_word <- function(x) if (grepl("[[:space:]<>|&;()]", x)) paste0("\"", x, "\"") else x

# Every one carries `required`, the ogquery versions opengwasr supports, and `found`, the version
# found when it is outside them (need "ogquery_version").
local_unavailable <- function(message, need, python = NA_character_, chosen_by = NA_character_,
                              started_by = NA_character_, install = NA_character_,
                              setting = NA_character_, found = NA_character_, call) {
  og_abort(message, class = "opengwasr_local_unavailable", need = need, python = python,
           chosen_by = chosen_by, started_by = started_by, install = install, setting = setting,
           found = found, required = ogquery_required, call = call)
}

# og_local_check()'s reason for each Store Release when no Python with ogquery is found.
unavailable_reason <- c(reticulate = "reticulate isn't installed", python = "no Python found",
                        ogquery = "the Python in use has no ogquery",
                        ogquery_version = paste0("the Python in use has an ogquery outside ", ogquery_required),
                        chosen_python = "reticulate didn't start the Python you chose")

python_message <- function(e) {
  sub("\nRun `reticulate::py_last_error\\(\\)` for details\\.$", "", conditionMessage(e))
}

na_as <- function(x, missing) if (is.null(x)) missing else ifelse(is.na(x), missing, x)

# The Python boundary ---------------------------------------------------------------------------
# Every call into reticulate, and every touch of a Python object. The tests mock these.

local_reticulate_installed <- function() rlang::is_installed("reticulate")

local_install_reticulate <- function() {
  rlang::check_installed("reticulate", reason = "to read Store Releases with `og_local()`")
}

local_reticulate_version <- function() {
  if (!local_reticulate_installed()) return(NA_character_)
  as.character(utils::packageVersion("reticulate"))
}

local_py_started <- function() reticulate::py_available(initialize = FALSE)

# reticulate's account of the Python it started (and starts it, if it hasn't): its path, how
# reticulate chose it (`forced`, such as "RETICULATE_PYTHON" or "py_require()") and whether it is
# reticulate's automatic environment.
local_py_config <- function() {
  config <- reticulate::py_config()
  list(python = config$python, forced = config$forced, ephemeral = isTRUE(config$ephemeral))
}

local_py_require <- function(packages) reticulate::py_require(packages)

# What the Python in use has of ogquery, found without importing any of it but its package (and
# starting Python, if it hasn't started): `installed`; `version`, from `ogquery.version` (as
# ogquery 0.1.0 and later have it), or else the installed distribution's metadata (NULL for
# neither); `release`, whether it has `ogquery.release`; and `pip`, whether the Python has pip.
local_find_ogquery <- function() {
  found <- reticulate::py_run_string(local_python_find, local = TRUE)$find()
  list(installed = isTRUE(found$installed), version = found$version, release = isTRUE(found$release),
       pip = isTRUE(found$pip))
}

# Its imports are inside find(): py_run_string(local = TRUE) keeps top-level names out of the
# globals a function defined there sees.
local_python_find <- "
def find():
    import importlib, importlib.metadata, importlib.util

    def has(name):
        try:
            return importlib.util.find_spec(name) is not None
        except Exception:
            return False
    found = {'installed': has('ogquery'), 'version': None, 'release': False, 'pip': has('pip')}
    if not found['installed']:
        return found
    found['release'] = has('ogquery.release')
    try:
        found['version'] = str(importlib.import_module('ogquery.version').__version__)
    except Exception:
        try:
            found['version'] = importlib.metadata.version('ogquery')
        except Exception:
            pass
    return found
"

# The command that clears uv's cached ogquery, with the uv reticulate uses and its cache. This
# follows reticulate 1.47's choice of uv (its internal uv_binary()): `RETICULATE_UV`, unless
# "managed"; then the uv reticulate last used (option `reticulate.uv_binary`); then a uv on the
# PATH or in ~/.local/bin; and otherwise reticulate's own uv, under its cache directory
# (tools::R_user_dir("reticulate", "cache")), which keeps its cache there too (`--cache-dir`).
local_uv_cache_clean <- function() {
  managed_root <- file.path(tools::R_user_dir("reticulate", "cache"), "uv")
  managed <- file.path(managed_root, "bin", if (on_windows()) "uv.exe" else "uv")
  configured <- Sys.getenv("RETICULATE_UV", unset = NA)
  cached <- getOption("reticulate.uv_binary")
  uv <- if (!is.na(configured) && configured != "managed") {
    configured
  } else if (is.na(configured) && length(cached) && !identical(cached, "managed") &&
             !isTRUE(attr(cached, "reticulate-managed", exact = TRUE))) {
    cached
  } else if (is.na(configured)) {
    on_path <- c(as.character(Sys.which("uv")), path.expand("~/.local/bin/uv"))
    on_path[nzchar(on_path) & file.exists(on_path)][1]
  }
  if (length(uv) && !is.na(uv)) return(paste0(shell_word(normalise_path(uv, resolve = FALSE)), " cache clean ogquery"))
  paste0(shell_word(normalise_path(managed, resolve = FALSE)), " cache clean ogquery --cache-dir ",
         shell_word(normalise_path(file.path(managed_root, "cache"), resolve = FALSE)))
}

# Starts Python, if it hasn't started, and imports the parts of ogquery opengwasr uses.
local_import_ogquery <- function() {
  list(release = reticulate::import("ogquery.release"), manifest = reticulate::import("ogquery.manifest"),
       format = reticulate::import("ogquery.format"), json = reticulate::import("json"), identity = reticulate::import("ogquery.identity"),
       stats = reticulate::import("ogquery.stats"), top_hits = reticulate::import("ogquery.top_hits"),
       by_variant = reticulate::import("ogquery.by_variant"),
       columns = reticulate::py_run_string(local_python_columns, local = TRUE))
}

# Two conversions done in Python, where they cost one call rather than one per row: an
# Associations record's index columns and z (`raw`), and its rows (`take(keep)` of them, 0-based,
# or all), resolved with their variant identity and rsID as the Data API resolves them
# (`resolve(..., include_variant_info=True)`), as one list per field (`resolved`).
local_python_columns <- "
def raw(record):
    return {name: record[name].tolist() for name in ('variant_index', 'analysis_index', 'z')}

def resolved(query, record, keep):
    if keep is not None:
        record = record.take(keep)
    rows = list(query.resolve(record, include_variant_info=True))
    names = list(rows[0]) if rows else []
    return {name: [row[name] for row in rows] for name in names}
"

# A Store Release's manifest, read with ogquery's StoreManifest.load(), which refuses a format
# series ogquery doesn't implement: its identity, as R strings.
local_load_manifest <- function(ogquery, path) {
  manifest <- ogquery$manifest$StoreManifest$load(path)
  list(store_id = as.character(manifest$store_id), release_id = as.character(manifest$release_id),
       format_version = as.character(manifest$format_version))
}

# What a manifest declares, for a manifest whose format StoreManifest.load() refused: its
# `store_id`, `release_id` and `format_version`, the three fields of manifest.json, parsed with
# Python's json, as ogquery parses it. NA for each the file has no such field for.
local_declared_manifest <- function(ogquery, path) {
  data <- tryCatch({
    text <- paste(readLines(file.path(path, "manifest.json"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    ogquery$json$loads(text)
  }, error = function(e) list())
  field <- function(name) {
    value <- if (is.list(data)) data[[name]]
    if (is.character(value) && length(value) == 1) value else NA_character_
  }
  list(store_id = field("store_id"), release_id = field("release_id"), format_version = field("format_version"))
}

local_open_release <- function(ogquery, path) ogquery$release$open_release(path)

# Reads what a query needs before it can read anything else, the Variant Axis, which refuses a
# 0.2.x release opened without a Store Variant Table reader.
local_probe_release <- function(release) {
  release$variant_axis(aliases = FALSE)
  invisible()
}

# The format series ogquery reads alone. ogquery has no public "which formats
# can I read" call, so this reads two undocumented parts of ogquery.format: the series it
# implements (`SUPPORTED_FORMAT_VERSIONS`), less those read through a Store Variant Table
# (`carries_variant_offsets()`: the 0.1 and 0.2 series), which needs a reader opengwasr doesn't
# have. A series, such as "0.3": ogquery reads any patch of it. Here alone, so a public call can
# replace it in one place.
local_ogquery_reads <- function(modules) {
  supported <- reticulate::iterate(modules$format$SUPPORTED_FORMAT_VERSIONS$items())
  series <- vapply(supported, function(s) paste(unlist(s[[1]]), collapse = "."), character(1))
  newest <- vapply(supported, function(s) paste(c(unlist(s[[1]]), unlist(s[[2]])), collapse = "."), character(1))
  alone <- !vapply(newest, function(v) isTRUE(modules$format$carries_variant_offsets(v)), logical(1))
  unname(series[alone])
}

local_is_stale <- function(object) reticulate::py_is_null_xptr(object)

# The query facade over an opened release; or, for a Ragged or Hybrid release without its
# by-variant index, which ogquery refuses (`VariantIndexError`), that error, which local_facade()
# refuses.
local_open_facade <- function(release) {
  tryCatch(release$query(), ogquery.by_variant.VariantIndexError = identity)
}

# A Store Release's Analyses, `analyses_table()`: every column of analyses.tsv, as text ("" for
# none), one row per Analysis in the file's order, as a data frame of character columns.
local_read_analyses <- function(facade) {
  table <- facade$analyses_table()
  table <- table[order(as.integer(names(table)))]
  columns <- if (length(table)) names(table[[1]]) else c("analysis_index", "analysis_id")
  frame <- lapply(stats::setNames(columns, columns), function(name) {
    vapply(table, function(row) as.character(row[[name]] %||% ""), character(1), USE.NAMES = FALSE)
  })
  as.data.frame(frame, stringsAsFactors = FALSE, optional = TRUE)
}

# Variant rows, as ogquery's facade gives them (a dict keyed by variant index), as a data frame
# of lookup_fields(), in variant-index order; a missing rsID is NA.
local_variant_frame <- function(found) {
  if (!length(found)) return(lookup_fields())
  found <- found[order(as.integer(names(found)))]
  text <- function(name) vapply(found, function(row) as.character(row[[name]] %||% NA_character_), "", USE.NAMES = FALSE)
  data.frame(variant_index = as.integer(names(found)), alid = text("alid"), chromosome = text("chromosome"),
             position = vapply(found, function(row) as.integer(row$position), 0L, USE.NAMES = FALSE),
             effect_allele = text("effect_allele"), other_allele = text("other_allele"), rsid = text("rsid"),
             stringsAsFactors = FALSE)
}

# The variants rsIDs or ALIDs name in a Store Release (`variants()`): every variant a multiallelic
# rsID names; an identifier the release lacks has none.
local_read_variants <- function(facade, identifiers) {
  local_variant_frame(facade$variants(as.list(identifiers)))
}

# The variants at one `chromosome:position` (`variants_in_range()`), in variant-index order; or NULL
# for a chromosome ogquery can't normalise (`VariantNormalisationError`), which
# variants_at_positions() refuses.
local_read_variants_at <- function(facade, chromosome, position) {
  tryCatch(local_variant_frame(facade$variants_in_range(chromosome, position, position)),
           ogquery.identity.VariantNormalisationError = function(e) NULL)
}

# The Analyses a Store Release holds, `analysis_index` and `analysis_id`, in index order: what a
# query maps the Analyses it names onto (analyses_held_here()).
local_read_analysis_ids <- function(facade) {
  table <- local_read_analyses(facade)
  data.frame(analysis_index = as.integer(table$analysis_index), analysis_id = table$analysis_id,
             stringsAsFactors = FALSE)
}

# A read of Associations through ogquery's facade, the one the Data API makes for the same query:
# `read` is the facade method (`lookup`, `top_hits`, `phewas` or `range_phewas`) and `args` its
# arguments. Returns ogquery's Associations record, which only local_record_index() and
# local_resolve() look into. ogquery's refusals become the API's: an Analysis the release doesn't
# hold, a top-hit tier it doesn't store, a chromosome no ALID spells. Each is checked before the
# read, as the API checks it, so these only guard what the checks missed.
local_read_associations <- function(facade, read, args, call = rlang::caller_env()) {
  tryCatch(
    do.call(facade[[read]], args),
    ogquery.analyses.UnknownAnalysisError = function(e) {
      og_abort(conditionMessage(e), class = api_error_class("analysis_not_found", 404L),
               api_error = "analysis_not_found", call = call)
    },
    ogquery.top_hits.UnstoredThresholdError = function(e) {
      og_abort(conditionMessage(e), class = api_error_class("unsupported_threshold", 400L),
               api_error = "unsupported_threshold", call = call)
    },
    ogquery.identity.VariantNormalisationError = function(e) {
      og_abort(conditionMessage(e), class = api_error_class("invalid_query", 400L),
               api_error = "invalid_query", call = call)
    }
  )
}

# A record's `variant_index` and `analysis_index` (integers) and `z` (doubles), one per row, which
# the API's cuts (threshold, rank, Analyses) read before any row is resolved.
local_record_index <- function(ogquery, record) {
  raw <- ogquery$columns$raw(record)
  list(variant_index = as.integer(unlist(raw$variant_index)), analysis_index = as.integer(unlist(raw$analysis_index)),
       z = as.numeric(unlist(raw$z)))
}

# Rows `keep` of a record (0-based, in that order; NULL for all), resolved as the API resolves
# them: a named list with one vector per field of ogquery's row, numbers as numbers, text as text.
# A field holding both (`eaf` and `n` are "." where a cell has none) is a list.
local_resolve <- function(ogquery, facade, record, keep = NULL) {
  if (!is.null(keep)) keep <- as.list(as.integer(keep))
  ogquery$columns$resolved(facade, record, keep)
}

# The top-hit tiers a Store Release stores, tightest first: ogquery's `TopHitTiers` over the
# release's arrays, as each facade opens them (a Hybrid release's are its own, facade.py:785).
local_top_hit_tiers <- function(ogquery, release) {
  as.numeric(unlist(ogquery$top_hits$TopHitTiers(release$arrays())$stored_thresholds()))
}

# A Store Release's `association_coverage`, from its manifest, as text.
local_association_coverage <- function(release) as.character(release$manifest$association_coverage)

# Whether a Store Release can read one variant across its Analyses without a scan, as the Data API
# decides it (ogdb-api releases.py `_phewas_indexed`): a Dense release always can; a Ragged or
# Hybrid one needs its by-variant index (ogquery's `has_variant_index()`, ADR 0060).
local_variant_indexed <- function(ogquery, release) {
  identical(as.character(release$manifest$primary_layout), "dense") ||
    isTRUE(ogquery$by_variant$has_variant_index(release$path))
}

# The |z| at or above which two-sided p <= `threshold` (ogquery's `z_critical()`): the one cut the
# Top-Hit Index, the Data API and a local query all make.
local_z_critical <- function(ogquery, threshold) as.numeric(ogquery$stats$z_critical(threshold))

# Whether `chromosome` names one of 1-22, X, Y or MT, as ogquery normalises it: the chromosomes the
# Data API takes a range on.
local_primary_chromosome <- function(ogquery, chromosome) {
  isTRUE(tryCatch(ogquery$identity$is_primary_contig(chromosome), error = function(e) FALSE))
}
