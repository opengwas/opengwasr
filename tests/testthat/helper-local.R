# Local Sources in tests: the example Store Releases bundled with opengwasr, and the skip.
#
# `inst/stores/` holds four of ogquery's contract releases (format 0.3.0; SOURCE there names the
# commit), laid out as three GWAS Store directories, each holding its Store Releases one level down.
# Tests name a Store Release by its name below. Their store_id, release_id and format_version are
# written in the tests as literals, read from each manifest.json by eye, never through the code
# under test:
#   name    directory     store_id      release_id    format_version
#   dense   dense/r       s             r             0.3.0
#   ragged  ragged/comp   residual-rag  comp          0.3.0
#   hyb     hyb/v1        hyb           v1            0.3.0
#   hyb-c   hyb/v1c       hyb           v1-completed  0.3.0
example_layout <- c(dense = "dense/r", ragged = "ragged/comp", hyb = "hyb/v1", `hyb-c` = "hyb/v1c")

# The directory of each named example Store Release.
example_releases <- function(...) {
  names <- c(...)
  stopifnot(all(names %in% names(example_layout)))
  local_path(file.path(system.file("stores", package = "opengwasr", mustWork = TRUE), unname(example_layout[names])))
}

# The example GWAS Store directories, by name: "dense", "ragged" or "hyb" (which holds two Store
# Releases).
example_stores <- function(...) {
  local_path(file.path(system.file("stores", package = "opengwasr", mustWork = TRUE), c(...)))
}

# A copy of the named example Store Release in a new temporary directory: the copy's path.
copy_release <- function(from, env) {
  dir <- withr::local_tempdir(.local_envir = env)
  file.copy(example_releases(from), dir, recursive = TRUE)
  file.path(dir, basename(example_releases(from)))
}

# The one way tests make a Local Source: over the named example Store Releases (by default dense,
# ragged and hyb, three GWAS Stores; or a parity source set, such as c("dense", "ragged", "hyb-c")),
# with `bfile` as its PLINK Reference if given. Each is opened once per test process and shared by
# the tests that only read it. Call it after skip_if_no_local().
local_source_of <- local({
  opened <- list()
  function(names = c("dense", "ragged", "hyb"), bfile = NULL) {
    key <- paste(c(names, bfile), collapse = "+")
    if (is.null(opened[[key]])) opened[[key]] <<- og_local(example_releases(names), bfile = bfile)
    opened[[key]]
  }
})

# A Local Source object that was never opened, so no Python: for refusals, which never read it,
# and for tests that mock the Python boundary. `releases` as og_local() keeps them.
unopened_source <- function(releases = data.frame(store_id = "s", release_id = "r", format_version = "0.3.0",
                                                  path = "/nowhere", stringsAsFactors = FALSE)) {
  structure(list(releases = releases, handles = new.env()), class = "og_local_source")
}

# A path as og_local() and og_local_check() store and report it: the one place tests normalise a
# path, through the package's own normaliser (whose separators test-local-source.R checks with
# literals), so a test compares like with like on Windows too.
local_path <- function(path) normalise_path(path)

# Skip unless this session can read Store Releases: never on CRAN, and only with a Python the run
# chose itself (`RETICULATE_PYTHON`), so a test never sets one up with uv or touches the network.
# The cheap checks come first: skip_if_not_installed() loads reticulate's namespace, about a second,
# which a run without Python needn't pay to skip.
skip_if_no_local <- function() {
  testthat::skip_on_cran()
  if (!nzchar(Sys.getenv("RETICULATE_PYTHON"))) {
    testthat::skip("RETICULATE_PYTHON isn't set; local tests never set Python up themselves.")
  }
  testthat::skip_if_not_installed("reticulate")
  found <- tryCatch(local_ogquery(), opengwasr_local_unavailable = function(e) NULL)
  if (is.null(found)) testthat::skip("The Python in RETICULATE_PYTHON has no ogquery.")
  invisible(found)
}

# A copy of an example Store Release in a temporary directory, its manifest's format_version set to
# `format_version` (the one edit, made on the JSON text so nothing else changes).
release_with_format <- function(format_version, from = "dense", env = parent.frame()) {
  path <- copy_release(from, env)
  manifest <- file.path(path, "manifest.json")
  text <- readLines(manifest)
  at <- grep('"format_version": "0.3.0"', text, fixed = TRUE)
  stopifnot(length(at) == 1)
  text[at] <- sub("0.3.0", format_version, text[at], fixed = TRUE)
  writeLines(text, manifest)
  path
}

# Run with the Python boundary of R/local-source.R in a clean state, put back afterwards, so a test
# that mocks it neither sees nor leaves a Python another test found.
local_fresh_python <- function(env = parent.frame()) {
  saved <- as.list(local_state, all.names = TRUE)
  rm(list = ls(local_state, all.names = TRUE), envir = local_state)
  withr::defer({
    rm(list = ls(local_state, all.names = TRUE), envir = local_state)
    list2env(saved, envir = local_state)
  }, envir = env)
}

# A condition shaped as reticulate raises a Python exception: its classes are the exception's
# (`classes`, most specific first, such as "python.builtin.ModuleNotFoundError").
python_error <- function(classes, message) {
  structure(class = c(classes, "python.builtin.Exception", "error", "condition"),
            list(message = message, call = NULL))
}

# The Python boundary mocked at its primitives, with none of reticulate's settings set, so a test
# drives the rule above it with `withr::local_envvar()`: whether Python has started, reticulate's
# config of it (or an error, when no Python starts), what the Python in use has of ogquery
# (`ogquery`: whether it is installed, its version, whether it has `ogquery.release`, and whether
# the Python has pip; an error, when `import` is one, as when no Python starts), and what importing
# ogquery gives (or an error). Returns an environment counting the calls of py_require()
# (`required`) and holding the requirements each was given (`packages`).
mock_python <- function(started = FALSE,
                        config = list(python = "/opt/py/bin/python", forced = "PATH", ephemeral = FALSE),
                        import = list(), ogquery = list(installed = TRUE, version = "0.1.0", release = TRUE, pip = TRUE),
                        env = parent.frame()) {
  local_fresh_python(env)
  withr::local_envvar(c(RETICULATE_PYTHON = NA, RETICULATE_PYTHON_ENV = NA, VIRTUAL_ENV = NA,
                        RETICULATE_USE_MANAGED_VENV = NA), .local_envir = env)
  calls <- new.env()
  calls$required <- 0
  calls$packages <- character()
  testthat::local_mocked_bindings(
    local_reticulate_installed = function() TRUE,
    local_py_started = function() started,
    local_py_config = function() if (inherits(config, "condition")) stop(config) else config,
    local_py_require = function(packages) {
      calls$required <- calls$required + 1
      calls$packages <- c(calls$packages, packages)
    },
    local_find_ogquery = function() if (inherits(import, "condition")) stop(import) else ogquery,
    local_import_ogquery = function() if (inherits(import, "condition")) stop(import) else import,
    local_ogquery_reads = function(modules) "0.3",
    .env = env
  )
  calls
}

# reticulate's config of its automatic environment, as reticulate 1.47 reports it.
automatic_python <- list(python = "/home/me/.cache/R/reticulate/uv/env/bin/python", forced = "py_require()",
                         ephemeral = TRUE)

# A copy of an example Store Release in a temporary directory, its analyses.tsv edited by
# `edit(table)` (a data frame of text, every cell as written; "" for none), written back as the
# same tab-separated text.
release_with_analyses <- function(edit, from = "dense", env = parent.frame()) {
  path <- copy_release(from, env)
  file <- file.path(path, "analyses.tsv")
  table <- utils::read.delim(file, colClasses = "character", na.strings = character(), check.names = FALSE,
                             quote = "")
  utils::write.table(edit(table), file, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  path
}

# A copy of an example Store Release in a temporary directory, without the file or directory at
# `drop` (relative to the release).
release_without <- function(drop, from = "ragged", env = parent.frame()) {
  path <- copy_release(from, env)
  stopifnot(file.exists(file.path(path, drop)))
  unlink(file.path(path, drop), recursive = TRUE)
  path
}
