# Local Sources in tests: the example Store Releases bundled with opengwasr, and the skip.
#
# `inst/releases/` holds four of ogquery's contract releases (format 0.3.0; SOURCE there names the
# commit). Their store_id, release_id and format_version are written in the tests as literals, read
# from each manifest.json by eye, never through the code under test:
#   dense   s             r             0.3.0
#   ragged  residual-rag  comp          0.3.0
#   hyb     hyb           v1            0.3.0
#   hyb-c   hyb           v1-completed  0.3.0

example_releases <- function(...) {
  file.path(normalizePath(system.file("releases", package = "opengwasr", mustWork = TRUE)), c(...))
}

# Skip unless this session can read Store Releases: never on CRAN, and only with a Python the run
# chose itself (`RETICULATE_PYTHON`), so a test never sets one up with uv or touches the network.
skip_if_no_local <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("reticulate")
  if (!nzchar(Sys.getenv("RETICULATE_PYTHON"))) {
    testthat::skip("RETICULATE_PYTHON isn't set; local tests never set Python up themselves.")
  }
  found <- tryCatch(local_ogquery(), opengwasr_local_unavailable = function(e) NULL)
  if (is.null(found)) testthat::skip("The Python in RETICULATE_PYTHON has no ogquery.")
  invisible(found)
}

# A copy of an example Store Release in a temporary directory, its manifest's format_version set to
# `format_version` (the one edit, made on the JSON text so nothing else changes).
release_with_format <- function(format_version, from = "dense", env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  file.copy(example_releases(from), dir, recursive = TRUE)
  path <- file.path(dir, from)
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
# config of it (or an error, when no Python starts), and what importing ogquery gives (or an
# error). Returns an environment counting the calls of py_require().
mock_python <- function(started = FALSE,
                        config = list(python = "/opt/py/bin/python", forced = "PATH", ephemeral = FALSE),
                        import = list(), env = parent.frame()) {
  local_fresh_python(env)
  withr::local_envvar(c(RETICULATE_PYTHON = NA, RETICULATE_PYTHON_ENV = NA, VIRTUAL_ENV = NA,
                        RETICULATE_USE_MANAGED_VENV = NA), .local_envir = env)
  calls <- new.env()
  calls$required <- 0
  testthat::local_mocked_bindings(
    local_reticulate_installed = function() TRUE,
    local_py_started = function() started,
    local_py_config = function() if (inherits(config, "condition")) stop(config) else config,
    local_py_require = function() calls$required <- calls$required + 1,
    local_import_ogquery = function() if (inherits(import, "condition")) stop(import) else import,
    local_ogquery_versions = function(modules) list(ogquery = "0.1.0", reads = "0.3"),
    .env = env
  )
  calls
}

# reticulate's config of its automatic environment, as reticulate 1.47 reports it.
automatic_python <- list(python = "/home/me/.cache/R/reticulate/uv/env/bin/python", forced = "py_require()",
                         ephemeral = TRUE)
