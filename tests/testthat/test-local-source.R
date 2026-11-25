# The Local Source (#31): og_local(gwas) over ogquery, finding Python, and og_local_check().
#
# Reads use the example Store Releases bundled in inst/stores (helper-local.R) and skip without a
# Python that has ogquery. The Python boundary (local_* in R/local-source.R) is mocked at its
# primitives (mock_python()) for the paths with no reticulate, no Python or no ogquery, and to
# drive the rule for which Python is chosen. Tests assert on classes and fields, never on
# opengwasr's own message text.

test_that("og_local() opens the Store Releases it is given, each named by its manifest", {
  skip_if_no_local()
  src <- local_source_of()
  expect_s3_class(src, "og_local_source")
  expect_equal(src$releases$store_id, c("s", "residual-rag", "hyb"))
  expect_equal(src$releases$release_id, c("r", "comp", "v1"))
  expect_equal(src$releases$format_version, rep("0.3.0", 3))
  expect_equal(src$releases$path, example_releases("dense", "ragged", "hyb"))

  # print() and format() describe its Store Releases and their paths.
  out <- format(src)
  expect_equal(out[[1]], "<og_local_source>")
  for (i in 1:3) {
    expect_true(any(grepl(paste0(src$releases$store_id[[i]], "/", src$releases$release_id[[i]]), out, fixed = TRUE)))
    expect_true(any(grepl(src$releases$path[[i]], out, fixed = TRUE)))
  }
  expect_identical(expect_output(print(src)), src)
})

test_that("a Local Source keeps the opened Store Release for the queries that follow", {
  skip_if_no_local()
  src <- local_source_of()
  release <- local_release(src, "hyb")
  expect_identical(local_release(src, "hyb"), release)
})

test_that("og_local() refuses two Store Releases of one GWAS Store", {
  skip_if_no_local()
  e <- expect_error(og_local(example_releases("dense", "hyb", "hyb-c")),
                    class = "opengwasr_duplicate_store")
  expect_s3_class(e, "opengwasr_refusal")
  expect_equal(e$store_id, "hyb")
  expect_equal(e$releases, c("hyb/v1", "hyb/v1-completed"))
  expect_equal(e$paths, example_releases("hyb", "hyb-c"))
})

# GWAS Store directories (#38) -------------------------------------------------------------------

test_that("og_local() given a GWAS Store directory opens its Store Release, as if given that Store Release", {
  skip_if_no_local()
  by_store <- og_local(example_stores("dense", "ragged"))
  by_release <- og_local(example_releases("dense", "ragged"))
  expect_equal(by_store$releases, by_release$releases)
  expect_equal(by_store$releases$path, example_releases("dense", "ragged"))
  expect_equal(og_analyses(by_store), og_analyses(by_release))

  # The two forms mix, one element each.
  mixed <- og_local(c(example_stores("dense"), example_releases("hyb")))
  expect_equal(mixed$releases$store_id, c("s", "hyb"))
  expect_equal(mixed$releases$path, example_releases("dense", "hyb"))
})

test_that("og_local() refuses a GWAS Store directory holding no Store Release, and one holding several", {
  skip_if_no_local()
  none <- withr::local_tempdir()
  dir.create(file.path(none, "work"))
  e <- expect_error(og_local(c(example_stores("dense"), none)), class = "opengwasr_not_store_release")
  expect_equal(e$paths, local_path(none))
  expect_length(e$reasons, 1)

  # hyb holds v1 and v1c, two Store Releases of GWAS Store hyb.
  e <- expect_error(og_local(example_stores("dense", "hyb")), class = "opengwasr_several_releases")
  expect_s3_class(e, "opengwasr_refusal")
  expect_equal(e$directory, rep(example_stores("hyb"), 2))
  expect_equal(e$store_id, c("hyb", "hyb"))
  expect_equal(e$release_id, c("v1", "v1-completed"))
  expect_equal(e$releases, c("hyb/v1", "hyb/v1-completed"))
  expect_equal(e$paths, example_releases("hyb", "hyb-c"))
})

test_that("a GWAS Store directory is searched one level down, never deeper", {
  skip_if_no_local()
  # A Hybrid Store Release one level down is the Store Release, not its Dense Component below it.
  store <- withr::local_tempdir()
  file.copy(example_releases("hyb"), store, recursive = TRUE)
  expect_true(file.exists(file.path(store, "v1", "dense", "manifest.json")))
  src <- og_local(store)
  expect_equal(src$releases$path, local_path(file.path(store, "v1")))
  expect_equal(src$releases$release_id, "v1")

  # Without the Store Release's own manifest, only the Dense Component's is left, two levels down:
  # never found.
  unlink(file.path(store, "v1", "manifest.json"))
  e <- expect_error(og_local(store), class = "opengwasr_not_store_release")
  expect_equal(e$paths, local_path(store))
  check <- og_local_check(store)
  expect_equal(check$releases$problem, "not_release")
  expect_true(is.na(check$releases$release_path))
})

test_that("a GWAS Store directory laid out as this host's resolves to its one Store Release", {
  skip_if_no_local()
  # The layout of /data/opengwasdb/stores/OGS-00011: the Store Release in store.opengwasdb/, earlier
  # ones under replaced/<time>/, and a build's work/, tmp/, admission, records and logs, none with a
  # manifest.json one level down, some with one deeper.
  store <- withr::local_tempdir()
  file.copy(example_releases("dense"), store, recursive = TRUE)
  file.rename(file.path(store, "r"), file.path(store, "store.opengwasdb"))
  for (deeper in c("replaced/20261122T224238Z", "replaced/20261125T035841Z", "work/x", "tmp/partial")) {
    dir.create(file.path(store, deeper), recursive = TRUE)
    file.copy(example_releases("ragged"), file.path(store, deeper), recursive = TRUE)
  }
  for (other in c("store.opengwasdb.admission", "records", "logs")) dir.create(file.path(store, other))
  writeLines("{}", file.path(store, "work", "analyses.exclusions.json"))
  writeLines("{}", file.path(store, "store.opengwasdb.admission", "admission.json"))

  src <- og_local(store)
  expect_equal(src$releases$path, local_path(file.path(store, "store.opengwasdb")))
  expect_equal(src$releases$store_id, "s")
  check <- og_local_check(store)
  expect_equal(check$releases$release_path, local_path(file.path(store, "store.opengwasdb")))
  expect_true(check$releases$readable)
})

test_that("a Store Release directory without its manifest.json is refused, never searched as a GWAS Store", {
  skip_if_no_local()
  # A Hybrid's own manifest gone (a partial download): its Dense Component, dense/, one level down,
  # still has one, and must not be taken for a Store Release.
  broken <- copy_release("hyb-c", environment())
  unlink(file.path(broken, "manifest.json"))
  expect_true(file.exists(file.path(broken, "dense", "manifest.json")))
  e <- expect_error(og_local(broken), class = "opengwasr_not_store_release")
  expect_equal(e$paths, local_path(broken))
  expect_length(e$reasons, 1)
  check <- og_local_check(broken)
  expect_true(is.na(check$releases$release_path))
  expect_equal(check$releases$problem, "not_release")
})

test_that("a hidden directory, such as a build in progress, is never a GWAS Store directory's Store Release", {
  skip_if_no_local()
  store <- withr::local_tempdir()
  file.copy(example_releases("dense"), store, recursive = TRUE)
  partial <- file.path(store, ".store.opengwasdb.partial.tmp.2209388.fb7a8d459875")
  dir.create(partial)
  file.copy(list.files(example_releases("ragged"), full.names = TRUE), partial, recursive = TRUE)
  expect_true(file.exists(file.path(partial, "manifest.json")))
  src <- og_local(store)
  expect_equal(src$releases$path, local_path(file.path(store, "r")))
  expect_equal(og_local_check(store)$releases$release_path, local_path(file.path(store, "r")))
})

test_that("a GWAS Store directory whose Store Release is format 0.2.x is refused by format", {
  skip_if_no_local()
  old <- release_with_format("0.2.3")
  e <- expect_error(og_local(dirname(old)), class = "opengwasr_format_unsupported")
  expect_equal(e$paths, local_path(old))
  expect_equal(e$format_version, "0.2.3")
})

test_that("release_path is NA for a path that doesn't exist or is a file, with Python or without", {
  missing <- file.path(withr::local_tempdir(), "no-such-store")
  file <- withr::local_tempfile()
  writeLines("x", file)
  offline <- local({
    local_fresh_python()
    local_mocked_bindings(local_reticulate_installed = function() FALSE,
                          local_install_reticulate = function() stop("not interactive"))
    og_local_check(c(missing, file))
  })
  expect_equal(offline$releases$release_path, c(NA_character_, NA_character_))

  skip_if_no_local()
  check <- og_local_check(c(missing, file))
  expect_equal(check$releases$release_path, c(NA_character_, NA_character_))
  expect_equal(check$releases$problem, c("not_release", "not_release"))
})

test_that("og_local_check() reports which Store Release each GWAS Store directory resolved to", {
  skip_if_no_local()
  none <- withr::local_tempdir()
  check <- og_local_check(c(example_stores("dense", "hyb"), example_releases("ragged"), none))
  rows <- check$releases
  expect_equal(rows$path, c(example_stores("dense", "hyb"), example_releases("ragged"), local_path(none)))
  expect_equal(rows$release_path, c(example_releases("dense"), NA, example_releases("ragged"), NA))
  expect_equal(rows$store_id, c("s", NA, "residual-rag", NA))
  expect_equal(rows$problem, c(NA, "several", NA, "not_release"))
  expect_equal(rows$readable, c(TRUE, FALSE, TRUE, FALSE))
  expect_false(is.na(rows$reason[[2]]))
  expect_identical(expect_output(print(check)), check)

  # Without Python, the directories are still resolved: that needs no manifest read.
  local_fresh_python()
  local_mocked_bindings(local_reticulate_installed = function() FALSE,
                        local_install_reticulate = function() stop("not interactive"))
  offline <- og_local_check(example_stores("dense", "hyb"))
  expect_equal(offline$releases$release_path, c(example_releases("dense"), NA))
  expect_equal(offline$releases$problem, c("unavailable", "unavailable"))
})

test_that("og_local() refuses a path that isn't a Store Release, and an empty `gwas`", {
  # The argument named is the caller's: og_local()'s `gwas`, og_local_check()'s `path`.
  for (bad in list(character(), NA_character_, 1)) {
    expect_equal(expect_error(og_local(bad), class = "opengwasr_argument")$arg, "gwas")
    expect_equal(expect_error(og_local_check(bad), class = "opengwasr_argument")$arg, "path")
  }
  missing <- file.path(withr::local_tempdir(), "no-such-release")
  e <- expect_error(og_local(missing), class = "opengwasr_not_store_release")
  expect_equal(e$paths, local_path(missing))
  expect_length(e$reasons, 1)

  skip_if_no_local()
  empty <- local_path(withr::local_tempdir())
  e <- expect_error(og_local(c(example_releases("dense"), empty)), class = "opengwasr_not_store_release")
  expect_equal(e$paths, empty)
})

test_that("a manifest ogquery can't parse isn't a Store Release, and the parse error says why", {
  skip_if_no_local()
  broken <- release_with_format("0.3.0")
  writeLines("{ not json", file.path(broken, "manifest.json"))
  e <- expect_error(og_local(broken), class = "opengwasr_not_store_release")
  expect_equal(e$paths, local_path(broken))
  expect_match(e$reasons, "JSONDecodeError", fixed = TRUE)  # Python's own error, carried on
})

test_that("og_local() refuses a Store Release ogquery can't read, naming each one's format", {
  skip_if_no_local()
  old <- release_with_format("0.2.3")
  e <- expect_error(og_local(old), class = "opengwasr_format_unsupported")
  expect_s3_class(e, "opengwasr_refusal")
  expect_equal(e$format_version, "0.2.3")
  expect_equal(e$readable, "0.3")
  expect_equal(e$paths, local_path(old))

  # A series ogquery doesn't implement is refused as its manifest is read, and each Store Release
  # keeps its own version.
  future <- release_with_format("0.4.0")
  e <- expect_error(og_local(c(old, example_releases("ragged"), future)), class = "opengwasr_format_unsupported")
  expect_equal(e$paths, local_path(c(old, future)))
  expect_equal(e$format_version, c("0.2.3", "0.4.0"))
  expect_equal(e$readable, "0.3")
})

test_that("a refused format's version comes from the manifest, whatever ogquery's refusal says", {
  skip_if_no_local()
  future <- release_with_format("0.4.0")
  refuse <- function(message) {
    function(ogquery, path) stop(python_error("ogquery.format.UnsupportedFormatVersion", message))
  }
  # ogquery's wording today, a decoy version in it, and a rewording with none: the version, and the
  # Store Release's identity, are the manifest's each time.
  for (message in c("release declares format_version='0.4.0', a release series this build does not implement",
                    "release declares format_version='9.9.9', a release series this build does not implement",
                    "this release's format is not one we read")) {
    local_mocked_bindings(local_load_manifest = refuse(message))
    e <- expect_error(og_local(future), class = "opengwasr_format_unsupported")
    expect_equal(e$format_version, "0.4.0")
    check <- og_local_check(future)
    expect_equal(check$releases$format_version, "0.4.0")
    expect_equal(check$releases$problem, "format")
    expect_equal(check$releases$store_id, "s")
    expect_equal(check$releases$release_id, "r")
  }

  # So a GWAS Store given twice is still named when one of its Store Releases is refused.
  expect_equal(og_local_check(c(future, example_releases("dense")))$duplicates, "s")

  # NA only when manifest.json has no field to read.
  writeLines("{ not json", file.path(future, "manifest.json"))
  local_mocked_bindings(local_load_manifest = refuse("format_version='0.4.0'"))
  expect_true(is.na(og_local_check(future)$releases$format_version))
})

# Paths: one normaliser, forward slashes on every platform --------------------------------------

test_that("paths are normalised once, with forward slashes, Windows ones included", {
  # Windows, simulated: its separators and drive letters, which don't exist here, so nothing is
  # resolved and only the separators change.
  local_mocked_bindings(on_windows = function() TRUE)
  expect_equal(normalise_path("C:\\hostedtoolcache\\windows\\Python\\3.13.17\\x64\\python.exe", resolve = FALSE),
               "C:/hostedtoolcache/windows/Python/3.13.17/x64/python.exe")
  expect_equal(normalise_path(c("D:\\a\\opengwasr\\releases/hyb-c", "D:/a/opengwasr/releases\\hyb")),
               c("D:/a/opengwasr/releases/hyb-c", "D:/a/opengwasr/releases/hyb"))
  # A missing Store Release is refused under the path as normalised.
  e <- expect_error(og_local("D:\\data\\OGS-00011\\OGS-00011-v2"), class = "opengwasr_not_store_release")
  expect_equal(e$paths, "D:/data/OGS-00011/OGS-00011-v2")
})

test_that("a Windows RETICULATE_PYTHON is the Python reticulate reports, whichever separators each uses", {
  local_mocked_bindings(on_windows = function() TRUE)
  started <- list(python = "C:/hostedtoolcache/windows/Python/3.13.17/x64/python.exe", forced = "RETICULATE_PYTHON",
                  ephemeral = FALSE)
  calls <- mock_python(config = started)
  withr::local_envvar(RETICULATE_PYTHON = "C:\\hostedtoolcache\\windows\\Python\\3.13.17\\x64\\python.exe")
  found <- local_ogquery()
  expect_equal(found$info$python, "C:/hostedtoolcache/windows/Python/3.13.17/x64/python.exe")
  expect_equal(found$info$chosen_by, "RETICULATE_PYTHON")
  expect_equal(calls$required, 0)

  # Reported the same way when reticulate spells it with backslashes too.
  mock_python(config = list(python = "C:\\py\\python.exe", forced = "RETICULATE_PYTHON", ephemeral = FALSE))
  withr::local_envvar(RETICULATE_PYTHON = "C:\\py\\python.exe")
  expect_equal(local_ogquery()$info$python, "C:/py/python.exe")

  # With no `forced`, the paths are compared: the same Python however it is spelt, or its directory.
  started$forced <- NULL
  for (named in c("C:\\hostedtoolcache\\windows\\Python\\3.13.17\\x64\\python.exe",
                  "C:\\hostedtoolcache\\windows\\Python\\3.13.17\\x64")) {
    mock_python(config = started)
    withr::local_envvar(RETICULATE_PYTHON = named)
    expect_equal(local_ogquery()$info$chosen_by, "RETICULATE_PYTHON")
  }
  mock_python(config = started)
  withr::local_envvar(RETICULATE_PYTHON = "C:\\other\\python.exe")
  e <- expect_error(local_ogquery(), class = "opengwasr_local_unavailable")
  expect_equal(e$need, "chosen_python")
  expect_equal(e$python, "C:/hostedtoolcache/windows/Python/3.13.17/x64/python.exe")
})

# Finding Python: the boundary mocked at its primitives ----------------------------------------

test_that("without reticulate, og_local() says what to install", {
  local_fresh_python()
  local_mocked_bindings(local_reticulate_installed = function() FALSE,
                        local_install_reticulate = function() stop("not interactive"))
  e <- expect_error(og_local(example_releases("dense")), class = "opengwasr_local_unavailable")
  expect_false(inherits(e, "opengwasr_refusal"))
  expect_equal(e$need, "reticulate")
  expect_equal(e$install, "install.packages(\"reticulate\")")
})

test_that("with no Python, og_local() says what to set", {
  calls <- mock_python(config = simpleError("Python not found"), import = simpleError("Python not found"))
  e <- expect_error(og_local(example_releases("dense")), class = "opengwasr_local_unavailable")
  expect_equal(calls$required, 1)
  expect_equal(e$need, "python")
  expect_equal(e$setting, "RETICULATE_PYTHON")
})

test_that("a Python without ogquery is refused with how to install it there", {
  calls <- mock_python(ogquery = list(installed = FALSE, pip = TRUE))
  e <- expect_error(og_local(example_releases("dense")), class = "opengwasr_local_unavailable")
  expect_equal(e$need, "ogquery")
  expect_equal(e$python, "/opt/py/bin/python")
  expect_equal(e$install, '/opt/py/bin/python -m pip install "ogquery>=0.1.0,<1"')

  # A Python without pip, such as a uv-made environment: uv installs into it.
  calls <- mock_python(ogquery = list(installed = FALSE, pip = FALSE))
  e <- expect_error(og_local(example_releases("dense")), class = "opengwasr_local_unavailable")
  expect_equal(e$need, "ogquery")
  expect_equal(e$install, 'uv pip install --python /opt/py/bin/python "ogquery>=0.1.0,<1"')

  # ogquery found, but one of the modules opengwasr imports isn't.
  calls <- mock_python(import = python_error("python.builtin.ModuleNotFoundError", "No module named 'ogquery'"))
  e <- expect_error(og_local(example_releases("dense")), class = "opengwasr_local_unavailable")
  expect_equal(e$need, "ogquery")
  expect_equal(e$install, '/opt/py/bin/python -m pip install "ogquery>=0.1.0,<1"')
})

# Which ogquery releases opengwasr reads: 0.1.0 (the oldest tested) up to, not including, 1.0.
# ogquery 0.0.0, an early placeholder on PyPI, has neither a version nor `ogquery.release`.
test_that("ogquery outside >=0.1.0,<1, or without ogquery.release, is refused before any read", {
  user_python <- list(python = "/home/me/venvs/og/bin/python", forced = "RETICULATE_PYTHON", ephemeral = FALSE)
  refused <- function(ogquery, config = user_python) {
    mock_python(config = config, ogquery = ogquery, env = parent.frame())
    if (!isTRUE(config$ephemeral)) withr::local_envvar(RETICULATE_PYTHON = config$python)
    expect_error(og_local(example_releases("dense")), class = "opengwasr_local_unavailable")
  }

  # 0.0.0 as PyPI has it: no ogquery.release, and no version of its own.
  e <- refused(list(installed = TRUE, version = NULL, release = FALSE, pip = TRUE))
  expect_false(inherits(e, "opengwasr_refusal"))
  expect_equal(e$need, "ogquery_version")
  expect_true(is.na(e$found))
  expect_equal(e$required, ">=0.1.0,<1")
  expect_equal(e$python, "/home/me/venvs/og/bin/python")
  expect_equal(e$chosen_by, "RETICULATE_PYTHON")
  expect_equal(e$install, '/home/me/venvs/og/bin/python -m pip install "ogquery>=0.1.0,<1"')

  # 0.0.0, its version read from the installed distribution.
  e <- refused(list(installed = TRUE, version = "0.0.0", release = FALSE, pip = TRUE))
  expect_equal(e$need, "ogquery_version")
  expect_equal(e$found, "0.0.0")

  # Too new, too old, and a version in range that lacks ogquery.release.
  e <- refused(list(installed = TRUE, version = "1.2.0", release = TRUE, pip = TRUE))
  expect_equal(e$need, "ogquery_version")
  expect_equal(e$found, "1.2.0")
  e <- refused(list(installed = TRUE, version = "1.0.0rc1", release = TRUE, pip = TRUE))
  expect_equal(e$found, "1.0.0rc1")
  e <- refused(list(installed = TRUE, version = "0.0.9", release = TRUE, pip = TRUE))
  expect_equal(e$found, "0.0.9")
  e <- refused(list(installed = TRUE, version = "0.2.0", release = FALSE, pip = TRUE))
  expect_equal(e$need, "ogquery_version")
  expect_equal(e$found, "0.2.0")

  # A Python the user chose without pip: uv installs into it.
  e <- refused(list(installed = TRUE, version = "0.0.0", release = FALSE, pip = FALSE))
  expect_equal(e$install, 'uv pip install --python /home/me/venvs/og/bin/python "ogquery>=0.1.0,<1"')

  # reticulate's automatic environment: clear uv's cached ogquery (as reticulate's own uv sees it).
  local_mocked_bindings(local_uv_cache_clean = function() "/home/me/.cache/R/reticulate/uv/bin/uv cache clean ogquery")
  e <- refused(list(installed = TRUE, version = "0.0.0", release = FALSE, pip = FALSE), config = automatic_python)
  expect_equal(e$need, "ogquery_version")
  expect_equal(e$chosen_by, "automatic")
  expect_equal(e$found, "0.0.0")
  expect_equal(e$install, "/home/me/.cache/R/reticulate/uv/bin/uv cache clean ogquery")
})

test_that("ogquery 0.1.0 and 0.3.0 are read", {
  for (version in c("0.1.0", "0.3.0", "0.3.1.dev2")) {
    mock_python(ogquery = list(installed = TRUE, version = version, release = TRUE, pip = TRUE))
    expect_equal(local_ogquery()$info$ogquery, version)
  }
})

test_that("the automatic setup asks py_require() for a supported ogquery", {
  calls <- mock_python(config = automatic_python)
  local_ogquery()
  expect_equal(calls$packages, "ogquery>=0.1.0,<1")
})

test_that("the uv cache advice names the uv reticulate uses, and its cache", {
  withr::local_envvar(RETICULATE_UV = "/opt/uv/bin/uv")
  expect_equal(local_uv_cache_clean(), "/opt/uv/bin/uv cache clean ogquery")

  # reticulate's own uv, which keeps its cache beside it.
  withr::local_envvar(RETICULATE_UV = "managed")
  cache <- tools::R_user_dir("reticulate", "cache")
  expect_equal(local_uv_cache_clean(),
               paste0(normalise_path(file.path(cache, "uv", "bin", if (on_windows()) "uv.exe" else "uv"),
                                     resolve = FALSE),
                      " cache clean ogquery --cache-dir ",
                      normalise_path(file.path(cache, "uv", "cache"), resolve = FALSE)))
})

# Which Python, and whether py_require() declares ogquery, setting by setting: reticulate's
# settings before Python starts, or the config of a Python already running.
test_that("a Python the user chose is used, and py_require() isn't", {
  calls <- mock_python(config = list(python = "/home/me/venvs/ogquery/bin/python", forced = "RETICULATE_PYTHON", ephemeral = FALSE))
  withr::local_envvar(RETICULATE_PYTHON = "/home/me/venvs/ogquery/bin/python")
  found <- local_ogquery()
  expect_equal(calls$required, 0)
  expect_equal(found$info$python, "/home/me/venvs/ogquery/bin/python")
  expect_equal(found$info$chosen_by, "RETICULATE_PYTHON")
})

test_that("RETICULATE_PYTHON=managed asks for the automatic Python", {
  calls <- mock_python(config = automatic_python)
  withr::local_envvar(RETICULATE_PYTHON = "managed")
  found <- local_ogquery()
  expect_equal(calls$required, 1)
  expect_equal(found$info$chosen_by, "automatic")
})

test_that("RETICULATE_PYTHON may name a directory, as reticulate allows", {
  calls <- mock_python(config = list(python = "/home/me/venvs/og/bin/python", forced = "RETICULATE_PYTHON", ephemeral = FALSE))
  withr::local_envvar(RETICULATE_PYTHON = "/home/me/venvs/og/bin")
  found <- local_ogquery()
  expect_equal(calls$required, 0)
  expect_equal(found$info$chosen_by, "RETICULATE_PYTHON")
})

test_that("RETICULATE_USE_MANAGED_VENV asks for the automatic Python, over an activated virtualenv", {
  calls <- mock_python(config = automatic_python)
  withr::local_envvar(RETICULATE_USE_MANAGED_VENV = "true", VIRTUAL_ENV = "/venv")
  found <- local_ogquery()
  expect_equal(calls$required, 1)
  expect_equal(found$info$chosen_by, "automatic")
})

test_that("RETICULATE_PYTHON_ENV and an activated virtualenv are the user's choice", {
  calls <- mock_python(config = list(python = "/envs/og/bin/python", forced = "RETICULATE_PYTHON_ENV", ephemeral = FALSE))
  withr::local_envvar(RETICULATE_PYTHON_ENV = "og")
  expect_equal(local_ogquery()$info$chosen_by, "RETICULATE_PYTHON_ENV")
  expect_equal(calls$required, 0)

  calls <- mock_python(config = list(python = "/venv/bin/python", forced = "VIRTUAL_ENV", ephemeral = FALSE))
  withr::local_envvar(VIRTUAL_ENV = "/venv")
  expect_equal(local_ogquery()$info$chosen_by, "VIRTUAL_ENV")
  expect_equal(calls$required, 0)
})

test_that("a Python already started is used as it is", {
  calls <- mock_python(started = TRUE, config = list(python = "/usr/bin/python3", forced = "PATH", ephemeral = FALSE))
  found <- local_ogquery()
  expect_equal(calls$required, 0)
  expect_equal(found$info$python, "/usr/bin/python3")
  expect_equal(found$info$chosen_by, "reticulate")

  # Started by the user's setting: named by it.
  calls <- mock_python(started = TRUE, config = list(python = "/x/bin/python", forced = "RETICULATE_PYTHON", ephemeral = FALSE))
  expect_equal(local_ogquery()$info$chosen_by, "RETICULATE_PYTHON")

  # reticulate's automatic Python, already running: ogquery is added to it.
  calls <- mock_python(started = TRUE, config = automatic_python)
  expect_equal(local_ogquery()$info$chosen_by, "automatic")
  expect_equal(calls$required, 1)
})

test_that("with no choice, py_require() declares ogquery", {
  calls <- mock_python(config = automatic_python)
  found <- local_ogquery()
  expect_equal(calls$required, 1)
  expect_equal(found$info$python, automatic_python$python)

  # A use_virtualenv(), use_condaenv() or use_python() call can't be seen before Python starts
  # (the documented gap); reticulate ranks it first all the same, and its `forced` names it after.
  calls <- mock_python(config = list(python = "/envs/og/bin/python", forced = "use_python() function", ephemeral = FALSE))
  expect_equal(local_ogquery()$info$chosen_by, "use_python")
  expect_equal(calls$required, 1)

  # Over an activated virtualenv, as reticulate ranks it: the user's choice, named by `forced`.
  calls <- mock_python(config = list(python = "/envs/og/bin/python", forced = "use_python() function", ephemeral = FALSE))
  withr::local_envvar(VIRTUAL_ENV = "/venv")
  expect_equal(local_ogquery()$info$chosen_by, "use_python")
  expect_equal(calls$required, 0)
})

test_that("a Python the user chose without ogquery is refused, never swapped for the automatic one", {
  calls <- mock_python(config = list(python = "/home/me/venvs/bare/bin/python", forced = "RETICULATE_PYTHON", ephemeral = FALSE),
                       ogquery = list(installed = FALSE, pip = TRUE))
  withr::local_envvar(RETICULATE_PYTHON = "/home/me/venvs/bare/bin/python")
  e <- expect_error(og_local(example_releases("dense")), class = "opengwasr_local_unavailable")
  expect_equal(calls$required, 0)
  expect_equal(e$need, "ogquery")
  expect_equal(e$chosen_by, "RETICULATE_PYTHON")
  expect_equal(e$install, '/home/me/venvs/bare/bin/python -m pip install "ogquery>=0.1.0,<1"')
})

test_that("if reticulate starts a Python other than the one the user chose, og_local() refuses", {
  # A different Python from the one named.
  mock_python(config = list(python = "/usr/bin/python3", forced = "PATH", ephemeral = FALSE))
  withr::local_envvar(RETICULATE_PYTHON = "/home/me/venvs/broken/bin/python")
  e <- expect_error(og_local(example_releases("dense")), class = "opengwasr_local_unavailable")
  expect_equal(e$need, "chosen_python")
  expect_equal(e$python, "/usr/bin/python3")
  expect_equal(e$setting, "RETICULATE_PYTHON")

  # The automatic one, after an activated virtualenv it couldn't start.
  mock_python(config = automatic_python)
  withr::local_envvar(VIRTUAL_ENV = "/home/me/venvs/broken")
  e <- expect_error(og_local(example_releases("dense")), class = "opengwasr_local_unavailable")
  expect_equal(e$need, "chosen_python")
  expect_equal(e$chosen_by, "VIRTUAL_ENV")

  # One reticulate found itself, after an environment it couldn't start.
  mock_python(config = list(python = "/home/me/.virtualenvs/r-reticulate/bin/python", forced = NULL, ephemeral = FALSE))
  withr::local_envvar(RETICULATE_PYTHON_ENV = "broken")
  e <- expect_error(og_local(example_releases("dense")), class = "opengwasr_local_unavailable")
  expect_equal(e$need, "chosen_python")

  # A choice reticulate ranks lower, after the one seen first: RETICULATE_PYTHON_ENV fell back to
  # the activated virtualenv.
  mock_python(config = list(python = "/venv/bin/python", forced = "VIRTUAL_ENV", ephemeral = FALSE))
  withr::local_envvar(RETICULATE_PYTHON_ENV = "broken", VIRTUAL_ENV = "/venv")
  e <- expect_error(og_local(example_releases("dense")), class = "opengwasr_local_unavailable")
  expect_equal(e$need, "chosen_python")
  expect_equal(e$chosen_by, "RETICULATE_PYTHON_ENV")
  expect_equal(e$started_by, "VIRTUAL_ENV")
})

test_that("with no `forced`, the Python started is compared with the one named", {
  mock_python(config = list(python = "/home/me/venvs/og/bin/python", forced = NULL, ephemeral = FALSE))
  withr::local_envvar(RETICULATE_PYTHON = "/home/me/venvs/og/bin/python")
  expect_equal(local_ogquery()$info$chosen_by, "RETICULATE_PYTHON")
})

test_that("a stale handle is opened again the way og_local() opens it, probe and all", {
  local_fresh_python()
  local_state$ogquery <- list(info = list(python = "/p", chosen_by = "RETICULATE_PYTHON", ogquery = "0.1.0", reads = "0.3"))
  dir <- local_path(withr::local_tempdir())
  src <- unopened_source(data.frame(store_id = "s", release_id = "r", format_version = "0.3.0", path = dir))
  assign(dir, "old", envir = src$handles)
  probed <- character()
  local_mocked_bindings(
    local_is_stale = function(object) identical(object, "old"),
    local_load_manifest = function(ogquery, path) list(store_id = "s", release_id = "r", format_version = "0.3.0"),
    local_open_release = function(ogquery, path) "new",
    local_probe_release = function(release) probed <<- c(probed, release)
  )
  expect_equal(local_release(src, "s"), "new")
  expect_equal(probed, "new")
  expect_equal(local_release(src, "s"), "new")
  expect_equal(probed, "new")

  assign(dir, "old", envir = src$handles)
  local_mocked_bindings(
    local_load_manifest = function(ogquery, path) list(store_id = "s", release_id = "r", format_version = "0.2.3"),
    local_probe_release = function(release) {
      stop(python_error(c("ogquery.release.VariantTableReaderRequired", "python.builtin.ValueError"), "0.2.3"))
    }
  )
  e <- expect_error(local_release(src, "s"), class = "opengwasr_format_unsupported")
  expect_equal(e$format_version, "0.2.3")
})

# og_local_check() ----------------------------------------------------------------------------

test_that("og_local_check() reports the Python, the versions and every Store Release, raising nothing", {
  skip_if_no_local()
  old <- release_with_format("0.2.3")
  empty <- withr::local_tempdir()
  check <- og_local_check(c(example_releases("dense", "hyb"), old, empty))
  expect_s3_class(check, "og_local_check")
  expect_equal(check$python, normalise_path(Sys.getenv("RETICULATE_PYTHON"), resolve = FALSE))
  expect_equal(check$chosen_by, "RETICULATE_PYTHON")
  expect_equal(check$reticulate, as.character(utils::packageVersion("reticulate")))
  expect_match(check$ogquery, "^[0-9]+\\.[0-9]+")
  expect_equal(check$reads, "0.3")
  expect_null(check$unavailable)

  rows <- check$releases
  expect_equal(nrow(rows), 4)
  expect_equal(rows$path, local_path(c(example_releases("dense", "hyb"), old, empty)))
  expect_equal(rows$store_id, c("s", "hyb", "s", NA))
  expect_equal(rows$release_id, c("r", "v1", "r", NA))
  expect_equal(rows$format_version, c("0.3.0", "0.3.0", "0.2.3", NA))
  expect_equal(rows$readable, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(rows$problem, c(NA, NA, "format", "not_release"))
  expect_equal(is.na(rows$reason), c(TRUE, TRUE, FALSE, FALSE))

  expect_equal(format(check)[[1]], "<og_local_check>")
  expect_identical(expect_output(print(check)), check)
})

test_that("og_local_check() takes a Local Source, and names two Store Releases of one GWAS Store", {
  skip_if_no_local()
  check <- og_local_check(local_source_of())
  expect_equal(check$releases$store_id, c("s", "residual-rag", "hyb"))
  expect_true(all(check$releases$readable))
  expect_length(check$duplicates, 0)
  expect_length(check$problems, 0)

  both <- og_local_check(example_releases("hyb", "hyb-c"))
  expect_true(all(both$releases$readable))
  expect_equal(both$duplicates, "hyb")
  expect_length(both$problems, 1)
})

test_that("og_local_check() reports a missing reticulate instead of raising it", {
  local_fresh_python()
  local_mocked_bindings(local_reticulate_installed = function() FALSE,
                        local_install_reticulate = function() stop("not interactive"))
  check <- og_local_check(example_releases("dense"))
  expect_s3_class(check, "og_local_check")
  expect_true(is.na(check$python))
  expect_true(is.na(check$reticulate))
  expect_s3_class(check$unavailable, "opengwasr_local_unavailable")
  expect_equal(check$unavailable$need, "reticulate")
  expect_false(check$releases$readable)
  expect_equal(check$releases$problem, "unavailable")
  expect_identical(expect_output(print(check)), check)
})

test_that("og_local_check() reports the ogquery version found and whether opengwasr reads it, raising nothing", {
  empty <- withr::local_tempdir()
  mock_python(ogquery = list(installed = TRUE, version = "0.0.0", release = FALSE, pip = TRUE))
  check <- og_local_check(empty)
  expect_equal(check$ogquery, "0.0.0")
  expect_false(check$ogquery_in_range)
  expect_equal(check$ogquery_required, ">=0.1.0,<1")
  expect_equal(check$unavailable$need, "ogquery_version")
  expect_equal(check$releases$problem, "unavailable")
  expect_identical(expect_output(print(check)), check)

  mock_python(ogquery = list(installed = TRUE, version = "0.3.0", release = TRUE, pip = TRUE))
  check <- og_local_check(empty)
  expect_equal(check$ogquery, "0.3.0")
  expect_true(check$ogquery_in_range)
  expect_null(check$unavailable)

  # No ogquery at all: nothing to range.
  mock_python(ogquery = list(installed = FALSE, pip = TRUE))
  check <- og_local_check(empty)
  expect_true(is.na(check$ogquery))
  expect_true(is.na(check$ogquery_in_range))
})

test_that("og_local_check() refuses only bad arguments", {
  expect_error(og_local_check(character()), class = "opengwasr_argument")
  expect_error(og_local_check(list()), class = "opengwasr_argument")
})

# The API Source never needs Python ------------------------------------------------------------

test_that("anything without a Local Source method refuses a Local Source", {
  src <- unopened_source(data.frame())
  # Every query verb has a Local Source method (#34 gave og_tophits(), og_clump() and og_ld_matrix()
  # theirs); the one Source check still refuses a Local Source wherever one reaches it.
  e <- expect_error(check_source(src), class = "opengwasr_local_unsupported")
  expect_s3_class(e, "opengwasr_refusal")
  expect_error(og_tophits("x", "s:a"), class = "opengwasr_argument")
})

test_that("an API query never reaches the Python boundary", {
  fake <- local_fake_api(n_synthetic = 1)
  withr::local_options(opengwasr.poll_cap = 0.05, opengwasr.progress = FALSE)
  touched <- character()
  touch <- function(name) function(...) {
    touched <<- c(touched, name)
    stop("the Python boundary was reached")
  }
  local_mocked_bindings(local_reticulate_installed = touch("installed"), local_py_started = touch("started"),
                        local_py_config = touch("config"), local_py_require = touch("require"),
                        local_import_ogquery = touch("import"), local_load_manifest = touch("manifest"),
                        local_open_release = touch("open"), local_probe_release = touch("probe"),
                        local_declared_manifest = touch("declared"))
  loaded <- "reticulate" %in% loadedNamespaces()
  top <- og_tophits(fake_source(fake), "OGS-00011:GCST003116", clump = FALSE)
  expect_gt(nrow(top), 0)
  expect_length(touched, 0)
  expect_identical("reticulate" %in% loadedNamespaces(), loaded)
})

test_that("loading opengwasr and making an API Source doesn't load reticulate", {
  skip_on_cran()
  # A fresh R process, which has loaded nothing yet: the package as this run has it (an installed
  # copy under R CMD check, the source tree under devtools::test()).
  path <- getNamespaceInfo("opengwasr", "path")
  load <- if (file.exists(file.path(path, "R", "local-source.R"))) {
    sprintf("pkgload::load_all('%s', helpers = FALSE, quiet = TRUE)", path)
  } else {
    sprintf("library(opengwasr, lib.loc = '%s')", dirname(path))
  }
  code <- paste0(load, "; src <- og_api(url = 'http://127.0.0.1:9'); ",
                 "cat('reticulate' %in% loadedNamespaces())")
  out <- withr::with_envvar(c(OPENGWAS_DEV_KEY = "k", OPENGWAS_URL = "http://127.0.0.1:9"),
                            system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(code)),
                                    stdout = TRUE, stderr = FALSE))
  expect_equal(utils::tail(out, 1), "FALSE")
})
