# Running each acceptance test against both Data APIs.
#
# `test_api()` runs its code against the fake always, and against the dev Data API too when
# `.Renviron` provides OPENGWAS_URL and OPENGWAS_DEV_KEY, and the run is not CRAN or CI. Inside the
# code, `src` is the Source and `backend` is "fake" or "live". Error paths the dev API can't
# produce on demand use `live = FALSE`.

live_available <- function() {
  nzchar(Sys.getenv("OPENGWAS_URL")) && nzchar(Sys.getenv("OPENGWAS_DEV_KEY")) &&
    identical(Sys.getenv("NOT_CRAN"), "true") && !nzchar(Sys.getenv("CI"))
}

test_api <- function(desc, code, live = TRUE) {
  code <- substitute(code)
  env <- parent.frame()
  run <- function(backend) {
    testthat::test_that(paste0(desc, " [", backend, "]"), {
      if (backend == "live") testthat::skip_on_cran()
      src <- if (backend == "fake") fake_source(get("fake", envir = env)) else og_api()
      eval(code, list(src = src, backend = backend), env)
    })
  }
  run("fake")
  if (live && live_available()) run("live")
  invisible()
}
