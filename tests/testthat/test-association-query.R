# The Association-query engine's own guards, which the query tests (test-tophits.R,
# test-associations.R) reach only through the fake.

engine <- getFromNamespace("association_query", "opengwasr")

test_that("the engine refuses to run unless a query method calls it directly", {
  # Provenance would record the helper's arguments as the query's. Refused before any request.
  helper <- function(src, analyses) engine(src, "api/tophits", analyses, FALSE, FALSE, NULL)
  expect_error(helper(NULL, "OGS-00011:GCST003116"), class = "opengwasr_internal")
  query <- function(src, ...) UseMethod("query")
  query.default <- function(src, analyses) helper(src, analyses)
  expect_error(query(NULL, "OGS-00011:GCST003116"), class = "opengwasr_internal")
})

test_that("no Association query method reassigns an argument before provenance reads it", {
  ns <- asNamespace("opengwasr")
  methods <- Filter(function(n) is.function(ns[[n]]) && "association_query" %in% all.names(body(ns[[n]])),
                    ls(ns, all.names = TRUE))
  expect_setequal(methods, c("og_tophits.og_api_source", "og_associations.og_api_source",
                              "og_phewas.og_api_source", "og_range.og_api_source",
                              "og_tophits.og_local_source", "og_associations.og_local_source",
                              "og_phewas.og_local_source", "og_range.og_local_source"))
  # Every name assigned anywhere in an expression (`x <- `, `x$a <- `, `x[i] = `, `x <<- `).
  assigned <- function(x) {
    if (!is.call(x)) return(character())
    op <- x[[1]]
    here <- if (is.symbol(op) && as.character(op) %in% c("<-", "=", "<<-")) {
      lhs <- x[[2]]
      while (is.call(lhs)) lhs <- lhs[[2]]
      as.character(lhs)
    }
    c(here, unlist(lapply(as.list(x)[-1], assigned)))
  }
  expect_equal(assigned(quote({ a <- 1; b$c[2] = 3; f(d <<- 4) })), c("a", "b", "d"))
  for (m in methods) {
    expect_equal(intersect(assigned(body(ns[[m]])), names(formals(ns[[m]]))), character(), info = m)
  }
})

test_that("a Job's total and truncated must fit the rows read", {
  check <- getFromNamespace("check_job_metadata", "opengwasr")
  job <- function(total, truncated) list(job_id = "j", total = total, truncated = truncated)
  expect_silent(check(job(5, FALSE), 5))
  expect_silent(check(job(9, TRUE), 5))     # `limit` cut 4 off
  expect_silent(check(job(0L, FALSE), 0))
  for (bad in list(job(4, FALSE),            # fewer than were read
                   job(4, TRUE),
                   job(6, FALSE),            # more than were read, yet nothing cut off
                   job(NULL, FALSE), job(5, NULL), job(NA, FALSE), job(5, NA),
                   job(5.5, TRUE), job(-1, TRUE), job("5", FALSE), job(c(5, 5), FALSE))) {
    expect_error(check(bad, 5), class = "opengwasr_job_invalid")
  }
})

test_that("the query engines share one scaffold, and a contribution may follow another's dimension", {
  ns <- asNamespace("opengwasr")
  for (engine in c("association_query", "ld_query", "lookup_query")) {
    called <- all.names(body(ns[[engine]]))
    expect_true("engine_query" %in% called, info = engine)
    expect_false(any(c("run_query", "query_plan", "attach_provenance", "local_query", "method_arguments") %in% called), info = engine)
  }
  body_of <- getFromNamespace("request_body", "opengwasr")
  contributions <- list(scope = list(dim = TRUE, body = function(chunk) list(analyses = as.list(chunk), releases = "r")),
                        clump = list(along = "scope", body = function(chunk) list(clump = list(of = chunk))))
  expect_equal(body_of(contributions, list(scope = 2L), list(limit = 5L), FALSE),
               list(analyses = list(2L), limit = 5L, clump = list(of = 2L), observed_only = FALSE, releases = "r"))
})
