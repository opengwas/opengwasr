# Parity (#35): for each query of a golden set, the API Source and a Local Source return the same
# data frame, and each returns what the real Data API would return.
#
# The golden set is fixtures/parity/queries.json: each query is a call (`verb`, `args`) over a
# source set of the example Store Releases (`source`), and the Data API requests the API Source
# makes for it (`requests`, as opengwasr sends them). fixtures/parity/generate.py answers every
# request the way the real Data API would, from ogdb-api's own code, reading the releases through
# ogquery and never through opengwasr (answers.json). The fake Data API serves those answers
# (`local_fake_api(parity = <source set>)`), so the API Source runs its usual transport, Job and
# shaping path over them.
#
# Three comparisons per golden query:
#  * the API Source against the generated answer (runs everywhere: no Python needed);
#  * the Local Source against the generated answer;
#  * the API Source against the Local Source: identical data frames (columns, types, row order and
#    values), the same warnings, the same refusal. Provenance is compared too, less what can't
#    agree: `retrieved` (when each answer was read), `job_ids` (a Local Source runs no Job) and
#    `cost_estimate` (the API's estimate per Job; a Local Source has none, NA).
# When the last fails, the first two say which side is off.
#
# Known differences ------------------------------------------------------------------------------
# What the two Sources, or the real API and the fake, still do differently, each left out of the
# golden set or asserted below:
#  * Non-finite statistics. The real API writes NaN and Infinity into its NDJSON, which isn't JSON
#    (opengwas/ogdb-api#193), so its API Source fails on a missing Association or an infinite SE.
#    The fake (and answers.json) write "NaN" and "Inf", as jsonlite does, so the golden set keeps
#    the missing Associations of residual-rag/comp; until #193 they pass only through the fake.
#  * Infinite SE. s/r holds 1:1000:A:G with an infinite SE. The API's pinned reader (opengwasdb
#    42850d8) computes a finite log10_p for it from z alone; ogquery, which the Local Source and the
#    generator read, gives NaN (until opengwas/ogdb-api#192 moves the API onto ogquery). Left out
#    of the golden set: tophits-truncated's limit of 5 stops above it. Asserted below for the Local
#    Source.
#  * Missing Top-Hit Index. The API answers a release without one with an untyped 501
#    (`opengwasr_api_error`); a Local Source refuses with `opengwasr_local_unsupported`
#    (opengwas/opengwasr#33). The example releases all have one, so it is left out; #33's tests
#    cover the local refusal.
#  * Ragged release without its by-variant index. The API serves its Associations and fails in the
#    reader; a Local Source refuses up front (opengwasr_local_unsupported, opengwas/opengwasr#33).
#    Left out: the example releases have their index; #33's tests cover the local refusal.
#  * og_analyses(). The catalogue's own columns (status, listed, trait_class, ancestry_category,
#    label_from_source) are NA on a Local Source (#32's ruling): compared only on the shared
#    columns, and those five asserted NA. A Local Source's further analyses.tsv columns follow
#    the API's and aren't compared.

fake_three <- local_fake_api(n_synthetic = 0, parity = "three")
fake_completed <- local_fake_api(n_synthetic = 0, parity = "completed")
parity_fakes <- list(three = fake_three, completed = fake_completed)

parity_dir <- fixture_file("parity")
# Each query's arguments as R values (simplified), its requests as parsed JSON (not simplified),
# as the fake receives them.
parity_queries <- local({
  file <- file.path(parity_dir, "queries.json")
  queries <- jsonlite::read_json(file, simplifyVector = TRUE, simplifyDataFrame = FALSE)$queries
  raw <- jsonlite::read_json(file, simplifyVector = FALSE)$queries
  Map(function(q, r) { q$requests <- r$requests; q }, queries, raw)
})
parity_answers <- jsonlite::read_json(file.path(parity_dir, "answers.json"), simplifyVector = FALSE)
names(parity_queries) <- vapply(parity_queries, `[[`, "", "name")

# The columns og_analyses() gives that only the catalogue makes (NA on a Local Source).
catalogue_only <- c("status", "listed", "trait_class", "ancestry_category", "label_from_source")

# Running a golden query --------------------------------------------------------------------------

# A golden query on `src`: its value (or the error it raised) and the classes of the warnings it gave.
parity_run <- function(query, src) {
  warnings <- character()
  value <- withCallingHandlers(
    tryCatch(do.call(query$verb, c(list(src), query$args)), error = function(e) e),
    warning = function(w) {
      warnings <<- c(warnings, class(w)[[1]])
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warnings)
}

# Each golden query once on the API Source (its fake), kept for the comparison with the Local
# Source: run again, the fake would reattach to the same Job (`existing`).
api_runs <- new.env()
api_run <- function(query) {
  if (is.null(api_runs[[query$name]])) {
    api_runs[[query$name]] <- parity_run(query, fake_source(parity_fakes[[query$source]]))
  }
  api_runs[[query$name]]
}

parity_sources <- jsonlite::read_json(file.path(parity_dir, "queries.json"), simplifyVector = TRUE)$sources
local_run <- function(query) parity_run(query, local_source_of(parity_sources[[query$source]]))

# The generated answer to a query's last request (the Job whose rows the result holds), from
# `answers` (answers.json, parsed).
generated_answer <- function(query, answers = parity_answers) {
  last <- query$requests[[length(query$requests)]]
  key <- server_env$parity_key(last$endpoint, last$body)
  for (a in answers$answers) if (identical(server_env$parity_key(a$endpoint, a$body), key)) return(a)
  stop("answers.json has no answer for query ", query$name, "; run fixtures/parity/generate.py.")
}

# What is compared of a value: a data frame with its provenance, less the fields that can't agree.
comparable <- function(value) {
  if (inherits(value, "condition")) {
    return(list(class = class(value), api_error = value$api_error))
  }
  prov <- attr(value, "og_provenance", exact = TRUE)
  if (!is.null(prov)) {
    prov[c("retrieved", "job_ids", "cost_estimate")] <- NULL
    attr(value, "og_provenance") <- prov
  }
  value
}

# Each Source against the generated answer ----------------------------------------------------------

# The generated rows as the columns a Source's result has for them: rsID and EAF "." (or null) as
# NA, non-finite numbers as numbers, and pval as 10^log10_p.
generated_frame <- function(rows, fields) {
  text <- function(name) vapply(rows, function(r) {
    v <- r[[name]]
    if (is.null(v) || identical(v, ".")) NA_character_ else as.character(v)
  }, "")
  number <- function(name) vapply(rows, function(r) {
    v <- r[[name]]
    if (is.null(v) || identical(v, ".")) NA_real_ else as.numeric(v)
  }, 0)
  out <- list()
  for (name in fields) {
    out[[name]] <- switch(name,
      position = as.integer(number("position")),
      eaf = , z = , se = number(name),
      pval = 10^number("log10_p"),
      text(name))
  }
  as.data.frame(out, stringsAsFactors = FALSE)
}

# How `result` (one Source's value for `query`) differs from the generated answer, as messages
# (none when it doesn't): a different refusal, or different rows (found by key, with every field's
# value), or, for verbs that keep the API's order (top hits, PheWAS, variant lookups), a different
# order, or a different total or truncation.
generated_differences <- function(result, query, answers = parity_answers) {
  answer <- generated_answer(query, answers)
  value <- result$value
  if (!is.null(answer$error)) {
    code <- answer$error$body$error
    if (!inherits(value, paste0("opengwasr_", code)) || !identical(value$api_error, code)) {
      return(paste0("expected the refusal `", code, "`, got ", class(value)[[1]]))
    }
    return(character())
  }
  if (inherits(value, "condition")) return(paste("expected rows, got", class(value)[[1]], conditionMessage(value)))
  lookup <- query$verb %in% c("og_variants_rsid", "og_variants_chrpos")
  fields <- if (lookup) {
    c("store_id", "release_id", "alid", "chromosome", "position", "effect_allele", "other_allele", "rsid")
  } else {
    c("store_id", "release_id", "analysis_id", "alid", "chromosome", "position", "effect_allele", "other_allele",
      "rsid", "eaf", "z", "se", "pval", "association_status")
  }
  expected <- generated_frame(answer$rows, fields)
  key <- function(x) paste(x$store_id, x$release_id, if (!lookup) x$analysis_id, x$alid)
  at <- match(key(expected), key(value))
  if (nrow(value) != nrow(expected) || anyNA(at)) {
    return(paste0(nrow(value), " rows, expected ", nrow(expected), "; missing: ",
                  paste(key(expected)[is.na(at)], collapse = ", ")))
  }
  found <- character()
  for (name in fields) {
    got <- value[[name]][at]
    if (!identical(got, expected[[name]])) {
      i <- which(!mapply(identical, got, expected[[name]]))[[1]]
      found <- c(found, paste0("`", name, "` of ", key(expected)[[i]], ": ", format(got[[i]], digits = 17),
                               ", expected ", format(expected[[name]][[i]], digits = 17)))
    }
  }
  if ((query$verb %in% c("og_tophits", "og_phewas") || lookup) && !identical(at, seq_len(nrow(expected)))) {
    found <- c(found, "rows in another order")
  }
  if (!is.null(answer$metadata$total)) {
    prov <- og_provenance(value)
    if (!identical(prov$total, as.numeric(answer$metadata$total))) found <- c(found, paste("total", prov$total))
    if (!identical(prov$truncated, answer$metadata$truncated)) found <- c(found, paste("truncated", prov$truncated))
  }
  found
}

# og_analyses(): the generated catalogue rows of the Analyses the result names, on the shared
# columns, in the result's order.
expect_generated_analyses <- function(result, query) {
  value <- result$value
  testthat::expect_s3_class(value, "data.frame")
  rows <- parity_answers$catalogue
  ids <- vapply(rows, `[[`, "", "id")
  releases <- vapply(rows, `[[`, "", "release_id")
  at <- match(paste(value$id, value$release_id), paste(ids, releases))
  testthat::expect_false(anyNA(at))
  shared <- setdiff(names(rows[[1]]), catalogue_only)
  for (name in shared) {
    expected <- vapply(rows[at], function(r) if (is.null(r[[name]])) NA_character_ else as.character(r[[name]]), "")
    got <- value[[name]]
    testthat::expect_identical(ifelse(is.na(got), NA_character_, as.character(got)), expected, info = name)
  }
}

# The tests ------------------------------------------------------------------------------------------

test_that("the golden set covers every verb, with refusals, truncation and pins", {
  verbs <- table(vapply(parity_queries, `[[`, "", "verb"))
  expect_setequal(names(verbs), c("og_analyses", "og_variants_rsid", "og_variants_chrpos", "og_associations",
                                  "og_tophits", "og_phewas", "og_range"))
  # Every request of every query has a generated answer.
  keys <- vapply(parity_answers$answers, function(a) server_env$parity_key(a$endpoint, a$body), "")
  for (q in parity_queries) for (r in q$requests) expect_true(server_env$parity_key(r$endpoint, r$body) %in% keys, info = q$name)
  # The header names what the answers were made from.
  expect_identical(parity_answers$header$ogquery, "0.1.0")
  expect_identical(parity_answers$header$ogdb_api_commit, "9d36e76")
  expect_identical(parity_answers$header$bundle$commit, "f95f4ab1d64bbae2b54db56c5528c08f93f63e9a")
})

for (query in parity_queries) local({
  query <- query
  test_that(paste0("the API Source gives the real API's answer: ", query$name), {
    result <- api_run(query)
    if (query$verb == "og_analyses") expect_generated_analyses(result, query)
    else expect_identical(generated_differences(result, query), character())
  })
})

for (query in parity_queries) local({
  query <- query
  test_that(paste0("a Local Source gives the real API's answer: ", query$name), {
    skip_if_no_local()
    result <- local_run(query)
    if (query$verb == "og_analyses") {
      expect_generated_analyses(result, query)
      # The catalogue's own columns are NA locally (#32).
      for (name in catalogue_only) expect_true(all(is.na(result$value[[name]])), info = name)
    } else {
      expect_identical(generated_differences(result, query), character())
    }
  })
})

for (query in parity_queries) local({
  query <- query
  test_that(paste0("the API Source and a Local Source return the same: ", query$name), {
    skip_if_no_local()
    api <- api_run(query)
    here <- local_run(query)
    expect_identical(here$warnings, api$warnings)
    if (query$verb == "og_analyses") {
      shared <- setdiff(names(api$value), catalogue_only)
      expect_identical(names(here$value)[seq_along(names(api$value))], names(api$value))
      expect_identical(here$value[shared], api$value[shared])
    } else {
      expect_identical(comparable(here$value), comparable(api$value))
    }
  })
})

# The parity tests fail when one generated value changes --------------------------------------------

test_that("one changed value in the generated answers fails the parity comparison", {
  query <- parity_queries[["tophits-truncated"]]
  # z of the third top hit (X:2500:G:T, -8) becomes -7.5: still a top hit, in the same place.
  mutate <- function(answers) {
    key <- server_env$parity_key(query$requests[[1]]$endpoint, query$requests[[1]]$body)
    for (i in seq_along(answers$answers)) {
      a <- answers$answers[[i]]
      if (identical(server_env$parity_key(a$endpoint, a$body), key)) answers$answers[[i]]$rows[[3]]$z <- -7.5
    }
    answers
  }
  mutated <- local_fake_api(n_synthetic = 0, parity = "three", parity_edit = mutate, env = environment())
  changed <- parity_run(query, fake_source(mutated))
  expect_equal(changed$value$z[[3]], -7.5)
  # The API Source still matches the answers it was served...
  expect_identical(generated_differences(changed, query, mutate(parity_answers)), character())
  # ...but neither the true generated answer nor the API Source over the true answers.
  expect_identical(generated_differences(changed, query), "`z` of s r bmi_irn X:2500:G:T: -7.5, expected -8")
  expect_false(identical(comparable(changed$value), comparable(api_run(query)$value)))
})

# Known differences, asserted ------------------------------------------------------------------------

test_that("known difference: s/r's infinite SE gives a NaN pval locally (the API's pinned reader: finite)", {
  skip_if_no_local()
  x <- og_associations(local_source_of(c("dense", "ragged", "hyb")), "1:1000:A:G", "s:bmi_irn")
  expect_identical(x$se, Inf)
  expect_true(is.nan(x$pval))
})
