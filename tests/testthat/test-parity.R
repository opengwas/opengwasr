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
#    values), the same warnings (class chain and message), the same refusal (class chain, message,
#    `api_error` and every field both carry). Provenance is compared too, less what can't agree:
#    `retrieved` (when each answer was read), `job_ids` (a Local Source runs no Job) and
#    `cost_estimate` (the API's estimate per Job; a Local Source has none, NA).
# `log10_p`, and `pval` (10^log10_p), are compared to within four ulps of log10_p in all three, as
# opengwasdb compares them across machines (column_agrees()): log10_p is the libm's, which differs
# in the last bits between the machine that generated answers.json and the one running the tests.
# Every other value exactly.
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
#    (`opengwasr_api_error`); a Local Source refuses with `opengwasr_local_unsupported`. No issue
#    resolves it, and none is planned: the API's 501 "Not yet implemented" is its one answer for a
#    capability a release lacks, with no code to class it by, and every published Store Release has
#    a Top-Hit Index. Left out: the example releases have one; test-local-associations.R covers the
#    local refusal.
#  * Ragged release without its by-variant index. The API serves its Associations and fails in the
#    reader; a Local Source refuses up front (`opengwasr_local_unsupported`). No issue resolves it,
#    and none is planned: a Ragged Store Release is published with its index, so only a damaged
#    copy lacks it. Left out: the example releases have theirs; test-local-associations.R covers the
#    local refusal.
#  * og_analyses(). The catalogue's own columns (status, listed, trait_class, ancestry_category,
#    label_from_source) are NA on a Local Source (#32's ruling): compared only on the shared
#    columns, and those five asserted NA. A Local Source's further analyses.tsv columns follow
#    the API's and aren't compared.
#  * Capabilities. The bundled copies of the example releases lack variants.tsv.gz, which
#    ogdb-api's `_capabilities` (releases.py:137-171) requires for variant lookups and PheWAS. The
#    bundle's own releases have it, so the generator takes every capability as present. No issue
#    is planned: it is a difference between the bundled copies and the releases the API serves, not
#    between the two Sources.
#  * HTTP. A refusal from the API Source carries what came over HTTP: `status`, `api_message` and
#    `api_body`. A Local Source has no HTTP, so carries none of them, and carries the reason for a
#    malformed request in `reason`, which the API gives only as "Malformed request". Permanent: the
#    Source comparison checks that these are the only fields one carries and the other doesn't.

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

# A golden query on `src`: its value (or the error it raised) and the warnings it gave (class chain
# and message of each).
parity_run <- function(query, src) {
  warnings <- list()
  value <- withCallingHandlers(
    tryCatch(do.call(query$verb, c(list(src), query$args)), error = function(e) e),
    warning = function(w) {
      warnings[[length(warnings) + 1]] <<- list(class = class(w), message = conditionMessage(w))
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

# Each golden query once on a Local Source, kept for the comparison with the API Source.
parity_sources <- jsonlite::read_json(file.path(parity_dir, "queries.json"), simplifyVector = TRUE)$sources
local_runs <- new.env()
local_run <- function(query) {
  if (is.null(local_runs[[query$name]])) {
    local_runs[[query$name]] <- parity_run(query, local_source_of(parity_sources[[query$source]]))
  }
  local_runs[[query$name]]
}

# The generated answer to a query's last request (the Job whose rows the result holds), from
# `answers` (answers.json, parsed).
generated_answer <- function(query, answers = parity_answers) {
  last <- query$requests[[length(query$requests)]]
  key <- server_env$parity_key(last$endpoint, last$body)
  for (a in answers$answers) if (identical(server_env$parity_key(a$endpoint, a$body), key)) return(a)
  stop("answers.json has no answer for query ", query$name, "; run fixtures/parity/generate.py.")
}

# Values compared across machines to within four ulps: log10_p is the libm's (erfc, erf, log,
# log1p), which differs in the last bits between C libraries, so answers generated on one machine
# and read on another (CI's) differ there. opengwasdb's contract compares it so (210e12c9,
# a1c5aa36: CROSS_MACHINE_RTOL, relative 4 * 2^-52). Both Sources give pval as 10^log10_p, which
# carries log10_p's difference multiplied by ln(10) * |log10_p|: CI's 1-ulp difference in a
# log10_p of -8.70 is 19 ulps of pval. So pval agrees when its log10_p does to within four ulps,
# and never more tightly than four ulps of pval itself. Every other value is stored or exact
# arithmetic on stored values, and is compared exactly.
cross_machine_rtol <- 4 * .Machine$double.eps
libm_columns <- c("pval", "log10_p")

# Whether `x` and `y` agree as values of column `name`: identical; or, for a libm column, doubles
# with the same attributes, NA and NaN at the same places, the same infinities and zeros, and each
# other pair within cross_machine_rtol of the larger in magnitude (for pval, of its log10_p).
column_agrees <- function(name, x, y) {
  if (identical(x, y)) return(TRUE)
  if (!name %in% libm_columns || !is.double(x) || !is.double(y) || length(x) != length(y) ||
      !identical(attributes(x), attributes(y))) {
    return(FALSE)
  }
  if (!identical(is.na(x), is.na(y)) || !identical(is.nan(x), is.nan(y))) return(FALSE)
  finite <- is.finite(x) & is.finite(y) & x != 0 & y != 0
  if (!identical(x[!finite], y[!finite])) return(FALSE)
  larger <- pmax(abs(x[finite]), abs(y[finite]))
  scale <- if (name == "pval") pmax(1, log(10) * abs(log10(larger))) else 1
  all(abs(x[finite] - y[finite]) <= cross_machine_rtol * scale * larger)
}

# A Local Source's data frame as compared with the API Source's: a libm column that agrees to
# within four ulps (column_agrees()) takes the API Source's values, so everything else, the
# columns' types, the rows' order and every other value, is compared exactly.
within_ulps <- function(here, api) {
  if (!is.data.frame(here) || !is.data.frame(api)) return(here)
  for (name in intersect(libm_columns, intersect(names(here), names(api)))) {
    if (column_agrees(name, here[[name]], api[[name]])) here[[name]] <- api[[name]]
  }
  here
}

# What is compared of a data frame: itself with its provenance, less the fields that can't agree.
comparable <- function(value) {
  prov <- attr(value, "og_provenance", exact = TRUE)
  if (!is.null(prov)) {
    prov[c("retrieved", "job_ids", "cost_estimate")] <- NULL
    attr(value, "og_provenance") <- prov
  }
  value
}

# Fields of a condition that aren't what it says (rlang's own), and those only one Source carries
# (the known difference "HTTP").
rlang_fields <- c("message", "trace", "parent", "call", "rlang", "body", "footer", "use_cli_format")
api_only_fields <- c("status", "api_message", "api_body")
local_only_fields <- "reason"

# How a Local Source's refusal differs from the API Source's, as messages: the class chain, the
# message, and every field both carry; and any field one carries beyond the known difference.
condition_differences <- function(here, api) {
  if (!inherits(here, "condition") || !inherits(api, "condition")) return("only one Source refused")
  found <- character()
  if (!identical(class(here), class(api))) found <- c(found, paste("class", paste(class(here), collapse = "/")))
  if (!identical(conditionMessage(here), conditionMessage(api))) found <- c(found, paste("message", conditionMessage(here)))
  here_fields <- setdiff(names(unclass(here)), rlang_fields)
  api_fields <- setdiff(names(unclass(api)), rlang_fields)
  for (name in intersect(here_fields, api_fields)) {
    if (!identical(here[[name]], api[[name]])) found <- c(found, paste("field", name))
  }
  extra <- c(setdiff(api_fields, c(here_fields, api_only_fields)), setdiff(here_fields, c(api_fields, local_only_fields)))
  if (length(extra) > 0) found <- c(found, paste("fields on one Source only:", paste(extra, collapse = ", ")))
  found
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
    body <- answer$error$body
    if (!inherits(value, paste0("opengwasr_", body$error)) || !identical(value$api_error, body$error)) {
      return(paste0("expected the refusal `", body$error, "`, got ", class(value)[[1]]))
    }
    found <- character()
    if (!identical(conditionMessage(value), body$message)) found <- paste("message", conditionMessage(value))
    # Every other field of the API's body, as the condition carries it.
    for (name in setdiff(names(body), c("error", "message", "detail"))) {
      if (!identical(unlist(value[[name]]), unlist(body[[name]]))) found <- c(found, paste("field", name))
    }
    return(found)
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
    if (!column_agrees(name, got, expected[[name]])) {
      i <- which(!mapply(function(a, b) column_agrees(name, a, b), got, expected[[name]]))[[1]]
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
    } else if (inherits(api$value, "condition") || inherits(here$value, "condition")) {
      expect_identical(condition_differences(here$value, api$value), character())
    } else {
      expect_identical(comparable(within_ulps(here$value, api$value)), comparable(api$value))
    }
  })
})

# pval agrees across machines to four ulps, every other value exactly ---------------------------

test_that("the comparison takes pval to within four ulps, and every other value exactly", {
  base <- api_run(parity_queries[["tophits-truncated"]])$value
  # 0.5 is a power of two, so one ulp of it is 2^-53 and five ulps are 5 * 2^-53, beyond
  # 4 * .Machine$double.eps * 0.5; at 0.5, ln(10) * |log10_p| is below 1, so the rule is four ulps
  # of pval itself. z's -9 has ulps of 2^-49.
  base$pval[[1]] <- 0.5
  moved <- function(name, by) {
    x <- base
    x[[name]][[1]] <- x[[name]][[1]] + by
    x
  }
  # Between the Sources.
  expect_identical(comparable(within_ulps(moved("pval", 2^-53), base)), comparable(base))
  expect_false(identical(comparable(within_ulps(moved("pval", 5 * 2^-53), base)), comparable(base)))
  expect_false(identical(comparable(within_ulps(moved("z", -2^-49), base)), comparable(base)))
  # Against the generated answer, by the same rule.
  expect_true(column_agrees("pval", 0.5 + 2^-53, 0.5))
  expect_false(column_agrees("pval", 0.5 + 5 * 2^-53, 0.5))
  expect_false(column_agrees("z", -9 - 2^-49, -9))
  # CI's case (run 45604755758): s/r 1:9000:A:G, whose log10_p differs by one ulp (2^-49 at -8.70),
  # passes; five ulps of that log10_p fail.
  expect_true(column_agrees("pval", 1.9731752900753962e-09, 1.9731752900753879e-09))
  log10_p <- -8.7048343318127248
  expect_false(column_agrees("pval", 10^(log10_p - 5 * 2^-49), 10^log10_p))
  expect_true(column_agrees("log10_p", log10_p - 2^-49, log10_p))
  expect_false(column_agrees("log10_p", log10_p - 5 * 2^-49, log10_p))
  # NA and NaN, and the infinities, are compared exactly.
  expect_false(column_agrees("pval", NA_real_, NaN))
  expect_false(column_agrees("pval", Inf, .Machine$double.xmax))
  expect_true(column_agrees("pval", c(NaN, 1), c(NaN, 1 + 2^-52)))
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

# The fake's own Job routes follow the real API ---------------------------------------------------
# Where the golden set touches them, the fake's routes over its usual world make the real API's
# cuts, order, refusals and counts (ogdb-api at 9d36e76), so the rest of the suite tests against
# the real rules. Each is checked here against a literal from the API's code or the generator.

fake_plain <- local_fake_api(n_synthetic = 0)
plain_cad <- "OGS-00011:GCST003116"
plain_bmi <- "OGS-00011:GCST90029007"
plain_releases <- list(list(store_id = "OGS-00011", release_id = "OGS-00011-completed"))

# A Job on the plain fake, through its routes alone (no opengwasr): its rows (each a list) and
# result metadata, or the error body it was refused with.
plain_job <- function(endpoint, body) {
  submitted <- raw_api(fake_plain$url(), fake_key, endpoint, body)
  if (is.null(submitted$job_id)) return(list(error = submitted))
  status <- raw_api(fake_plain$url(), fake_key, paste0("api/jobs/", submitted$job_id))
  page <- raw_api(fake_plain$url(), fake_key, paste0("api/jobs/", submitted$job_id, "/result"))
  lines <- strsplit(page$ndjson, "\n", fixed = TRUE)[[1]]
  list(rows = lapply(lines, jsonlite::parse_json), metadata = status$result$metadata)
}
alids_of <- function(job) vapply(job$rows, `[[`, "", "alid")

test_that("the fake orders rows as the API does: GWAS Store, Release, Analysis, then genome order", {
  # ordering.py `row_key`: GCST003116 sorts before GCST90029007, whatever order they are asked in.
  job <- plain_job("api/associations", list(variants = list("18:60161902:C:T", "5:75701728:A:AAAAT"),
                                            analyses = list(plain_bmi, plain_cad), observed_only = FALSE,
                                            releases = plain_releases))
  expect_identical(vapply(job$rows, `[[`, "", "analysis_id"), c("GCST003116", "GCST003116", "GCST90029007", "GCST90029007"))
  expect_identical(alids_of(job), rep(c("5:75701728:A:AAAAT", "18:60161902:C:T"), 2))
  # Top hits: each Analysis strongest first, ties in genome order (query_operations.py:202).
  job <- plain_job("api/tophits", list(analyses = list(plain_bmi, plain_cad), threshold = 5e-8, limit = 1000L,
                                       observed_only = FALSE, releases = plain_releases))
  expect_identical(vapply(job$rows, function(r) r$z, 0), c(-20, 16, 10, -8, 5.6, -40, 6))
})

test_that("the fake cuts at |z| >= z_critical(threshold), as the API and the Top-Hit Index do", {
  # The generator's z_critical(5e-8), from ogquery, which the API cuts with.
  expect_equal(server_env$fake_z_critical(5e-8), parity_answers$header$z_critical[["5e-08"]], tolerance = 1e-12)
  # CAD's rs1333049 has z 5.6 (two-sided p 2.14e-8) but log10_p -7.5: |z| decides, not log10_p.
  top <- function(threshold) {
    alids_of(plain_job("api/tophits", list(analyses = list(plain_cad), threshold = threshold, limit = 1000L,
                                           observed_only = FALSE, releases = plain_releases)))
  }
  expect_true("9:22115503:A:G" %in% top(2.2e-8))
  expect_false("9:22115503:A:G" %in% top(2.1e-8))
})

test_that("the fake refuses a top-hits threshold looser than every tier, as the API does", {
  job <- plain_job("api/tophits", list(analyses = list(plain_cad), threshold = 1e-3, limit = 1000L,
                                       observed_only = FALSE, releases = plain_releases))
  # query_operations.py:107-110: the message names the release's GWAS Store and its loosest tier.
  expect_identical(job$error$error, "unsupported_threshold")
  expect_identical(job$error$message,
                   "threshold must be at most 0.0005, the loosest p-value the Top-Hit Index of OGS-00011 holds")
  expect_identical(unlist(job$error$indexed_thresholds), c(5e-8, 5e-6, 5e-4))
  # Another GWAS Store's release is named for its own.
  job <- plain_job("api/tophits", list(analyses = list("OGS-00009:ukb-b-19953"), threshold = 1e-3, limit = 1000L,
                                       observed_only = FALSE,
                                       releases = list(list(store_id = "OGS-00009", release_id = "OGS-00009-completed"))))
  expect_identical(job$error$message,
                   "threshold must be at most 0.0005, the loosest p-value the Top-Hit Index of OGS-00009 holds")
  # The body model refuses a threshold outside (0, 1) first (schemas.py:225, errors.py:64).
  job <- plain_job("api/tophits", list(analyses = list(plain_cad), threshold = 1, limit = 1000L,
                                       observed_only = FALSE, releases = plain_releases))
  expect_identical(job$error$error, "invalid_request")
})

test_that("the fake's PheWAS counts every match in `total` and cuts the ordered rows at `limit`", {
  phewas <- function(threshold, limit) {
    plain_job("api/phewas", list(variant = "16:53767042:C:T", threshold = threshold, limit = limit,
                                 observed_only = FALSE, releases = plain_releases))
  }
  # CAD (z 3.08) and BMI (z 38) hold it; threshold 1 keeps both (top_hits.py `significant`).
  job <- phewas(1, 1L)
  expect_identical(alids_of(job), "16:53767042:C:T")
  expect_identical(job$rows[[1]]$analysis_id, "GCST003116")
  expect_identical(job$metadata[c("variant", "total", "returned", "truncated")],
                   list(variant = "16:53767042:C:T", total = 2L, returned = 1L, truncated = TRUE))
  # At 1e-8 (z_critical about 5.73) only BMI's clears.
  job <- phewas(1e-8, 1000L)
  expect_identical(vapply(job$rows, `[[`, "", "analysis_id"), "GCST90029007")
  expect_identical(job$metadata$total, 1L)
})

test_that("the fake refuses a range on a chromosome other than 1-22, X, Y or MT, as the API does", {
  range <- function(chromosome) {
    plain_job("api/associations/range", list(analyses = list(plain_cad), chromosome = chromosome, start = 1L,
                                             end = 1000L, observed_only = FALSE, releases = plain_releases))
  }
  # schemas.py:35-44 and 166, refused by the body model (errors.py:64).
  expect_identical(range("27")$error$error, "invalid_request")
  expect_identical(range("Z")$error$error, "invalid_request")
  expect_null(range("X")$error)
  expect_null(range("23")$error)
})

test_that("the fake sends each Association once, and a missing one with NaN statistics and EAF \".\"", {
  # An Analysis named twice reads it once (releases.py `local_analysis_ids`, planner.py:259-266).
  job <- plain_job("api/associations", list(variants = list("18:60161902:C:T"), analyses = list(plain_cad, plain_cad),
                                            observed_only = FALSE, releases = plain_releases))
  expect_identical(alids_of(job), "18:60161902:C:T")
  # Through the API Source: the missing row's statistics are NaN, its EAF NA.
  x <- og_associations(fake_source(fake_plain), "21:5000:A:G", plain_cad, releases = "OGS-00011/OGS-00011-completed")
  expect_identical(x$association_status, "missing")
  expect_true(is.nan(x$z) && is.nan(x$se) && is.nan(x$beta))
  expect_identical(x$eaf, NA_real_)
})
