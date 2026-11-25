# The fake Data API's server side. Self-contained (base R, jsonlite, rlang, webfakes), because webfakes
# runs the app in a separate R process, which loads this file itself. helper-fake-api.R sources it
# into the test session too, so tests can use `fake_world()`, `schema_problems()` and friends.
# `fixture_dir` must be set before sourcing.

`%||%` <- function(x, y) if (is.null(x)) y else x

read_fixture <- function(name) {
  jsonlite::read_json(file.path(fixture_dir, name), simplifyVector = FALSE)
}

# A small JSON Schema validator: the subset the Data API's schemas use. Returns the problems found
# as a character vector (empty when `value` is valid). `value` is parsed JSON, lists not vectors.
schema_problems <- function(value, schema, root, path = "$") {
  if (!is.null(schema[["$ref"]])) {
    schema <- root$components$schemas[[basename(schema[["$ref"]])]]
  }
  if (!is.null(schema$anyOf)) {
    tries <- lapply(schema$anyOf, function(s) schema_problems(value, s, root, path))
    if (any(lengths(tries) == 0)) return(character())
    return(paste0(path, ": matches none of the allowed shapes"))
  }
  type_ok <- switch(schema$type %||% "any",
    string = is.character(value) && length(value) == 1,
    integer = is.numeric(value) && length(value) == 1 && value == round(value),
    number = is.numeric(value) && length(value) == 1,
    boolean = is.logical(value) && length(value) == 1,
    null = is.null(value),
    array = is.list(value) && is.null(names(value)),
    object = is.list(value) && (length(value) == 0 || !is.null(names(value))),
    any = TRUE
  )
  if (!type_ok) return(paste0(path, ": expected ", schema$type))
  problems <- character()
  if (!is.null(schema$const) && !identical(value, schema$const)) {
    problems <- c(problems, paste0(path, ": must be ", schema$const))
  }
  if (is.numeric(value) && !is.null(schema$minimum) && value < schema$minimum) {
    problems <- c(problems, paste0(path, ": below ", schema$minimum))
  }
  if (is.numeric(value) && !is.null(schema$maximum) && value > schema$maximum) {
    problems <- c(problems, paste0(path, ": above ", schema$maximum))
  }
  if (is.numeric(value) && !is.null(schema$exclusiveMinimum) && value <= schema$exclusiveMinimum) {
    problems <- c(problems, paste0(path, ": not above ", schema$exclusiveMinimum))
  }
  if (is.numeric(value) && !is.null(schema$exclusiveMaximum) && value >= schema$exclusiveMaximum) {
    problems <- c(problems, paste0(path, ": not below ", schema$exclusiveMaximum))
  }
  if (is.character(value) && !is.null(schema$maxLength) && nchar(value) > schema$maxLength) {
    problems <- c(problems, paste0(path, ": longer than ", schema$maxLength))
  }
  if (identical(schema$type, "array")) {
    if (!is.null(schema$maxItems) && length(value) > schema$maxItems) {
      problems <- c(problems, paste0(path, ": more than ", schema$maxItems, " items"))
    }
    if (!is.null(schema$items)) {
      for (i in seq_along(value)) {
        problems <- c(problems, schema_problems(value[[i]], schema$items, root, paste0(path, "[", i, "]")))
      }
    }
  }
  if (identical(schema$type, "object")) {
    for (name in schema$required) {
      if (!name %in% names(value)) problems <- c(problems, paste0(path, ": missing `", name, "`"))
    }
    for (name in intersect(names(schema$properties), names(value))) {
      problems <- c(problems, schema_problems(value[[name]], schema$properties[[name]], root,
                                              paste0(path, ".", name)))
    }
  }
  problems
}

# Problems with a request body for `path` and `method`, per the OpenAPI document.
request_body_problems <- function(body, path, method, openapi) {
  schema <- openapi$paths[[path]][[method]]$requestBody$content[["application/json"]]$schema
  if (is.null(schema)) return(paste0(path, " takes no JSON body in the contract"))
  schema_problems(body, schema, openapi)
}

fake_key <- "fake-key-not-a-secret"

# The world the fake serves: real Analysis rows recorded from the dev Data API
# (fixtures/world.json) plus hand-written scenarios the dev catalogue can't produce today.
fake_world <- function(n_synthetic = 1001, parity = NULL, parity_edit = identity) {
  world <- read_fixture("world.json")
  template <- world$analyses[[1]]
  # Synthetic rows carry only the keys the tests read, so a 1,001-row catalogue is cheap to serve.
  slim <- template[intersect(names(template), c("id", "store_id", "analysis_id", "analysis_label", "release_id",
                                                "stored_effect_scale", "sample_size", "sample_size_kind"))]
  synthetic <- lapply(seq_len(n_synthetic), function(i) {
    row <- slim
    row$analysis_id <- sprintf("SYN%05d", i)
    row$id <- paste0("OGS-00006:", row$analysis_id)
    row$store_id <- "OGS-00006"
    row$release_id <- "OGS-00006-completed"
    row$analysis_label <- sprintf("Synthetic trait %d", i)
    row
  })
  # A bare analysis id held by two GWAS Stores (the dev catalogue has none).
  ambiguous <- lapply(c("OGS-00011", "OGS-00012"), function(store) {
    row <- template
    row$analysis_id <- "GCST90000001"
    row$id <- paste0(store, ":GCST90000001")
    row$store_id <- store
    row$release_id <- paste0(store, "-completed")
    row$analysis_label <- paste("Ambiguous trait in", store)
    row
  })
  # A binary Analysis stored per SD (the 1,846 `ukb-b-*` binary Analyses in the dev catalogue).
  binary_sd <- template
  binary_sd$analysis_id <- "ukb-b-13745"
  binary_sd$id <- "OGS-00009:ukb-b-13745"
  binary_sd$store_id <- "OGS-00009"
  binary_sd$release_id <- "OGS-00009-completed"
  binary_sd$analysis_label <- "Binary trait stored per SD"
  binary_sd$outcome_type <- "binary"
  binary_sd$sample_size_kind <- "total"
  binary_sd$sample_size <- 442169L
  binary_sd$n_cases <- 125001L
  binary_sd["original_sd"] <- list(NULL)
  binary_sd$original_sd_method <- "declared_standardised"
  binary_sd$original_effect_scale <- "sd"
  # An AFR Analysis (served under the `afr_analysis` scenario), for LD matched by ancestry.
  afr <- template
  afr$analysis_id <- "GCST90475090"
  afr$id <- "OGS-00011:GCST90475090"
  afr$analysis_label <- "HbA1c in African ancestry"
  afr$assigned_ancestry <- "AFR"
  world$afr_analysis <- list(afr)
  world$analyses <- c(world$analyses, ambiguous, synthetic)
  world$binary_sd <- list(binary_sd)  # served only under the `binary_sd` scenario
  world$retired <- list(list(id = "OGS-00013:OLD1", catalog_id = "OGS-00013:OLD1",
                             superseded_by = "OGS-00014:NEW1"),
                        list(id = "OGS-00013:SPLIT", catalog_id = "OGS-00013:SPLIT",
                             superseded_by = list("OGS-00014:A", "OGS-00014:B")),
                        list(id = "OGS-00013:GONE", catalog_id = "OGS-00013:GONE", superseded_by = NULL))
  # Job scenarios (`job`): normal (succeeds at the first look), queued_running, never (stays running),
  # failed_request, failed_temporary, failed_leak (the failure echoes the caller's key), cancelled,
  # expired, ignored (the API ignored a field, and echoes the key), result_gone (410 on the result);
  # defects: bad_status, no_result, no_ndjson, bad_ndjson, short_rows, no_metadata, total_too_big,
  # total_too_small (the status and NDJSON ones echo the caller's key). `page_rows` sets the result
  # page size, `poll_after_ms` the interval the API asks for, `delay_ms` how long it takes to answer a
  # Job request, `many_stores` adds 11 Analyses in 11 GWAS Stores (OGS-02001 to OGS-02011).
  # Busy API: `busy` is a code from the errors guide (`user_queue_full` answers 429, `intake_paused` 503),
  # `busy_times` how many requests to Job routes it refuses before it recovers, `busy_after` the
  # Retry-After it sends, `busy_route` which request (submit, status or result). DELETE /api/jobs/<id> cancels a Job.
  # Scenarios, switched by POST /_fake/<setting>/<value>: `two_releases` makes GWAS Store OGS-00099
  # serve two Store Releases (listed newest-first; the API gives no order); `leak` makes errors and
  # lookup reasons echo the caller's key, as a careless server might; `plain401` answers 401 in plain
  # text; `no_analyses` answers gwasinfo with `{}`.
  world$extra_releases <- list(list(store_id = "OGS-00099", release_id = "OGS-00099-b"),
                               list(store_id = "OGS-00099", release_id = "OGS-00099-a"))
  world$many_analyses <- lapply(1:11, function(i) {
    row <- slim
    row$analysis_id <- sprintf("MANY%02d", i)
    row$store_id <- sprintf("OGS-%05d", 2000 + i)
    row$id <- paste0(row$store_id, ":", row$analysis_id)
    row$release_id <- paste0(row$store_id, "-r")
    row
  })
  world$many_releases <- lapply(world$many_analyses, function(a) list(store_id = a$store_id, release_id = a$release_id))
  world$tophits <- fake_tophits()
  world$variants <- fake_variants()
  world$associations <- fake_associations()
  world$ranges <- list()
  # The Analyses the splitting tests spread queries over (first 120 synthetic, and the 11 of
  # `many_stores`) each hold the variant every one of them shares (3:1000:A:C) and one of their own.
  # The same rows are their top hits.
  held <- function(a, i, own) {
    one <- function(alid, z) {
      parts <- strsplit(alid, ":", fixed = TRUE)[[1]]
      list(alid = alid, analysis_id = a$analysis_id, analysis_label = a$analysis_label,
           association_status = "observed", chromosome = parts[[1]], eaf = 0.3, effect_allele = parts[[3]],
           log10_p = -9 - i / 100, other_allele = parts[[4]], position = as.integer(parts[[2]]),
           release_id = a$release_id, rsid = ".", se = 0.01, store_id = a$store_id, z = z)
    }
    list(one(own, 6), one("3:1000:A:C", 7))
  }
  for (i in seq_len(min(n_synthetic, 120))) {
    a <- synthetic[[i]]
    world$associations[[a$id]] <- world$tophits[[a$id]] <- held(a, i, sprintf("3:%d:A:G", 2000 + i))
  }
  for (i in 1:11) {
    a <- world$many_analyses[[i]]
    world$associations[[a$id]] <- world$tophits[[a$id]] <- held(a, i, sprintf("4:%d:A:G", 100 + i))
  }
  # Rows only `POST /api/associations/range` serves (so no other query's answer changes): each of
  # the first 25 synthetic Analyses and the first MANY Analysis hold two more variants on
  # chromosome 3, 12 Mb and 25 Mb along, and one on chromosome 4.
  far <- function(a, i, alid) held(a, i, alid)[[1]]
  for (i in seq_len(min(n_synthetic, 25))) {
    a <- synthetic[[i]]
    world$ranges[[a$id]] <- list(far(a, i, sprintf("3:%d:A:G", 12000000 + i)),
                                 far(a, i, sprintf("3:%d:A:G", 25000000 + i)),
                                 far(a, i, sprintf("4:%d:A:G", 500 + i)))
  }
  world$ranges[["OGS-00011:GCST003116"]] <- world$tophits[["OGS-00011:GCST003116"]]  # five on chromosome 9
  # Limits: the Job routes enforce the contract's array limits (100 Analyses, 1,000 variants, 10 Store
  # Releases) as the API does; `lift_limits` lifts them, for the reference run of a split query.
  world$mode <- list(paging = "cursor", lift_limits = FALSE, malformed = FALSE, two_releases = FALSE, leak = FALSE,
                     plain401 = FALSE, no_analyses = FALSE, many_stores = FALSE, job = "normal",
                     page_rows = 1000L, poll_after_ms = 10, delay_ms = 0,
                     busy = "none", busy_times = 0L, busy_after = 3L, busy_route = "submit")
  world$key <- fake_key
  world$ld_stores <- fake_ld_stores()
  if (!is.null(parity)) world <- fake_parity_world(world, parity, parity_edit)
  world
}

# The parity world (test-parity.R): the Data API as it would serve the example Store Releases of
# source set `set` ("three" or "completed", fixtures/parity/queries.json), from the answers
# fixtures/parity/generate.py wrote (answers.json). The fake serves those Store Releases, their
# Analyses as the catalogue would (`catalogue`), and, to each golden Job, its generated answer
# (rows and metadata, or the API's error), found by endpoint and body (parity_key()); the Job,
# paging and result routes are the usual ones. A Job request with no generated answer is a 500
# naming it, so a golden query whose requests changed fails loudly rather than reading the fake's
# own rows. `edit` changes the answers first (test-parity.R's mutation test).
fake_parity_world <- function(world, set, edit = identity) {
  answers <- edit(read_fixture(file.path("parity", "answers.json")))
  releases <- answers$sources[[set]]
  if (is.null(releases)) stop("No parity source set `", set, "` in answers.json.")
  held <- vapply(releases, function(r) paste0(r$store_id, "/", r$release_id), "")
  world$releases <- releases
  world$analyses <- Filter(function(a) paste0(a$store_id, "/", a$release_id) %in% held, answers$catalogue)
  world$legacy <- list()
  world$retired <- list()
  world$parity <- stats::setNames(answers$answers, vapply(answers$answers, function(a) parity_key(a$endpoint, a$body), ""))
  world$mode$parity <- TRUE
  world
}

# A Job request's key among the generated answers: its endpoint and its body, every object's
# members in name order, as JSON.
parity_key <- function(endpoint, body) {
  canonical <- function(x) {
    if (!is.list(x)) return(x)
    if (!is.null(names(x))) x <- x[order(names(x), method = "radix")]
    lapply(x, canonical)
  }
  paste(endpoint, jsonlite::toJSON(canonical(body), auto_unbox = TRUE, null = "null", digits = NA))
}

# The LD Stores, as `GET /api/ld/stores` gives them (the real five, with their real limits). Under the
# `ld_declared_default` scenario the API declares a default; under `ld_two_eur` a second EUR store is served.
fake_ld_stores <- function() {
  store <- function(ancestry, n, t_far, n_variants) {
    list(store = paste0("hgdp1kgp-v2-", ancestry, "-k10"), ancestry = ancestry, ready = TRUE,
         panel = paste0("hgdp1kgp-v2-", ancestry), n = n, k = 10L, t_far = t_far, near_bp = 3000000L,
         far_bp = 10000000L, n_variants = n_variants, format_version = "0.1.0", ogld_version = "0.1.0")
  }
  list(store("EUR", 665L, 0.03186544, 8586889L), store("AFR", 608L, 0.03490787, 14374341L),
       store("EAS", 724L, 0.02922861, 7837854L), store("SAS", 672L, 0.03152799, 8879035L),
       store("AMR", 341L, 0.06315152, 7561067L))
}

# The fake LD Stores hold every variant on a chromosome 1 to 22 except those whose position ends in
# 999 (so a test can ask for absent ones). r between two held variants is deterministic and
# symmetric: 1 on the diagonal, else a signed value that decays with distance. The LD Store holds
# it within `near_bp`, beyond that only where r^2 reaches `t_far`, and nothing beyond `far_bp` or
# across chromosomes (`NA`, which the API sends as null).
fake_ld_holds <- function(alid) {
  parts <- strsplit(alid, ":", fixed = TRUE)[[1]]
  suppressWarnings(as.integer(parts[[1]])) %in% 1:22 && as.numeric(parts[[2]]) %% 1000 != 999
}
fake_ld_r <- function(a, b, store) {
  pa <- strsplit(a, ":", fixed = TRUE)[[1]]
  pb <- strsplit(b, ":", fixed = TRUE)[[1]]
  if (identical(a, b)) return(1)
  if (!identical(pa[[1]], pb[[1]])) return(NA_real_)
  x <- as.numeric(pa[[2]])
  y <- as.numeric(pb[[2]])
  d <- abs(x - y)
  if (d > store$far_bp) return(NA_real_)
  r <- ((x * y) %% 1000 / 500 - 1) * exp(-d / 2e6)
  if (d > store$near_bp && r^2 < store$t_far) return(NA_real_)
  round(r, 6)
}

# Top hits per Analysis, strongest |z| first, as the API's result rows. The numbers are literal so
# tests can state their expectations by hand: beta = z * se, pval = 10^log10_p.
fake_tophits <- function() {
  hit <- function(store, release, analysis, label, alid, rsid, z, se, log10_p, status = "observed") {
    parts <- strsplit(alid, ":", fixed = TRUE)[[1]]
    list(alid = alid, analysis_id = analysis, analysis_label = label, association_status = status,
         chromosome = parts[[1]], eaf = 0.5, effect_allele = parts[[3]], log10_p = log10_p,
         other_allele = parts[[4]], position = as.integer(parts[[2]]), release_id = release,
         rsid = rsid, se = se, store_id = store, z = z)
  }
  cad <- function(...) hit("OGS-00011", "OGS-00011-completed", "GCST003116", "Coronary artery disease", ...)
  bmi <- function(...) hit("OGS-00011", "OGS-00011-completed", "GCST90029007", "Body mass index", ...)
  ukb <- function(analysis, ...) hit("OGS-00009", "OGS-00009-completed", analysis, "ukb trait", ...)
  list(
    "OGS-00009:ukb-b-19953" = list(
      ukb("ukb-b-19953", "1:100:A:G", "rs100", 8, 0.05, -15),
      ukb("ukb-b-19953", "2:200:A:G", "rs200", -6, 0.1, -8.5)
    ),
    # Molecular Analyses carry a per-Association N on some rows (the dev API's ENSG rows do).
    "OGS-00027:ENSG00000000419" = list(
      c(hit("OGS-00027", "OGS-00027", "ENSG00000000419", "ENSG00000000419", "X:100:A:G", "rs1", -9, 0.1, -18), list(n = 30694L)),
      c(hit("OGS-00027", "OGS-00027", "ENSG00000000419", "ENSG00000000419", "X:200:A:G", "rs2", 8.6, 0.1, -17), list(n = 30508L)),
      hit("OGS-00027", "OGS-00027", "ENSG00000000419", "ENSG00000000419", "X:300:A:G", "rs3", 6, 0.1, -9)
    ),
    "OGS-00009:ukb-b-13745" = list(ukb("ukb-b-13745", "3:300:A:G", "rs300", 7, 0.02, -11)),
    "OGS-00011:GCST003116" = list(
      cad("9:22098620:A:G", "rs2891168", -20, 0.0125, -88),
      cad("9:22098575:A:G", "rs4977574", 16, 0.0125, -57),
      cad("9:22123767:A:C", "rs10738610", 10, 0.02, -23, "imputed"),
      cad("9:22106226:A:G", "rs10757275", -8, 0.05, -15),
      cad("9:22115503:A:G", "rs1333049", 5.6, 0.05, -7.5)
    ),
    "OGS-00011:GCST90029007" = list(
      bmi("16:53767042:A:C", "rs1558902", -40, 0.001, -350),
      bmi("16:53800000:A:G", "rs9939609", 6, 0.25, -8)
    ),
    # The AFR Analysis (`afr_analysis` scenario): one hit no LD Store holds (position ending 999).
    "OGS-00011:GCST90475090" = list(
      hit("OGS-00011", "OGS-00011-completed", "GCST90475090", "HbA1c in African ancestry", "1:5000:A:G", "rs51", 9, 0.02, -19),
      hit("OGS-00011", "OGS-00011-completed", "GCST90475090", "HbA1c in African ancestry", "1:5999:A:G", "rs52", 8, 0.02, -15),
      hit("OGS-00011", "OGS-00011-completed", "GCST90475090", "HbA1c in African ancestry", "1:5200:A:G", "rs53", 7, 0.02, -12)
    )
  )
}

# Clumping as the API does it (plink's greedy rule): strongest score first, each lead takes every
# weaker variant not yet clumped within `kb` whose r^2 with it (fake_ld_r()) is above `r2`. Returns
# each variant's lead and r with it.
fake_clump <- function(alids, scores, store, r2, kb) {
  lead <- rep(NA_character_, length(alids))
  r <- rep(NA_real_, length(alids))
  order <- order(-scores)
  position <- function(a) as.numeric(strsplit(a, ":", fixed = TRUE)[[1]][[2]])
  for (i in order) {
    if (!is.na(lead[i])) next
    lead[i] <- alids[i]
    r[i] <- 1
    for (j in order) {
      if (!is.na(lead[j])) next
      rr <- fake_ld_r(alids[i], alids[j], store)
      if (!is.na(rr) && abs(position(alids[i]) - position(alids[j])) < kb * 1000 && rr^2 > r2) {
        lead[j] <- alids[i]
        r[j] <- rr
      }
    }
  }
  list(lead = lead, r = r)
}

# The variant table of GWAS Store OGS-00011: one row per ALID an rsID names, as the dev Data API
# answers `POST /api/variants/rsid`. rs34159589 and rs1421085 are multiallelic: each names two ALIDs.
fake_variants <- function() {
  variant <- function(alid, rsid) {
    parts <- strsplit(alid, ":", fixed = TRUE)[[1]]
    list(alid = alid, chromosome = parts[[1]], effect_allele = parts[[3]], other_allele = parts[[4]],
         position = as.integer(parts[[2]]), release_id = "OGS-00011-completed", rsid = rsid,
         store_id = "OGS-00011")
  }
  list(variant("5:75701728:A:AAAAT", "rs34159589"), variant("5:75701728:AAAAT:AAAATAAAT", "rs34159589"),
       variant("16:53767042:A:G", "rs1421085"), variant("16:53767042:C:T", "rs1421085"),
       variant("18:60161902:C:T", "rs6567160"))
}

# 1 to 22, then X, Y and MT, then any other name, as the API orders genome positions
# (ogdb-api ordering.py `chromosome_rank`).
fake_chromosome_rank <- function(chromosome) {
  rank <- suppressWarnings(as.numeric(chromosome))
  if (!is.na(rank) && rank %in% 1:22) return(rank)
  c(X = 23, Y = 24, MT = 25)[chromosome] %|na|% 100
}
`%|na|%` <- function(x, y) if (is.na(x)) y else unname(x)

# The real Data API's rules the fake's Job routes follow (ogdb-api at 9d36e76; test-parity.R tests
# each against the API's own literals).

# Rows in the API's one order (ordering.py `row_key`, planner.py:258, query_operations.py:168): GWAS
# Store, Store Release and Analysis, then genome order (chromosome, position, ALID). With
# `strongest`, |z| descending (a missing z last) before genome order: top hits
# (query_operations.py:202).
fake_api_order <- function(rows, strongest = FALSE) {
  if (length(rows) < 2) return(rows)
  field <- function(name) vapply(rows, function(r) as.character(r[[name]] %||% ""), "")
  strength <- vapply(rows, function(r) fake_strength(r$z), 0)
  order <- order(field("store_id"), field("release_id"), field("analysis_id"),
                 if (strongest) -strength else rep(0, length(rows)),
                 vapply(rows, function(r) fake_chromosome_rank(r$chromosome), 0),
                 vapply(rows, function(r) as.numeric(r$position), 0), field("alid"), method = "radix")
  rows[order]
}

# |z|, a missing one -1 (top_hits.py `strength`).
fake_strength <- function(z) {
  z <- suppressWarnings(as.numeric(z))
  if (is.na(z)) -1 else abs(z)
}

# Whether a row's two-sided p is at most `threshold`, as the API cuts it (top_hits.py `significant`
# and `at_threshold`): |z| >= z_critical(threshold), a missing z never; a threshold of 1 keeps every
# row (PheWAS only: top hits refuse 1).
fake_clears <- function(row, threshold) {
  threshold >= 1 || fake_strength(row$z) >= fake_z_critical(threshold)
}

# ogquery stats.py `z_critical`: -qnorm(threshold / 2).
fake_z_critical <- function(threshold) -stats::qnorm(threshold / 2)

# The top-hit tiers every Store Release of the fake stores (ogquery's TOP_HIT_THRESHOLDS, which
# every real Store Release holds).
fake_top_hit_tiers <- c(5e-8, 5e-6, 5e-4)

# Each Association once (planner.py:259-266): of rows with the same GWAS Store, Store Release,
# Analysis and ALID, the first. The fake holds no two that differ, which the API would refuse.
fake_distinct <- function(rows) {
  key <- vapply(rows, function(r) paste(r$store_id, r$release_id, r$analysis_id, r$alid), "")
  rows[!duplicated(key)]
}

# Whether a range's chromosome is one of 1-22, X, Y or MT (schemas.py `check_chromosome`, with
# ogquery's `normalise_chromosome`: a `chr` prefix, 23-26 and M are aliases).
fake_primary_chromosome <- function(chromosome) {
  text <- toupper(sub("^chr", "", trimws(chromosome), ignore.case = TRUE))
  text %in% c(as.character(1:22), "X", "Y", "MT", "23", "24", "25", "26", "M")
}

# Associations of the variants above, per Analysis, as the API's result rows. The numbers are
# literal (the first of each Analysis as the dev Data API gave them), so tests state expectations by
# hand: beta = z * se, pval = 10^log10_p. As in the vignette, no Analysis holds the second
# 5:75701728 ALID, and only the BMI one holds 16:53767042:A:G (the dev Data API's holds neither).
fake_associations <- function() {
  row <- function(analysis, label, alid, rsid, eaf, z, se, log10_p, status) {
    parts <- strsplit(alid, ":", fixed = TRUE)[[1]]
    list(alid = alid, analysis_id = analysis, analysis_label = label, association_status = status,
         chromosome = parts[[1]], eaf = eaf, effect_allele = parts[[3]], log10_p = log10_p,
         other_allele = parts[[4]], position = as.integer(parts[[2]]), release_id = "OGS-00011-completed",
         rsid = rsid, se = se, store_id = "OGS-00011", z = z)
  }
  cad <- function(...) row("GCST003116", "Coronary artery disease", ...)
  bmi <- function(...) row("GCST90029007", "Body mass index", ...)
  list(
    "OGS-00011:GCST003116" = list(
      cad("5:75701728:A:AAAAT", "rs34159589", 0.369216, -0.3066406, 0.009841919, -0.1196913, "imputed"),
      cad("16:53767042:C:T", "rs1421085", 0.3791291, 3.0810547, 0.009628296, -2.6855665, "observed"),
      cad("18:60161902:C:T", "rs6567160", 0.25, 5.5, 0.01, -7.5, "observed"),
      # The variants of `fake_tophits()`'s BMI hits, for the vignette's BMI-to-CAD workflow.
      cad("16:53767042:A:C", "rs1558902", 0.4, 2, 0.01, -1.4, "imputed"),
      cad("16:53800000:A:G", "rs9939609", 0.3, 4, 0.01, -4.2, "observed"),
      # A missing Association of a Reference-Completed release: no statistics (NaN, which the fake
      # writes "NaN"; the real API writes NaN, which isn't JSON: opengwas/ogdb-api#193) and EAF ".".
      cad("21:5000:A:G", "rs999025", ".", NaN, NaN, NaN, "missing")
    ),
    "OGS-00011:GCST90029007" = list(
      bmi("5:75701728:A:AAAAT", "rs34159589", 0.37, -12, 0.002, -33, "imputed"),
      bmi("16:53767042:C:T", "rs1421085", 0.38, 38, 0.002, -300, "observed"),
      bmi("18:60161902:C:T", "rs6567160", 0.25, 24, 0.002, -125, "observed"),
      # Not in the dev Data API: the one Analysis that holds both ALIDs of a multiallelic rsID.
      # The API sends rsid "." where a variant has no rsID.
      bmi("16:53767042:A:G", ".", 0.62, -38, 0.002, -300, "observed"),
      # The API sends eaf "." where the Store Release has none (ogquery's marker).
      bmi("21:1000:A:G", "rs999021", ".", 7, 0.01, -11.6, "observed"),
      # An infinite SE (a release before format 0.2.3 may hold one) with a finite log10_p, as the
      # API's pinned reader computes log10_p from z alone. jsonlite writes Inf as "Inf"; the real API
      # would write `Infinity`, which isn't JSON (opengwas/ogdb-api#193).
      bmi("21:2000:A:G", "rs999022", 0.4, 6, Inf, -8.7, "observed"),
      # An EAF and a per-Association N with 17 significant digits, and a row with "." in both, so
      # one page mixes numbers and "." in each (a Ragged release's rows, as ogquery resolves them).
      c(bmi("21:3000:A:G", "rs999023", 0.05000000074505806, 5, 0.01, -6.3, "observed"), list(n = 30694.123456789012)),
      c(bmi("21:4000:A:G", "rs999024", ".", 5, 0.01, -6.3, "observed"), list(n = "."))
    )
  )
}

served_releases <- function(world) {
  c(world$releases, if (isTRUE(world$mode$two_releases)) world$extra_releases,
    if (isTRUE(world$mode$many_stores)) world$many_releases)
}

fake_api <- function(world = fake_world()) {
  app <- webfakes::new_app()
  app$use(webfakes::mw_json(simplifyVector = FALSE))
  app$locals$world <- world
  app$locals$openapi <- read_fixture("openapi.json")
  app$locals$errors <- read_fixture("errors.json")

  # Every error is the API's real body for `code` (fixtures/errors.json), sent verbatim. `edit` alters
  # it only for a scenario that plays a careless server (one that echoes the key).
  fail <- function(req, res, code, edit = identity, problems = NULL) {
    error <- req$app$locals$errors[[code]]
    # What the fake found wrong, for whoever debugs a test; never in the body.
    if (length(problems) > 0) res$set_header("X-Fake-Problems", paste(problems, collapse = "; "))
    res$set_status(error$status)$send_json(edit(error$body), auto_unbox = TRUE, null = "null", digits = NA)
  }
  authorised <- function(req, res) {
    token <- sub("^Bearer ", "", req$get_header("Authorization") %||% "")
    mode <- req$app$locals$world$mode
    if (isTRUE(mode$plain401) && !identical(token, req$app$locals$world$key)) {
      res$set_status(401)$set_type("text/plain")$send("Unauthorized")
      return(FALSE)
    }
    if (!identical(token, req$app$locals$world$key)) {
      # `leak`: a careless server echoes the token in the message and as a member name.
      echo <- function(body) c(utils::modifyList(body, list(message = paste(body$message, token))),
                               stats::setNames(list("x"), token))
      fail(req, res, "unauthorized", if (isTRUE(mode$leak)) echo else identity)
      return(FALSE)
    }
    TRUE
  }
  needs_user <- function(req, res) {
    if (is.null(req$get_header("X-OGDB-User"))) {
      fail(req, res, "user_required")
      return(FALSE)
    }
    TRUE
  }

  # Test control: switch a scenario on for the next requests, e.g. POST /_fake/paging/offset.
  app$post("/_fake/:setting/:value", function(req, res) {
    value <- req$params$value
    req$app$locals$world$mode[[req$params$setting]] <- if (value %in% c("true", "false")) value == "true" else value
    res$send_json(list(ok = TRUE), auto_unbox = TRUE)
  })

  # The job protocol. A Job's status follows the number of times it has been asked about: the
  # scenario's outcome at once, or, for `queued_running`, queued twice and running once first. The
  # rows are decided at submission. Resubmitting the same query (and scenario) returns the same Job
  # with `existing: true`, as the API deduplicates.
  app$locals$jobs <- new.env()
  app$locals$job_keys <- new.env()
  app$locals$bodies <- new.env()
  app$locals$gwasinfo <- new.env()
  app$locals$events <- new.env()
  app$locals$events$log <- list()
  # The array limits are part of the request contract; a test may lift them.
  body_problems <- function(req, body, path) {
    openapi <- req$app$locals$openapi
    if (isTRUE(req$app$locals$world$mode$lift_limits)) {
      strip <- function(x) {
        if (!is.list(x)) return(x)
        x$maxItems <- NULL
        lapply(x, strip)
      }
      openapi <- strip(openapi)
    }
    request_body_problems(body, path, "post", openapi)
  }
  # What was submitted to a Job endpoint, for tests of the request itself (GET /_fake/bodies).
  received <- function(req, endpoint, body) {
    assign(paste0("b", length(ls(req$app$locals$bodies))), list(endpoint = endpoint, body = body), envir = req$app$locals$bodies)
  }
  log_event <- function(req, what, id) {
    log <- req$app$locals$events$log
    req$app$locals$events$log[[length(log) + 1]] <- list(event = what, job_id = id)
  }
  busy_refusal <- function(req, res, route) {
    mode <- req$app$locals$world$mode
    if (!identical(mode$busy_route, route)) return(FALSE)
    if (identical(mode$busy, "none") || as.integer(mode$busy_times) <= 0L) return(FALSE)
    req$app$locals$world$mode$busy_times <- as.integer(mode$busy_times) - 1L
    res$set_header("Retry-After", as.character(mode$busy_after))
    fail(req, res, mode$busy)
    TRUE
  }
  job_status_body <- function(job, mode) {
    n <- job$polls
    scenario <- job$scenario
    waiting <- identical(scenario, "queued_running")
    status <- if (isTRUE(job$cancelled)) "cancelled" else if (waiting && n <= 2) "queued" else if (waiting && n == 3) "running" else switch(scenario,
      never = "running", failed_request = , failed_temporary = , failed_leak = "failed",
      cancelled = "cancelled", expired = "expired", bad_status = paste("weird", job$token), "succeeded")
    body <- list(job_id = job$id, status = status, endpoint = job$endpoint,
                 release_id = job$release_id, attempts = 1L,
                 queue_position = if (status == "queued") 4L - n else NULL,
                 poll_after_seconds = if (status %in% c("queued", "running")) job$poll_after else 0)
    if (status == "failed") {
      body$error <- switch(scenario, failed_leak = paste("worker rejected the query for", job$token),
                           "worker rejected the query")
      body$error_kind <- switch(scenario, failed_temporary = "temporary",
                                failed_leak = paste("request", job$token), "request")
    }
    if (status == "succeeded" && !identical(scenario, "no_result")) {
      rows <- length(job$rows) + if (identical(scenario, "short_rows")) 1L else 0L
      body$result <- list(
        release_id = job$release_id,
        manifest = list(row_count = rows, page_count = max(1L, ceiling(length(job$rows) / job$page_rows)),
                        releases = job$releases),
        result_url = paste0("/api/jobs/", job$id, "/result"))
      if (!identical(scenario, "no_metadata")) body$result$metadata <- job$metadata
      if (identical(scenario, "total_too_big")) body$result$metadata$total <- 99L
      if (identical(scenario, "total_too_small")) body$result$metadata$total <- 1L
    }
    list(status = status, body = body)
  }
  # Create (or find, for a resubmission) the Job for `rows` and answer 202.
  submit_job <- function(req, res, endpoint, body, rows, metadata) {
    world <- req$app$locals$world
    releases <- lapply(body$releases, function(r) list(store_id = r$store_id, release_id = r$release_id))
    token <- sub("^Bearer ", "", req$get_header("Authorization") %||% "")
    key <- paste(endpoint, world$mode$job, world$mode$page_rows, as.character(jsonlite::toJSON(body, auto_unbox = TRUE)))
    # Long bodies (1,000 variants) can't be variable names: hash them.
    key <- paste0("k", rlang::hash(key))
    existing <- exists(key, envir = req$app$locals$job_keys, inherits = FALSE)
    if (existing) {
      job <- get(get(key, envir = req$app$locals$job_keys), envir = req$app$locals$jobs)
    } else {
      id <- paste(sample(c(0:9, letters[1:6]), 32, TRUE), collapse = "")
      id <- paste(substring(id, c(1, 9, 13, 17, 21), c(8, 12, 16, 20, 32)), collapse = "-")
      job <- list(id = id, polls = 0L, scenario = world$mode$job, rows = rows, releases = releases,
                  release_id = paste(vapply(releases, function(r) paste0(r$store_id, "/", r$release_id), ""),
                                     collapse = ","),
                  page_rows = as.integer(world$mode$page_rows),
                  poll_after = as.numeric(world$mode$poll_after_ms) / 1000, token = token,
                  metadata = metadata, endpoint = endpoint,
                  served = character(), times = numeric())
      assign(id, job, envir = req$app$locals$jobs)
      assign(key, id, envir = req$app$locals$job_keys)
    }
    log_event(req, "submit", job$id)
    ignored <- if (identical(job$scenario, "ignored")) list("observed_onyl", token) else list()
    res$set_status(202)$set_header("Retry-After", "1")$set_header("Location", paste0("/api/jobs/", job$id))
    res$set_header("X-OGDB-Cost-Estimate", "1")
    res$send_json(list(job_id = job$id, status_url = paste0("/api/jobs/", job$id),
                       status = if (existing) job_status_body(job)$status else "queued",
                       query_hash = "sha256:fake", request_fingerprint = "sha256:fake", existing = existing,
                       queue_position = NULL, poll_after_seconds = job$poll_after,
                       ignored_fields = ignored),
                  auto_unbox = TRUE, null = "null")
  }
  # In the parity world (fake_parity_world()), a golden Job's generated answer: its rows and
  # metadata as a Job, or the API's error, with the body the API builds (`detail` repeating it, as
  # fixtures/errors.json records). TRUE when it answered. Each Job route asks it first, before its
  # own checks, so a golden query is always answered from answers.json, its refusals included.
  parity_job <- function(req, res, endpoint, body) {
    world <- req$app$locals$world
    if (!isTRUE(world$mode$parity)) return(FALSE)
    key <- parity_key(endpoint, body)
    found <- world$parity[[key]]
    if (is.null(found)) {
      res$set_status(500)$set_type("text/plain")$send(paste("The parity fake has no generated answer for", key))
    } else if (!is.null(found$error)) {
      error <- found$error
      res$set_status(error$status)$send_json(c(error$body, list(detail = error$body)), auto_unbox = TRUE,
                                             null = "null", digits = NA)
    } else {
      submit_job(req, res, endpoint, body, found$rows, if (length(found$metadata) > 0) found$metadata)
    }
    TRUE
  }
  app$post("/api/tophits", function(req, res) {
    if (!authorised(req, res) || !needs_user(req, res)) return()
    if (busy_refusal(req, res, "submit")) return()
    world <- req$app$locals$world
    body <- req$json %||% list()
    received(req, "/api/tophits", body)
    if (parity_job(req, res, "/api/tophits", body)) return()
    problems <- body_problems(req, body, "/api/tophits")
    if (length(problems) > 0) return(fail(req, res, "invalid_request", problems = problems))
    threshold <- body$threshold %||% 5e-8
    # Each Store Release's tiers are checked when the Job is submitted (query_operations.py:101-112):
    # none at or above the threshold is refused, naming the first such release's GWAS Store.
    if (!any(fake_top_hit_tiers >= threshold)) {
      store <- body$releases[[1]]$store_id %||% "configured"
      message <- paste0("threshold must be at most ", formatC(max(fake_top_hit_tiers), format = "g"),
                        ", the loosest p-value the Top-Hit Index of ", store, " holds")
      return(fail(req, res, "unsupported_threshold", edit = function(b) {
        b$message <- b$detail$message <- message
        b$indexed_thresholds <- b$detail$indexed_thresholds <- as.list(fake_top_hit_tiers)
        b
      }))
    }
    limit <- body$limit %||% 1000L
    ids <- unlist(body$analyses)
    # Clumped (`clump`): every hit at the threshold is clumped in the LD Store (by default EUR, r^2
    # 0.001 within 3,000 kb, as the API's own defaults are), and `limit` counts the leads.
    clump <- body$clump
    if (!is.null(clump)) {
      if (length(ids) == 0) return(fail(req, res, "invalid_request", problems = "clump needs analyses"))
      store <- ld_store_of(req, res, clump)
      if (is.null(store)) return()
      clump$r2 <- clump$r2 %||% 0.001
      clump$kb <- clump$kb %||% 3000
      if (ld_refusal(req, res, store, clump)) return()
      counts <- list(hits = 0L, leads = 0L, absent = 0L)
    }
    rows <- list()
    truncated <- FALSE
    for (id in unique(ids)) {
      hits <- world$tophits[[id]] %||% list()
      hits <- Filter(function(h) h$association_status == "observed" || !isTRUE(body$observed_only), hits)
      hits <- Filter(function(h) fake_clears(h, threshold), hits)
      # Each Analysis's strongest first, ties in genome order, before `limit` (top_hits.py:18-37).
      hits <- fake_api_order(hits, strongest = TRUE)
      if (!is.null(clump)) {
        held <- Filter(function(h) fake_ld_holds(h$alid), hits)
        alids <- vapply(held, function(h) h$alid, "")
        clumped <- fake_clump(alids, vapply(held, function(h) abs(h$z), 0), store, clump$r2, clump$kb)
        leads <- held[clumped$lead == alids]
        leads <- lapply(leads, function(h) c(h, list(clump_size = sum(clumped$lead == h$alid))))
        counts <- list(hits = counts$hits + length(hits), leads = counts$leads + length(leads),
                       absent = counts$absent + length(hits) - length(held))
        hits <- leads
      }
      if (length(hits) > limit) truncated <- TRUE
      rows <- c(rows, utils::head(hits, limit))
    }
    if (is.null(clump)) rows <- fake_api_order(rows, strongest = TRUE)
    metadata <- list(threshold = threshold, observed_only = isTRUE(body$observed_only),
                     total = length(rows), truncated = truncated)
    if (!is.null(clump)) {
      metadata$clump <- c(list(r2 = clump$r2, kb = clump$kb), counts,
                          list(ld_stores = stats::setNames(list(store$store), store$ancestry)))
    }
    submit_job(req, res, "/api/tophits", body, rows, metadata)
  })
  # Variants in Analyses. `variants` are rsIDs or ALIDs, as the API takes them; an rsID stands for
  # every ALID it names. Rows come in the API's order (fake_api_order()), each Association once.
  app$post("/api/associations", function(req, res) {
    if (!authorised(req, res) || !needs_user(req, res)) return()
    world <- req$app$locals$world
    body <- req$json %||% list()
    received(req, "/api/associations", body)
    if (parity_job(req, res, "/api/associations", body)) return()
    problems <- body_problems(req, body, "/api/associations")
    if (length(problems) > 0) return(fail(req, res, "invalid_request", problems = problems))
    # The real API also expands rsIDs. The fake does not, so a client that skips resolving them gets
    # no rows (and the tests that look at the submitted body see ALIDs only).
    alids <- unlist(body$variants)
    rows <- list()
    for (id in unique(unlist(body$analyses %||% body$ids))) {
      hits <- Filter(function(h) h$alid %in% alids, world$associations[[id]] %||% list())
      # The API's own default is to leave Imputed Associations out.
      hits <- Filter(function(h) h$association_status == "observed" || !isTRUE(body$observed_only %||% TRUE), hits)
      rows <- c(rows, hits)
    }
    submit_job(req, res, "/api/associations", body, fake_distinct(fake_api_order(rows)), NULL)
  })
  # rsIDs to the variants they name, in each Store Release asked for; one row per ALID, so a
  # multiallelic rsID gives several. An rsID no variant table holds gives no row.
  app$post("/api/variants/rsid", function(req, res) {
    if (!authorised(req, res) || !needs_user(req, res)) return()
    world <- req$app$locals$world
    body <- req$json %||% list()
    received(req, "/api/variants/rsid", body)
    if (parity_job(req, res, "/api/variants/rsid", body)) return()
    problems <- body_problems(req, body, "/api/variants/rsid")
    if (length(problems) > 0) return(fail(req, res, "invalid_request", problems = problems))
    wanted <- vapply(body$releases, function(r) paste0(r$store_id, "/", r$release_id), "")
    rows <- Filter(function(v) v$rsid %in% unlist(body$rsids) &&
                     paste0(v$store_id, "/", v$release_id) %in% wanted, world$variants)
    submit_job(req, res, "/api/variants/rsid", body, rows, NULL)
  })

  # LD. `GET /api/ld/stores`, and Jobs for the matrix and proxies, which name an LD Store by `store`
  # or `ancestry` (never a fallback: a request naming neither is answered from EUR, as the API does).
  ld_served <- function(req) {
    stores <- req$app$locals$world$ld_stores
    mode <- req$app$locals$world$mode
    if (isTRUE(mode$ld_two_eur)) {
      second <- stores[[1]]
      second$store <- "hgdp1kgp-v2-EUR-k10-b"
      stores <- c(stores, list(second))
    }
    if (isTRUE(mode$ld_not_ready)) {
      stores <- lapply(stores, function(s) { if (s$ancestry == "AMR") { s$ready <- FALSE; s$error <- "still building" }; s })
    }
    if (isTRUE(mode$ld_declared_default)) {
      stores <- lapply(stores, function(s) { s$default <- !endsWith(s$store, "-b"); s })
    }
    stores
  }
  ld_store_of <- function(req, res, body) {
    named <- body$store %||% NULL
    found <- Filter(function(s) identical(s$store, named) || (is.null(named) && identical(s$ancestry, body$ancestry %||% "EUR")),
                    ld_served(req))
    if (length(found) == 0) {
      fail(req, res, "invalid_request")
      return(NULL)
    }
    found[[1]]
  }
  app$get("/api/ld/stores", function(req, res) {
    if (!authorised(req, res) || !needs_user(req, res)) return()
    res$send_json(list(stores = ld_served(req)), auto_unbox = TRUE, null = "null", digits = NA)
  })
  # The 422 a store gives for a request it can't answer exactly (`r2` below `t_far` over more than `near_bp`).
  ld_refusal <- function(req, res, store, body) {
    if (is.numeric(body$r2) && is.numeric(body$kb) && body$r2 < store$t_far && body$kb * 1000 > store$near_bp) {
      error <- req$app$locals$errors[["ld_refusal"]]
      error$body[c("store", "ancestry", "t_far", "near_bp", "far_bp")] <-
        store[c("store", "ancestry", "t_far", "near_bp", "far_bp")]
      res$set_status(error$status)$send_json(error$body, auto_unbox = TRUE, null = "null", digits = NA)
      return(TRUE)
    }
    FALSE
  }
  ld_job <- function(req, res, endpoint, make_rows) {
    if (!authorised(req, res) || !needs_user(req, res)) return()
    if (busy_refusal(req, res, "submit")) return()
    body <- req$json %||% list()
    received(req, endpoint, body)
    problems <- body_problems(req, body, endpoint)
    if (length(problems) > 0) return(fail(req, res, "invalid_request", problems = problems))
    store <- ld_store_of(req, res, body)
    if (is.null(store)) return()
    if (identical(endpoint, "/api/ld/matrix") && length(body$alids) > 500 && !isTRUE(req$app$locals$world$mode$lift_limits)) {
      return(fail(req, res, "invalid_request", problems = "at most 500 alids"))
    }
    if (endpoint %in% c("/api/ld/proxies", "/api/ld/clump") && ld_refusal(req, res, store, body)) return()
    alids <- unlist(body$alids)
    held <- alids[vapply(alids, fake_ld_holds, NA)]
    made <- make_rows(held, store, body)
    metadata <- c(list(store = store$store, ancestry = store$ancestry, t_far = store$t_far, near_bp = store$near_bp,
                       far_bp = store$far_bp, absent = as.list(setdiff(alids, held))), made$metadata)
    # The real API reports the LD Store as the Store Release a Job read.
    read <- list(list(store_id = store$store, release_id = store$store))
    submit_job(req, res, endpoint, c(body, list(releases = read)), made$rows, metadata)
  }
  app$post("/api/ld/matrix", function(req, res) {
    ld_job(req, res, "/api/ld/matrix", function(held, store, body) {
      rows <- lapply(held, function(a) {
        parts <- strsplit(a, ":", fixed = TRUE)[[1]]
        list(a1 = parts[[3]], a2 = parts[[4]], alid = a, chromosome = parts[[1]], position = as.integer(parts[[2]]),
             r = lapply(held, function(b) { r <- fake_ld_r(a, b, store); if (is.na(r)) NULL else r }))
      })
      list(rows = rows, metadata = list(variants = length(held)))
    })
  })
  # Clumping a set of variants by their scores: every held variant's row, with its lead and r with it.
  app$post("/api/ld/clump", function(req, res) {
    ld_job(req, res, "/api/ld/clump", function(held, store, body) {
      alids <- unlist(body$alids)
      scores <- unlist(body$scores)[match(held, alids)]
      clumped <- fake_clump(held, scores, store, body$r2, body$kb)
      rows <- lapply(seq_along(held), function(i) {
        parts <- strsplit(held[[i]], ":", fixed = TRUE)[[1]]
        list(alid = held[[i]], chromosome = parts[[1]], lead = clumped$lead[[i]],
             position = as.integer(parts[[2]]), r = clumped$r[[i]], score = scores[[i]])
      })
      list(rows = rows, metadata = list(r2 = body$r2, kb = body$kb, window_bp = body$kb * 1000,
                                        leads = length(unique(clumped$lead)), variants = length(held)))
    })
  })
  # Proxies of a held variant: held variants 100 bp either side and 250 bp and 300 bp away, with r of
  # sign(offset) * (1 - |offset| / 2000) (0.95, 0.875, 0.85), kept at r^2 >= `r2` within `kb`,
  # strongest first. Not the matrix's r: the fake only needs proxies a test can state by hand.
  app$post("/api/ld/proxies", function(req, res) {
    ld_job(req, res, "/api/ld/proxies", function(held, store, body) {
      rows <- list()
      for (a in held) {
        parts <- strsplit(a, ":", fixed = TRUE)[[1]]
        for (offset in c(-100, 100, 250, -300)) {
          proxy <- paste(parts[[1]], format(as.numeric(parts[[2]]) + offset, scientific = FALSE), "A", "G", sep = ":")
          r <- sign(offset) * (1 - abs(offset) / 2000)
          if (!fake_ld_holds(proxy) || identical(a, proxy) || r^2 < body$r2 || abs(offset) > body$kb * 1000) next
          rows[[length(rows) + 1]] <- list(distance = abs(offset), proxy = proxy, query = a, r = r, r2 = r^2)
        }
      }
      list(rows = rows, metadata = list(r2 = body$r2, window_bp = body$kb * 1000, variants = length(held)))
    })
  })
  # Whether the LD Store holds each ALID asked: one row each, in the order asked.
  app$post("/api/ld/reflookup", function(req, res) {
    ld_job(req, res, "/api/ld/reflookup", function(held, store, body) {
      list(rows = lapply(unlist(body$alids), function(a) list(alid = a, present = a %in% held)),
           metadata = list(variants = length(held)))
    })
  })
  # One variant across Analyses (`variant` is an ALID: opengwasr resolves rsIDs itself). The rows are
  # the variant's Associations in the Analyses of the Store Releases asked for (or of `analyses`) at
  # p <= `threshold` (fake_clears()), at most `limit`, in the API's order; `total` counts those that
  # pass.
  app$post("/api/phewas", function(req, res) {
    if (!authorised(req, res) || !needs_user(req, res)) return()
    world <- req$app$locals$world
    body <- req$json %||% list()
    received(req, "/api/phewas", body)
    if (parity_job(req, res, "/api/phewas", body)) return()
    problems <- body_problems(req, body, "/api/phewas")
    if (length(problems) > 0) return(fail(req, res, "invalid_request", problems = problems))
    threshold <- body$threshold %||% 1e-8
    limit <- body$limit %||% 1000L
    held <- world$associations[unique(unlist(body$analyses)) %||% names(world$associations)]
    # Store Release by Store Release (query_operations.py:155-172): its Associations of the variant
    # at the threshold, counted into `total`, its first `limit` kept; then all of them in the API's
    # order, cut to `limit`.
    total <- 0L
    rows <- list()
    for (release in body$releases) {
      found <- list()
      for (id in names(held)) {
        found <- c(found, Filter(function(h) {
          h$alid == body$variant && identical(h$store_id, release$store_id) && identical(h$release_id, release$release_id) &&
            fake_clears(h, threshold) && (h$association_status == "observed" || !isTRUE(body$observed_only %||% TRUE))
        }, held[[id]]))
      }
      total <- total + length(found)
      rows <- c(rows, utils::head(found, limit))
    }
    rows <- utils::head(fake_api_order(rows), limit)
    submit_job(req, res, "/api/phewas", body, rows,
               list(variant = body$variant, threshold = threshold, observed_only = isTRUE(body$observed_only %||% TRUE),
                    total = total, returned = length(rows), truncated = total > length(rows)))
  })
  # Every Association in a window of one chromosome, in the Analyses asked for, in the API's order. A
  # chromosome other than 1-22, X, Y or MT, and a window over 10 Mb, are refused as the API refuses
  # them (the window unless limits are lifted).
  app$post("/api/associations/range", function(req, res) {
    if (!authorised(req, res) || !needs_user(req, res)) return()
    world <- req$app$locals$world
    body <- req$json %||% list()
    received(req, "/api/associations/range", body)
    if (parity_job(req, res, "/api/associations/range", body)) return()
    problems <- body_problems(req, body, "/api/associations/range")
    if (length(problems) > 0) return(fail(req, res, "invalid_request", problems = problems))
    # The chromosome is checked with the body, before anything else: refused as a malformed request.
    if (!fake_primary_chromosome(body$chromosome)) {
      return(fail(req, res, "invalid_request", problems = "chromosome is not one of 1-22, X, Y or MT"))
    }
    if (!isTRUE(world$mode$lift_limits) && body$end - body$start + 1 > 1e7) return(fail(req, res, "request_too_large"))
    rows <- list()
    for (id in unique(unlist(body$analyses %||% body$ids))) {
      hits <- Filter(function(h) h$chromosome == body$chromosome && h$position >= body$start && h$position <= body$end &&
                       (h$association_status == "observed" || !isTRUE(body$observed_only %||% TRUE)),
                     c(world$associations[[id]], world$ranges[[id]]))
      rows <- c(rows, hits)
    }
    submit_job(req, res, "/api/associations/range", body, fake_distinct(fake_api_order(rows)), NULL)
  })
  # Positions (`chromosome:position`, a `chr` prefix optional) to the variants there, in each Store
  # Release asked for: one row per ALID, so a position holding two alleles gives several.
  app$post("/api/variants/chrpos", function(req, res) {
    if (!authorised(req, res) || !needs_user(req, res)) return()
    world <- req$app$locals$world
    body <- req$json %||% list()
    received(req, "/api/variants/chrpos", body)
    if (parity_job(req, res, "/api/variants/chrpos", body)) return()
    problems <- body_problems(req, body, "/api/variants/chrpos")
    if (length(problems) > 0) return(fail(req, res, "invalid_request", problems = problems))
    wanted <- vapply(body$releases, function(r) paste0(r$store_id, "/", r$release_id), "")
    positions <- sub("^chr", "", unlist(body$positions))
    rows <- Filter(function(v) paste0(v$chromosome, ":", v$position) %in% positions &&
                     paste0(v$store_id, "/", v$release_id) %in% wanted, world$variants)
    submit_job(req, res, "/api/variants/chrpos", body, rows, NULL)
  })
  app$get("/api/jobs/:id", function(req, res) {
    if (!authorised(req, res) || !needs_user(req, res)) return()
    if (busy_refusal(req, res, "status")) return()
    # A look at Job `refuse-<code>` is answered with that error code's real body.
    if (startsWith(req$params$id, "refuse-")) return(fail(req, res, sub("^refuse-", "", req$params$id)))
    env <- req$app$locals$jobs
    if (!exists(req$params$id, envir = env, inherits = FALSE)) return(fail(req, res, "not_found"))
    arrived <- as.numeric(Sys.time())
    job <- get(req$params$id, envir = env)
    job$polls <- job$polls + 1L
    out <- job_status_body(job, req$app$locals$world$mode)
    if (identical(out$status, "succeeded")) log_event(req, "done", job$id)
    job$served <- c(job$served, out$status)
    job$times <- c(job$times, arrived)
    assign(job$id, job, envir = env)
    delay <- as.numeric(req$app$locals$world$mode$delay_ms) / 1000
    if (delay > 0) Sys.sleep(delay)
    res$send_json(out$body, auto_unbox = TRUE, null = "null")
  })
  app$delete("/api/jobs/:id", function(req, res) {
    if (!authorised(req, res) || !needs_user(req, res)) return()
    env <- req$app$locals$jobs
    if (!exists(req$params$id, envir = env, inherits = FALSE)) return(fail(req, res, "not_found"))
    job <- get(req$params$id, envir = env)
    # A finished Job is returned unchanged: one asked about at least once has finished (`normal`).
    if (identical(job$scenario, "normal") && job$polls > 0L) {
      return(res$send_json(job_status_body(job, NULL)$body, auto_unbox = TRUE, null = "null"))
    }
    job$cancelled <- TRUE
    log_event(req, "cancel", job$id)
    assign(job$id, job, envir = env)
    res$send_json(job_status_body(job, NULL)$body, auto_unbox = TRUE, null = "null")
  })
  app$get("/api/jobs/:id/result", function(req, res) {
    if (!authorised(req, res) || !needs_user(req, res)) return()
    if (busy_refusal(req, res, "result")) return()
    env <- req$app$locals$jobs
    if (!exists(req$params$id, envir = env, inherits = FALSE)) return(fail(req, res, "not_found"))
    job <- get(req$params$id, envir = env)
    delay <- as.numeric(req$app$locals$world$mode$delay_ms) / 1000
    if (delay > 0) Sys.sleep(delay)
    if (identical(job$scenario, "result_gone")) return(fail(req, res, "result_expired"))
    page <- as.integer(sub("^page:", "", req$query$cursor %||% "page:0"))
    first <- page * job$page_rows
    rows <- job$rows[seq_along(job$rows) > first & seq_along(job$rows) <= first + job$page_rows]
    final <- first + job$page_rows >= length(job$rows)
    # Every number so that it reads back as the double the API's json.dumps() sends: 17 significant
    # digits (`digits = NA` gives only 15, which loses the last digits; json.dumps writes the shortest
    # form that reads back, 0.1 where this writes 0.10000000000000001, the same double), and a whole
    # double with its decimal point (-9.0, which jsonlite reads as a double, not the integer -9).
    lines <- vapply(rows, function(r) as.character(jsonlite::toJSON(r, auto_unbox = TRUE, null = "null", digits = I(17),
                                                                    always_decimal = TRUE)), "")
    out <- list(page_index = page, row_count = length(rows), ndjson = paste(lines, collapse = "\n"),
                next_cursor = if (final) NULL else paste0("page:", page + 1L), final = final)
    if (identical(job$scenario, "no_ndjson")) out$ndjson <- NULL
    if (identical(job$scenario, "bad_ndjson")) out$ndjson <- paste("{not json", job$token)
    res$send_json(out, auto_unbox = TRUE, null = "null")
  })
  # When the fake answered each Job's status requests (seconds), for tests of the waiting.
  # The bodies submitted to Job endpoints, oldest first.
  app$get("/_fake/gwasinfo", function(req, res) {
    env <- req$app$locals$gwasinfo
    keys <- ls(env)
    keys <- keys[order(as.integer(sub("^g", "", keys)))]
    res$send_json(lapply(keys, function(k) get(k, envir = env)), auto_unbox = TRUE, null = "null")
  })
  app$get("/_fake/bodies", function(req, res) {
    env <- req$app$locals$bodies
    keys <- ls(env)
    keys <- keys[order(as.integer(sub("^b", "", keys)))]
    res$send_json(lapply(keys, function(k) get(k, envir = env)), auto_unbox = TRUE, null = "null", digits = NA)
  })
  # Submissions, completions (a status look that found the Job succeeded), cancellations, in the order they happened.
  app$get("/_fake/events", function(req, res) {
    res$send_json(req$app$locals$events$log, auto_unbox = TRUE)
  })
  app$get("/_fake/jobs", function(req, res) {
    env <- req$app$locals$jobs
    times <- lapply(stats::setNames(nm = ls(env)), function(id) as.list(get(id, envir = env)$times))
    res$send_json(list(times = times), auto_unbox = TRUE)
  })
  app$get("/api/status", function(req, res) {
    delay <- as.numeric(req$app$locals$world$mode$delay_ms) / 1000
    if (delay > 0) Sys.sleep(delay)
    res$send_json(req$app$locals$world$status, auto_unbox = TRUE, null = "null")
  })
  app$get("/api/user", function(req, res) {
    if (!authorised(req, res)) return()
    user <- req$app$locals$world$user
    user$user <- paste0("user:", req$get_header("X-OGDB-User") %||% "unknown")
    if (isTRUE(req$app$locals$world$mode$malformed)) return(res$set_type("application/json")$send("{not json"))
    res$send_json(user, auto_unbox = TRUE, null = "null")
  })
  app$get("/api/batches", function(req, res) {
    if (!authorised(req, res)) return()
    batches <- lapply(served_releases(req$app$locals$world), function(r) c(r, list(analysis_count = 0L)))
    res$send_json(list(batches = batches, not_served = list()), auto_unbox = TRUE, null = "null")
  })

  gwasinfo <- function(req, res) {
    if (!authorised(req, res) || !needs_user(req, res)) return()
    world <- req$app$locals$world
    body <- req$json %||% list()
    if (identical(toupper(req$method), "POST")) {
      # The ids asked for, for tests of which Analyses a query looked up (GET /_fake/gwasinfo).
      assign(paste0("g", length(ls(req$app$locals$gwasinfo))), body, envir = req$app$locals$gwasinfo)
      problems <- request_body_problems(body, "/api/gwasinfo", "post", req$app$locals$openapi)
      if (length(problems) > 0) return(fail(req, res, "invalid_request", problems = problems))
    }
    if (isTRUE(world$mode$malformed)) return(res$set_type("application/json")$send("{not json"))
    if (isTRUE(world$mode$no_analyses)) return(res$set_type("application/json")$send("{}"))
    if (isTRUE(world$mode$leak)) {
      token <- sub("^Bearer ", "", req$get_header("Authorization") %||% "")
      world$legacy[["ieu-a-2"]]$reason <- paste("OpenGWAS batch ieu-a is not held by any Store; asked by", token)
    }
    analyses <- c(world$analyses, if (isTRUE(world$mode$many_stores)) world$many_analyses,
                  if (isTRUE(world$mode$binary_sd)) world$binary_sd,
                  if (isTRUE(world$mode$afr_analysis)) world$afr_analysis)
    ids <- unlist(body$id %||% body$ids)
    if (!is.null(body$releases)) {
      wanted <- vapply(body$releases, function(r) paste0(r$store_id, "/", r$release_id), "")
      served <- vapply(served_releases(world), function(r) paste0(r$store_id, "/", r$release_id), "")
      unknown <- setdiff(wanted, served)
      if (length(unknown) > 0) {
        return(fail(req, res, "release_not_found"))
      }
      analyses <- Filter(function(a) paste0(a$store_id, "/", a$release_id) %in% wanted, analyses)
    }
    release_id <- paste(vapply(served_releases(world), function(r) paste0(r$store_id, "/", r$release_id), ""),
                        collapse = ",")

    if (!is.null(ids)) {
      if (length(ids) > 1000) return(fail(req, res, "invalid_request"))
      found <- fake_lookup(world, analyses, unique(ids), release_id)
      return(res$send_json(text = fake_analyses_json(found, found$analyses)))
    }
    limit <- as.integer(body$limit %||% req$query$limit %||% 100)
    after <- body$after %||% req$query$after
    offset <- as.integer(body$offset %||% 0)
    all_ids <- vapply(analyses, function(a) a$id, "")
    mode <- world$mode$paging
    start <- if (!is.null(after)) match(after, all_ids, nomatch = 0L) else offset
    if (mode == "truncating" && start > 0) analyses <- list()
    page <- analyses[seq_along(analyses) > start]
    more <- length(page) > limit
    page <- utils::head(page, limit)
    # "cursor": next_after, as the real API. "offset": total only. "truncating": total only, and
    # offset is ignored (a broken API that can't be completed).
    next_after <- if (mode == "cursor" && more) page[[length(page)]]$id else NULL
    res$send_json(text = fake_analyses_json(
      list(release_id = release_id, catalog_version = "fake", offset = start, after = after,
           limit = limit, total = length(all_ids), analyses = NULL, next_after = next_after), page))
  }
  app$get("/api/gwasinfo", gwasinfo)
  app$post("/api/gwasinfo", gwasinfo)
  app
}

# The JSON text of `body`, whose `analyses` member is `rows` (a list of Analysis rows): byte for byte what
# `jsonlite::toJSON(body, auto_unbox = TRUE, null = "null", digits = NA)` writes. Serialising a row is the
# slow part of answering a catalogue page, and the same rows are served again and again, so each row is
# serialised once (keyed by its content) and the page is assembled from the pieces. `body` has an
# `analyses` member, whose value is replaced.
fake_analyses_json <- local({
  cache <- new.env()
  function(body, rows) {
    pieces <- vapply(rows, function(row) {
      key <- rlang::hash(row)
      cache[[key]] %||% (cache[[key]] <- unclass(jsonlite::toJSON(row, auto_unbox = TRUE, null = "null", digits = NA)))
    }, "", USE.NAMES = FALSE)
    body["analyses"] <- list("@@analyses@@")
    text <- unclass(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null", digits = NA))
    sub("\"@@analyses@@\"", paste0("[", paste(pieces, collapse = ","), "]"), text, fixed = TRUE)
  }
})

# POST /api/gwasinfo with `id`, as the real API answers it: one row per Analysis however many ids
# name it, legacy OpenGWAS ids resolved through `world$legacy`, retired and unknown ids listed.
fake_lookup <- function(world, analyses, ids, release_id) {
  row_ids <- vapply(analyses, function(a) a$id, "")
  row_analysis_ids <- vapply(analyses, function(a) a$analysis_id, "")
  found_rows <- integer()
  not_found <- character()
  retired <- list()
  opengwas <- list()
  retired_ids <- vapply(world$retired, function(r) r$id, "")
  for (id in ids) {
    if (id %in% retired_ids) {
      retired[[length(retired) + 1]] <- world$retired[[match(id, retired_ids)]]
      next
    }
    legacy <- world$legacy[[id]]
    if (!is.null(legacy)) {
      entry <- c(list(id = id, batch = sub("^([a-z]+-[a-z]+)-.*", "\\1", id)),
                 list(catalog_ids = legacy$catalog_ids %||% list()), legacy["match"], legacy["reason"])
      opengwas[[length(opengwas) + 1]] <- entry[lengths(entry) > 0 | names(entry) == "catalog_ids"]
      hits <- which(row_ids %in% unlist(legacy$catalog_ids))
    } else if (grepl(":", id, fixed = TRUE)) {
      hits <- which(row_ids == id)
    } else {
      hits <- which(row_analysis_ids == id)
    }
    if (length(hits) == 0) not_found <- c(not_found, id)
    found_rows <- c(found_rows, hits)
  }
  rows <- analyses[unique(found_rows)]
  out <- list(release_id = release_id, requested = length(ids), total = length(rows), analyses = rows,
              not_found = as.list(not_found), catalog_version = "fake", retired = retired)
  if (length(opengwas) > 0) out$opengwas <- opengwas
  out
}

