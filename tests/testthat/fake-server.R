# The fake Data API's server side. Self-contained (base R, jsonlite, webfakes), because webfakes
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
fake_world <- function(n_synthetic = 1001) {
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
  # Limits: the Job routes enforce the contract's array limits (100 Analyses, 1,000 variants, 10 Store
  # Releases) as the API does; `lift_limits` lifts them, for the reference run of a split query.
  world$mode <- list(paging = "cursor", lift_limits = FALSE, malformed = FALSE, two_releases = FALSE, leak = FALSE,
                     plain401 = FALSE, no_analyses = FALSE, many_stores = FALSE, job = "normal",
                     page_rows = 1000L, poll_after_ms = 10, delay_ms = 0,
                     busy = "none", busy_times = 0L, busy_after = 3L, busy_route = "submit")
  world$key <- fake_key
  world
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
    )
  )
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

# 1 to 22, then X, Y and MT, as the API orders genome positions.
fake_chromosome_rank <- function(chromosome) {
  rank <- suppressWarnings(as.numeric(chromosome))
  if (is.na(rank)) rank <- c(X = 23, Y = 24, MT = 25)[[chromosome]]
  rank
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
      cad("16:53800000:A:G", "rs9939609", 0.3, 4, 0.01, -4.2, "observed")
    ),
    "OGS-00011:GCST90029007" = list(
      bmi("5:75701728:A:AAAAT", "rs34159589", 0.37, -12, 0.002, -33, "imputed"),
      bmi("16:53767042:C:T", "rs1421085", 0.38, 38, 0.002, -300, "observed"),
      bmi("18:60161902:C:T", "rs6567160", 0.25, 24, 0.002, -125, "observed"),
      # Not in the dev Data API: the one Analysis that holds both ALIDs of a multiallelic rsID.
      # The API sends rsid "." where a variant has no rsID.
      bmi("16:53767042:A:G", ".", 0.62, -38, 0.002, -300, "observed")
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
  app$post("/api/tophits", function(req, res) {
    if (!authorised(req, res) || !needs_user(req, res)) return()
    if (busy_refusal(req, res, "submit")) return()
    world <- req$app$locals$world
    body <- req$json %||% list()
    received(req, "/api/tophits", body)
    problems <- body_problems(req, body, "/api/tophits")
    if (length(problems) > 0) return(fail(req, res, "invalid_request", problems = problems))
    threshold <- body$threshold %||% 5e-8
    if (threshold > 5e-4) return(fail(req, res, "unsupported_threshold"))
    limit <- body$limit %||% 1000L
    ids <- unlist(body$analyses)
    rows <- list()
    truncated <- FALSE
    for (id in ids) {
      hits <- world$tophits[[id]] %||% list()
      hits <- Filter(function(h) h$association_status == "observed" || !isTRUE(body$observed_only), hits)
      hits <- Filter(function(h) 10^h$log10_p <= threshold || h$log10_p < -300, hits)
      if (length(hits) > limit) truncated <- TRUE
      rows <- c(rows, utils::head(hits, limit))
    }
    submit_job(req, res, "/api/tophits", body, rows,
               list(threshold = threshold, observed_only = isTRUE(body$observed_only),
                    total = length(rows), truncated = truncated))
  })
  # Variants in Analyses. `variants` are rsIDs or ALIDs, as the API takes them; an rsID stands for
  # every ALID it names. Rows come in the order of the Analyses asked for, then genome position .
  app$post("/api/associations", function(req, res) {
    if (!authorised(req, res) || !needs_user(req, res)) return()
    world <- req$app$locals$world
    body <- req$json %||% list()
    received(req, "/api/associations", body)
    problems <- body_problems(req, body, "/api/associations")
    if (length(problems) > 0) return(fail(req, res, "invalid_request", problems = problems))
    # The real API also expands rsIDs. The fake does not, so a client that skips resolving them gets
    # no rows (and the tests that look at the submitted body see ALIDs only).
    alids <- unlist(body$variants)
    rows <- list()
    for (id in unlist(body$analyses %||% body$ids)) {
      hits <- Filter(function(h) h$alid %in% alids, world$associations[[id]] %||% list())
      # The API's own default is to leave Imputed Associations out.
      hits <- Filter(function(h) h$association_status == "observed" || !isTRUE(body$observed_only %||% TRUE), hits)
      rows <- c(rows, hits[order(vapply(hits, function(h) fake_chromosome_rank(h$chromosome) * 1e9 + h$position, 0))])
    }
    submit_job(req, res, "/api/associations", body, rows, NULL)
  })
  # rsIDs to the variants they name, in each Store Release asked for; one row per ALID, so a
  # multiallelic rsID gives several. An rsID no variant table holds gives no row.
  app$post("/api/variants/rsid", function(req, res) {
    if (!authorised(req, res) || !needs_user(req, res)) return()
    world <- req$app$locals$world
    body <- req$json %||% list()
    received(req, "/api/variants/rsid", body)
    problems <- body_problems(req, body, "/api/variants/rsid")
    if (length(problems) > 0) return(fail(req, res, "invalid_request", problems = problems))
    wanted <- vapply(body$releases, function(r) paste0(r$store_id, "/", r$release_id), "")
    rows <- Filter(function(v) v$rsid %in% unlist(body$rsids) &&
                     paste0(v$store_id, "/", v$release_id) %in% wanted, world$variants)
    submit_job(req, res, "/api/variants/rsid", body, rows, NULL)
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
    lines <- vapply(rows, function(r) as.character(jsonlite::toJSON(r, auto_unbox = TRUE, null = "null", digits = NA)), "")
    out <- list(page_index = page, row_count = length(rows), ndjson = paste(lines, collapse = "\n"),
                next_cursor = if (final) NULL else paste0("page:", page + 1L), final = final)
    if (identical(job$scenario, "no_ndjson")) out$ndjson <- NULL
    if (identical(job$scenario, "bad_ndjson")) out$ndjson <- paste("{not json", job$token)
    res$send_json(out, auto_unbox = TRUE, null = "null")
  })
  # When the fake answered each Job's status requests (seconds), for tests of the waiting.
  # The bodies submitted to Job endpoints, oldest first.
  app$get("/_fake/bodies", function(req, res) {
    env <- req$app$locals$bodies
    keys <- ls(env)
    keys <- keys[order(as.integer(sub("^b", "", keys)))]
    res$send_json(lapply(keys, function(k) get(k, envir = env)), auto_unbox = TRUE, null = "null")
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
                  if (isTRUE(world$mode$binary_sd)) world$binary_sd)
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
      return(res$send_json(fake_lookup(world, analyses, unique(ids), release_id),
                           auto_unbox = TRUE, null = "null"))
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
    res$send_json(list(release_id = release_id, catalog_version = "fake", offset = start, after = after,
                       limit = limit, total = length(all_ids),
                       analyses = page, next_after = next_after),
                  auto_unbox = TRUE, null = "null")
  }
  app$get("/api/gwasinfo", gwasinfo)
  app$post("/api/gwasinfo", gwasinfo)
  app
}

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

