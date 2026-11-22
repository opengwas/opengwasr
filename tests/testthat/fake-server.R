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
  slim <- template[c("id", "store_id", "analysis_id", "analysis_label", "release_id")]
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
  world$analyses <- c(world$analyses, ambiguous, synthetic)
  world$retired <- list(list(id = "OGS-00013:OLD1", catalog_id = "OGS-00013:OLD1",
                             superseded_by = "OGS-00014:NEW1"),
                        list(id = "OGS-00013:SPLIT", catalog_id = "OGS-00013:SPLIT",
                             superseded_by = list("OGS-00014:A", "OGS-00014:B")),
                        list(id = "OGS-00013:GONE", catalog_id = "OGS-00013:GONE", superseded_by = NULL))
  # Scenarios, switched by POST /_fake/<setting>/<value>: `two_releases` makes GWAS Store OGS-00099
  # serve two Store Releases (listed newest-first; the API gives no order); `leak` makes errors and
  # lookup reasons echo the caller's key, as a careless server might; `plain401` answers 401 in plain
  # text; `no_analyses` answers gwasinfo with `{}`.
  world$extra_releases <- list(list(store_id = "OGS-00099", release_id = "OGS-00099-b"),
                               list(store_id = "OGS-00099", release_id = "OGS-00099-a"))
  world$mode <- list(paging = "cursor", malformed = FALSE, two_releases = FALSE, leak = FALSE,
                     plain401 = FALSE, no_analyses = FALSE)
  world$key <- fake_key
  world
}

served_releases <- function(world) {
  c(world$releases, if (isTRUE(world$mode$two_releases)) world$extra_releases)
}

fake_api <- function(world = fake_world()) {
  app <- webfakes::new_app()
  app$use(webfakes::mw_json(simplifyVector = FALSE))
  app$locals$world <- world
  app$locals$openapi <- read_fixture("openapi.json")

  fail <- function(res, status, code, message, ...) {
    res$set_status(status)$send_json(c(list(error = code, message = message), list(...)),
                                     auto_unbox = TRUE, null = "null")
  }
  authorised <- function(req, res) {
    token <- sub("^Bearer ", "", req$get_header("Authorization") %||% "")
    mode <- req$app$locals$world$mode
    if (isTRUE(mode$plain401) && !identical(token, req$app$locals$world$key)) {
      res$set_status(401)$set_type("text/plain")$send("Unauthorized")
      return(FALSE)
    }
    if (!nzchar(token)) {
      fail(res, 401, "unauthorized", "Bearer token required")
      return(FALSE)
    }
    if (!identical(token, req$app$locals$world$key)) {
      if (isTRUE(mode$leak)) {
        # The token as a JSON member name, and in the message.
        do.call(fail, c(list(res, 401, "unauthorized", paste("Invalid bearer token", token)),
                        stats::setNames(list("x"), token)))
        return(FALSE)
      }
      # Echoes the token, as a careless server might: the client must not keep it.
      fail(res, 401, "unauthorized", paste("Invalid bearer token", token))
      return(FALSE)
    }
    TRUE
  }
  needs_user <- function(req, res) {
    if (is.null(req$get_header("X-OGDB-User"))) {
      fail(res, 400, "user_required", "Gateway key on a data route without X-OGDB-User")
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
      if (length(problems) > 0) return(fail(res, 400, "invalid_request", "Malformed request"))
    }
    if (isTRUE(world$mode$malformed)) return(res$set_type("application/json")$send("{not json"))
    if (isTRUE(world$mode$no_analyses)) return(res$set_type("application/json")$send("{}"))
    if (isTRUE(world$mode$leak)) {
      token <- sub("^Bearer ", "", req$get_header("Authorization") %||% "")
      world$legacy[["ieu-a-2"]]$reason <- paste("OpenGWAS batch ieu-a is not held by any Store; asked by", token)
    }
    analyses <- world$analyses
    ids <- unlist(body$id %||% body$ids)
    if (!is.null(body$releases)) {
      wanted <- vapply(body$releases, function(r) paste0(r$store_id, "/", r$release_id), "")
      served <- vapply(served_releases(world), function(r) paste0(r$store_id, "/", r$release_id), "")
      unknown <- setdiff(wanted, served)
      if (length(unknown) > 0) {
        return(fail(res, 404, "release_not_found", paste("unknown release:", unknown[[1]])))
      }
      analyses <- Filter(function(a) paste0(a$store_id, "/", a$release_id) %in% wanted, analyses)
    }
    release_id <- paste(vapply(served_releases(world), function(r) paste0(r$store_id, "/", r$release_id), ""),
                        collapse = ",")

    if (!is.null(ids)) {
      if (length(ids) > 1000) return(fail(res, 400, "invalid_request", "Malformed request"))
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

