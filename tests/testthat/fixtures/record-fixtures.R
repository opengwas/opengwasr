# Regenerates fixtures/world.json and fixtures/shapes.json from the dev Data API.
# Run by hand, with credentials in the environment:
#   R_ENVIRON_USER=/path/to/.Renviron Rscript tests/testthat/fixtures/record-fixtures.R
# The fixtures hold real Analysis rows (public metadata) and nothing about the caller's key.
source("tests/testthat/raw-api.R")
live <- live_credentials()
get <- function(path, body = NULL, parse = TRUE) raw_api(live$url, live$key, path, body, parse = parse)
keep <- c("OGS-00011:GCST90029007", "OGS-00011:GCST003116", "OGS-00009:ukb-b-19953",
          "OGS-00017:finngen-r13-I9_CHD", "OGS-00027:ENSG00000000419")
info <- get("api/gwasinfo", list(id = as.list(keep)))
status <- get("api/status")
batches <- get("api/batches")
world <- list(
  releases = lapply(batches$batches, `[`, c("store_id", "release_id")),
  analyses = info$analyses,
  legacy = list(
    `finn-b-I9_CHD` = list(catalog_ids = list("OGS-00017:finngen-r13-I9_CHD"), match = "successor"),
    `ebi-a-GCST003116` = list(catalog_ids = list("OGS-00011:GCST003116"), match = "exact"),
    `ieu-a-2` = list(catalog_ids = list(), reason = "OpenGWAS batch ieu-a is not held by any Store")
  ),
  user = get("api/user"),
  status = list(status = status$status, ready = status$ready,
                execution = list(limits = status$execution$limits))
)
jsonlite::write_json(world, "tests/testthat/fixtures/world.json", auto_unbox = TRUE, pretty = TRUE, null = "null")
# Key sets of the live responses: the contract the fake must keep (the OpenAPI response schemas are open objects).
shapes <- list(
  analysis_row = names(info$analyses[[1]]),
  gwasinfo_lookup = setdiff(names(get("api/gwasinfo", list(id = list("finn-b-I9_CHD", "ieu-a-2")))), c("opengwas", "retired")),
  gwasinfo_page = names(get("api/gwasinfo", list(limit = 1))),
  user = names(world$user),
  status = c("status", "ready", "execution"),
  status_limits = names(status$execution$limits),
  opengwas_entry = c("id", "batch", "catalog_ids"),
  retired_entry = c("id", "catalog_id", "superseded_by"),
  batches = "batches",
  batch_row = c("release_id", "store_id", "analysis_count")
)
jsonlite::write_json(shapes, "tests/testthat/fixtures/shapes.json", auto_unbox = TRUE, pretty = TRUE)
# The OpenAPI snapshot, verbatim.
writeLines(get("openapi.json", parse = FALSE), "tests/testthat/fixtures/openapi.json")
# 1,001 real ids (one more than a lookup holds) from one GWAS Store, for the live batching test.
page <- get("api/gwasinfo", list(limit = 1000, releases = list(list(store_id = "OGS-00006", release_id = "OGS-00006-completed"))))
more <- get("api/gwasinfo", list(limit = 2, after = page$analyses[[1000]]$id, releases = list(list(store_id = "OGS-00006", release_id = "OGS-00006-completed"))))
ids <- c(vapply(page$analyses, function(a) a$id, ""), more$analyses[[1]]$id)
writeLines(ids, "tests/testthat/fixtures/live-ids.txt")
