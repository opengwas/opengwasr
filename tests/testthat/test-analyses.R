fake <- local_fake_api()

# Hand-written expectations: the real rows behind the fixtures (dev Data API, 22 Nov 2026).
bmi <- "OGS-00011:GCST90029007"
cad <- "OGS-00011:GCST003116"

test_api("og_analyses() looks up Analyses by canonical id, in the order given", {
  info <- og_analyses(src, c(cad, bmi))
  expect_s3_class(info, "data.frame")
  expect_equal(info$id, c(cad, bmi))
  expect_equal(info$requested_id, c(cad, bmi))
  expect_equal(info$store_id, c("OGS-00011", "OGS-00011"))
  expect_equal(info$analysis_id, c("GCST003116", "GCST90029007"))
  expect_equal(info$analysis_label, c("Coronary artery disease", "Body mass index"))
  expect_equal(info$sample_size, c(187599, 532396))
  expect_equal(info$outcome_type, c("binary", "continuous"))
  expect_equal(info$stored_effect_scale, c("log_or", "sd"))
  expect_equal(info$release_id, rep("OGS-00011-completed", 2))
  expect_true(all(c("assigned_ancestry", "n_cases", "n_controls", "original_sd", "license") %in% names(info)))
  expect_equal(names(info)[1:2], c("requested_id", "id"))
})

test_api("og_analyses() answers one row per identifier given, repeats included", {
  info <- og_analyses(src, c(cad, bmi, cad))
  expect_equal(info$id, c(cad, bmi, cad))
})

test_api("og_analyses() with no identifiers returns an empty data frame with the same leading columns", {
  info <- og_analyses(src, character())
  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), 0)
  expect_equal(names(info)[1:2], c("requested_id", "id"))
})

test_that("og_analyses() refuses ids that aren't a character vector", {
  src <- fake_source(fake)
  expect_error(og_analyses(src, 1:3), class = "opengwasr_argument")
  expect_error(og_analyses(src, c("a", NA)), class = "opengwasr_argument")
})

test_api("a legacy id that maps uniquely is answered, with requested_id showing what was asked", {
  expect_no_warning(info <- og_analyses(src, c("ukb-b-19953", "ebi-a-GCST003116")))
  expect_equal(info$requested_id, c("ukb-b-19953", "ebi-a-GCST003116"))
  expect_equal(info$id, c("OGS-00009:ukb-b-19953", cad))
  expect_equal(info$analysis_label, c("Body mass index (BMI)", "Coronary artery disease"))
})

test_api("a legacy id with a successor is answered by the successor, with a classed warning", {
  warned <- NULL
  info <- withCallingHandlers(
    og_analyses(src, "finn-b-I9_CHD"),
    opengwasr_legacy_successor = function(w) {
      warned <<- w
      rlang::cnd_muffle(w)
    }
  )
  expect_equal(info$requested_id, "finn-b-I9_CHD")
  expect_equal(info$id, "OGS-00017:finngen-r13-I9_CHD")
  expect_s3_class(warned, "opengwasr_legacy_successor")
  expect_false(inherits(warned, "opengwasr_error"))
  expect_equal(warned$requested_id, "finn-b-I9_CHD")
  expect_equal(warned$id, "OGS-00017:finngen-r13-I9_CHD")
})

test_api("asking for the successor by name raises no warning", {
  expect_no_warning(og_analyses(src, "OGS-00017:finngen-r13-I9_CHD"))
})

test_api("one warning per legacy id with a successor", {
  warnings <- list()
  withCallingHandlers(
    og_analyses(src, c("finn-b-I9_CHD", bmi)),
    warning = function(w) {
      warnings[[length(warnings) + 1]] <<- w
      rlang::cnd_muffle(w)
    }
  )
  expect_length(warnings, 1)
})

test_api("an id held by more than one GWAS Store raises opengwasr_ambiguous_analysis, listing the candidates", {
  err <- expect_error(og_analyses(src, c(bmi, "GCST90000001")), class = "opengwasr_ambiguous_analysis")
  expect_s3_class(err, "opengwasr_error")
  expect_equal(err$ids, "GCST90000001")
  expect_equal(sort(err$candidates[["GCST90000001"]]), c("OGS-00011:GCST90000001", "OGS-00012:GCST90000001"))
}, live = FALSE)

test_api("naming the GWAS Store resolves an ambiguous id", {
  info <- og_analyses(src, "OGS-00012:GCST90000001")
  expect_equal(info$id, "OGS-00012:GCST90000001")
}, live = FALSE)

test_api("an unknown id raises opengwasr_unknown_analysis carrying the server's reason", {
  err <- expect_error(og_analyses(src, c(cad, "ieu-a-2", "nope")), class = "opengwasr_unknown_analysis")
  expect_s3_class(err, "opengwasr_error")
  expect_equal(err$ids, c("ieu-a-2", "nope"))
  # The server's own words are kept apart from opengwasr's: `api_reasons` is NA where the API gave none.
  expect_equal(err$api_reasons[["ieu-a-2"]], "OpenGWAS batch ieu-a is not held by any Store")
  expect_true(is.na(err$api_reasons[["nope"]]))
  expect_named(err$reasons, c("ieu-a-2", "nope"))
})

test_api("a retired Analysis raises a classed error naming its successor", {
  err <- expect_error(og_analyses(src, c("OGS-00013:OLD1", "OGS-00013:GONE")),
                      class = "opengwasr_retired_analysis")
  expect_s3_class(err, "opengwasr_unknown_analysis")
  expect_s3_class(err, "opengwasr_error")
  expect_equal(err$ids, c("OGS-00013:OLD1", "OGS-00013:GONE"))
  expect_equal(err$superseded_by[["OGS-00013:OLD1"]], "OGS-00014:NEW1")
  expect_true(is.na(err$superseded_by[["OGS-00013:GONE"]]))
}, live = FALSE)

test_api("more than 1,000 ids come back complete and in order", {
  ids <- if (backend == "fake") {
    sprintf("OGS-00006:SYN%05d", 1:1001)
  } else {
    readLines(fixture_file("live-ids.txt"))
  }
  info <- og_analyses(src, ids)
  expect_equal(nrow(info), length(ids))
  expect_equal(info$id, ids)
  expect_equal(info$requested_id, ids)
  expect_false(anyNA(info$analysis_label))
})

test_api("a Store Release can be pinned, in either form", {
  by_store <- og_analyses(src, cad, releases = "OGS-00011")
  by_release <- og_analyses(src, cad, releases = "OGS-00011/OGS-00011-completed")
  expect_equal(by_store$release_id, "OGS-00011-completed")
  expect_equal(by_release$release_id, "OGS-00011-completed")
  expect_equal(by_store$id, cad)
})

test_api("without `releases`, each GWAS Store answers from its sole served Store Release", {
  info <- og_analyses(src, c(cad, "OGS-00009:ukb-b-19953"))
  expect_equal(info$release_id, c("OGS-00011-completed", "OGS-00009-completed"))
})

test_api("several Releases can be pinned at once", {
  info <- og_analyses(src, c("OGS-00009:ukb-b-19953", cad), releases = c("OGS-00009", "OGS-00011"))
  expect_equal(info$id, c("OGS-00009:ukb-b-19953", cad))
})

test_api("a pinned Release leaves out Analyses of other Stores, as unknown", {
  err <- expect_error(og_analyses(src, c(cad, "OGS-00009:ukb-b-19953"), releases = "OGS-00011"),
                      class = "opengwasr_unknown_analysis")
  expect_equal(err$ids, "OGS-00009:ukb-b-19953")
})

test_api("pinning a Release resolves an id that is ambiguous without it", {
  info <- og_analyses(src, "GCST90000001", releases = "OGS-00012")
  expect_equal(info$id, "OGS-00012:GCST90000001")
}, live = FALSE)

test_api("a Release the API doesn't serve raises opengwasr_unknown_release", {
  expect_error(og_analyses(src, cad, releases = "OGS-00011/nonesuch"), class = "opengwasr_unknown_release")
  expect_error(og_analyses(src, cad, releases = "OGS-99999"), class = "opengwasr_unknown_release")
})

test_that("a malformed `releases` is refused before any request", {
  src <- fake_source(fake)
  expect_error(og_analyses(src, cad, releases = ""), class = "opengwasr_argument")
  expect_error(og_analyses(src, cad, releases = 7), class = "opengwasr_argument")
})

test_api("og_analyses() with no ids reads the whole catalogue across pages", {
  catalogue <- og_analyses(src)
  expect_s3_class(catalogue, "data.frame")
  expect_equal(catalogue$requested_id, catalogue$id)
  expect_false(anyDuplicated(catalogue$id) > 0)
  expect_true(all(c(bmi, cad, "OGS-00009:ukb-b-19953") %in% catalogue$id))
  if (backend == "fake") {
    # 5 recorded rows, 2 ambiguous-id rows, 1,001 synthetic rows: two pages of at most 1,000.
    expect_equal(nrow(catalogue), 1008)
  } else {
    expect_gt(nrow(catalogue), 30000)
  }
})

test_api("the catalogue can be limited to pinned Releases", {
  catalogue <- og_analyses(src, releases = "OGS-00011")
  expect_true(all(catalogue$store_id == "OGS-00011"))
  expect_true(cad %in% catalogue$id)
  if (backend == "fake") expect_equal(nrow(catalogue), 3)
})

test_api("a GWAS Store with several Store Releases can't be used unpinned, and is never guessed", {
  fake_set(fake, "two_releases", "true", "false")
  err <- expect_error(og_analyses(src, cad, releases = "OGS-00099"),
                      class = "opengwasr_release_selection_required")
  expect_s3_class(err, "opengwasr_error")
  expect_equal(err$store_id, "OGS-00099")
  expect_setequal(err$releases, c("OGS-00099/OGS-00099-a", "OGS-00099/OGS-00099-b"))
  # Pinning one is honoured (the fake holds no Analyses there, so the id is unknown, not an error of selection).
  expect_error(og_analyses(src, cad, releases = "OGS-00099/OGS-00099-a"), class = "opengwasr_unknown_analysis")
}, live = FALSE)

test_api("a catalogue page with `total` but no cursor is followed by offset", {
  fake_set(fake, "paging", "offset", "cursor")
  catalogue <- og_analyses(src)
  expect_equal(nrow(catalogue), 1008)
  expect_false(anyDuplicated(catalogue$id) > 0)
}, live = FALSE)

test_api("a catalogue that can't be completed raises a classed error, never a shorter catalogue", {
  fake_set(fake, "paging", "truncating", "cursor")
  err <- expect_error(og_analyses(src), class = "opengwasr_api_error")
  expect_s3_class(err, "opengwasr_error")
  expect_equal(c(err$collected, err$total), c(1000, 1008))
}, live = FALSE)

test_api("omitting `releases` doesn't mean every Store Release: a GWAS Store with several is refused", {
  fake_set(fake, "two_releases", "true", "false")
  err <- expect_error(og_analyses(src, cad), class = "opengwasr_release_selection_required")
  expect_equal(err$store_id, "OGS-00099")
  expect_setequal(err$releases, c("OGS-00099/OGS-00099-a", "OGS-00099/OGS-00099-b"))
  expect_error(og_analyses(src), class = "opengwasr_release_selection_required")
  # Pinning the other GWAS Stores explicitly still works.
  expect_equal(og_analyses(src, cad, releases = "OGS-00011")$id, cad)
}, live = FALSE)

test_api("a retired Analysis names every successor the API gives, not the first", {
  err <- expect_error(og_analyses(src, "OGS-00013:SPLIT"), class = "opengwasr_retired_analysis")
  expect_equal(err$superseded_by[["OGS-00013:SPLIT"]], c("OGS-00014:A", "OGS-00014:B"))
}, live = FALSE)

test_api("a success without an `analyses` field is an API error, not an empty result", {
  fake_set(fake, "no_analyses", "true", "false")
  expect_error(og_analyses(src), class = "opengwasr_api_error")
  expect_error(og_analyses(src, cad), class = "opengwasr_api_error")
}, live = FALSE)

test_api("an explicit empty `analyses` is an empty catalogue", {
  fake_set(fake, "two_releases", "true", "false")
  catalogue <- og_analyses(src, releases = "OGS-00099/OGS-00099-a")
  expect_equal(nrow(catalogue), 0)
  expect_equal(names(catalogue)[1:2], c("requested_id", "id"))
}, live = FALSE)
