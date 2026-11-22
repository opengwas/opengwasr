# LD Stores, LD matrices and LD proxies (#12), through the public functions: the fake always, the
# dev Data API too where a test says nothing of the fake's own numbers.

fake <- local_fake_api(n_synthetic = 1)
withr::local_options(opengwasr.poll_cap = 0.05, opengwasr.progress = FALSE)

eur <- "hgdp1kgp-v2-EUR-k10"
cad <- "OGS-00011:GCST003116"
afr_analysis <- "OGS-00011:GCST90475090"
unassigned <- "OGS-00017:finngen-r13-I9_CHD"
snv <- "16:53767042:C:T"
pair <- c("16:53772541:A:G", snv)
vignette_variants <- c("7:105919152:C:T", "7:105920689:A:G", "7:110998200:C:T", "7:117559403:A:G", snv)

# The ld Jobs the fake was sent, oldest first.
ld_bodies <- function(endpoint) {
  all <- raw_api(fake$url(), fake_key, "_fake/bodies")
  lapply(Filter(function(b) identical(b$endpoint, endpoint), all), `[[`, "body")
}

# A data frame of Associations as far as LD reads it: `id` and `alid`.
associations_of <- function(ids, alids) data.frame(id = ids, alid = alids)

test_api("og_ld_stores() lists the LD Stores with the columns the vignette shows", {
  stores <- og_ld_stores(src)
  expect_s3_class(stores, "data.frame")
  expect_equal(names(stores), c("ld_store", "ancestry", "n", "t_far", "near_bp", "far_bp", "n_variants", "default", "ready"))
  expect_true(all(c("EUR", "AFR", "EAS", "SAS", "AMR") %in% stores$ancestry))
  expect_equal(stores$ld_store[stores$ancestry == "EUR"], eur)
  expect_true(all(stores$t_far > 0 & stores$t_far < 1))
  expect_equal(unique(stores$near_bp), 3000000)
  expect_equal(unique(stores$far_bp), 10000000)
  expect_type(stores$n, "integer")
  # The API declares no default yet: NA, not FALSE.
  expect_true(all(is.na(stores$default)))
})

test_that("several LD Stores of one ancestry are refused unless the API declares a default, which og_ld_stores() shows", {
  fake_set(fake, "ld_two_eur", "true", "false")
  src <- fake_source(fake)
  before <- length(ld_bodies("/api/ld/matrix"))
  e <- expect_error(og_ld_matrix(src, pair, ld = "EUR"), class = "opengwasr_ld_unmatched")
  expect_s3_class(e, "opengwasr_refusal")
  expect_length(ld_bodies("/api/ld/matrix"), before)
  expect_true(all(is.na(og_ld_stores(src)$default)))
  # Naming one is always allowed.
  expect_equal(og_provenance(og_ld_matrix(src, pair, ld = paste0(eur, "-b")))$ld, paste0(eur, "-b"))
  # A declared default chooses between them.
  fake_set(fake, "ld_declared_default", "true", "false")
  stores <- og_ld_stores(src)
  expect_equal(stores$default[stores$ld_store == eur], TRUE)
  expect_equal(stores$default[stores$ld_store == paste0(eur, "-b")], FALSE)
  m <- og_ld_matrix(src, pair, ld = "EUR")
  expect_equal(og_provenance(m)$ld, eur)
  expect_equal(og_provenance(m)$ld_choice, "ancestry_default")
})

test_api("the LD Store is chosen by name, by ancestry code, or matched from the Associations' ancestry", {
  by_name <- og_ld_matrix(src, pair, ld = eur)
  expect_equal(og_provenance(by_name)$ld, eur)
  expect_equal(og_provenance(by_name)$ld_choice, "user")
  by_ancestry <- og_ld_matrix(src, pair, ld = "EUR")
  expect_equal(og_provenance(by_ancestry)$ld, eur)
  expect_equal(og_provenance(by_ancestry)$ld_choice, "ancestry_default")
  matched <- og_ld_matrix(src, associations_of(c(cad, cad), pair))
  expect_equal(og_provenance(matched)$ld, eur)
  expect_equal(og_provenance(matched)$ld_choice, "matched")
  expect_equal(matched, by_name, ignore_attr = TRUE)
  # What the Job was sent: the LD Store by name, whichever way it was chosen.
  if (backend == "fake") {
    sent <- ld_bodies("/api/ld/matrix")
    expect_equal(unique(vapply(utils::tail(sent, 3), function(b) b$store, "")), eur)
  }
})

test_api("an unknown `ld` is an argument error, and a plain vector needs `ld`", {
  expect_error(og_ld_matrix(src, pair, ld = "MARS"), class = "opengwasr_argument")
  expect_error(og_ld_matrix(src, pair, ld = 1), class = "opengwasr_argument")
  e <- expect_error(og_ld_matrix(src, pair), class = "opengwasr_ld_unmatched")
  expect_s3_class(e, "opengwasr_refusal")
  expect_true(all(c("EUR", "AFR") %in% e$ld_stores$ancestry))
  expect_error(og_ld_proxies(src, snv), class = "opengwasr_ld_unmatched")
})

test_that("Unassigned and mixed ancestries are refused before any LD request; `ld` overrides", {
  fake_set(fake, "afr_analysis", "true", "false")
  src <- fake_source(fake)
  before <- length(ld_bodies("/api/ld/matrix"))
  mixed <- associations_of(c(cad, afr_analysis), pair)
  e <- expect_error(og_ld_matrix(src, mixed), class = "opengwasr_ld_unmatched")
  expect_match(conditionMessage(e), "differ")
  e <- expect_error(og_ld_matrix(src, associations_of(unassigned, snv)), class = "opengwasr_ld_unmatched")
  expect_match(conditionMessage(e), "Unassigned")
  expect_match(conditionMessage(e), unassigned, fixed = TRUE)
  expect_length(ld_bodies("/api/ld/matrix"), before)
  # One ancestry from an Analysis with a different one is matched to its own LD Store.
  afr <- og_ld_matrix(src, associations_of(afr_analysis, pair))
  expect_equal(og_provenance(afr)$ld, "hgdp1kgp-v2-AFR-k10")
  # `ld` is the user's say-so.
  said <- og_ld_matrix(src, associations_of(unassigned, snv), ld = "AMR")
  expect_equal(og_provenance(said)$ld, "hgdp1kgp-v2-AMR-k10")
})

test_api("og_ld_matrix() is named by ALID, symmetric, with 1 on the diagonal and NA for pairs not held", {
  v <- c("16:53772541:A:G", snv, "18:60161902:C:T")
  m <- og_ld_matrix(src, v, ld = "EUR")
  expect_true(is.matrix(m))
  expect_equal(dimnames(m), list(v, v))
  expect_equal(unname(diag(m)), rep(1, 3))
  expect_equal(m, t(m))
  expect_true(abs(m[v[1], v[2]]) <= 1 && !is.na(m[v[1], v[2]]))
  expect_true(all(is.na(m[3, 1:2])))        # another chromosome: not measured
  prov <- og_provenance(m)
  expect_equal(prov$ld, eur)
  expect_equal(prov$absent, character())
  # The LD Store is `ld`, not a Store Release; no rsID lookup ran, so none was read.
  expect_equal(prov$store_releases, character())
  expect_match(format(prov), "LD Store", all = FALSE)
  # It prints as a matrix: the provenance is read with og_provenance(), not shown after it.
  expect_false(any(grepl("provenance", utils::capture.output(print(round(m, 3))))))
  # A fact both backends know (the vignette's): on one chromosome, 3 Mb to 10 Mb apart, no r held.
  w <- og_ld_matrix(src, c("7:105919152:C:T", "7:105920689:A:G", "7:110998200:C:T"), ld = "EUR")
  if (backend == "live") expect_equal(w[1, 2], -0.1819961, tolerance = 1e-5)
  expect_true(all(is.na(w[3, 1:2])))
})

test_api("variants the LD Store doesn't hold are left out with a classed warning, and listed in provenance", {
  absent <- if (backend == "fake") "1:2999:A:G" else "1:100:A:G"
  w <- expect_warning(og_ld_matrix(src, c(pair, absent), ld = "EUR"), class = "opengwasr_ld_absent_variants")
  expect_equal(w$count, 1)
  expect_equal(w$ids, absent)
  expect_equal(w$ld_store, eur)
  m <- suppressWarnings(og_ld_matrix(src, c(pair, absent), ld = "EUR"))
  expect_equal(rownames(m), pair)
  expect_equal(colnames(m), pair)
  expect_equal(og_provenance(m)$absent, absent)
  # Everything absent: an empty matrix, no Job's worth of rows.
  none <- suppressWarnings(og_ld_matrix(src, absent, ld = "EUR"))
  expect_equal(dim(none), c(0, 0))
  expect_equal(og_provenance(none)$absent, absent)
})

test_that("rsIDs are resolved to every ALID they name; one that resolves to none is left out with the absent", {
  src <- fake_source(fake)
  warned <- NULL
  m <- withCallingHandlers(
    og_ld_matrix(src, c("rs1421085", "rs6567160", "rs1"), ld = "EUR"),
    opengwasr_ld_absent_variants = function(w) { warned <<- w; invokeRestart("muffleWarning") })
  expect_equal(warned$ids, "rs1")
  expect_setequal(rownames(m), c("16:53767042:A:G", "16:53767042:C:T", "18:60161902:C:T"))
  expect_equal(og_provenance(m)$absent, "rs1")
  expect_error(og_ld_matrix(src, "16:53767042:T:C", ld = "EUR"), class = "opengwasr_argument")  # not canonical
})

test_that("more variants than one Job takes are split, and the matrix equals the unsplit one", {
  expect_equal(job_limits()$ld_matrix, 500)
  expect_equal(job_limits()$ld_proxies, 1000)
  src <- fake_source(fake)
  # Variants on two chromosomes, 2.5 Mb apart on the first: near, far and beyond-far pairs.
  v <- c(sprintf("1:%d:A:G", 10000 + 2.5e6 * (0:5)), "2:10000:A:G", "2:20000:A:G")
  # One of them is absent from the LD Store: absent whichever Job finds out.
  whole <- suppressWarnings(og_ld_matrix(src, c(v, "1:1999:A:G"), ld = "EUR"))
  before <- length(ld_bodies("/api/ld/matrix"))
  withr::local_options(opengwasr.job_limits = list(ld_matrix = 4))
  split <- suppressWarnings(og_ld_matrix(src, c(v, "1:1999:A:G"), ld = "EUR"))
  sent <- utils::tail(ld_bodies("/api/ld/matrix"), length(ld_bodies("/api/ld/matrix")) - before)
  expect_equal(split, whole, ignore_attr = "og_provenance")
  expect_gt(length(sent), 1)
  expect_true(all(lengths(lapply(sent, `[[`, "alids")) <= 4))
  expect_equal(og_provenance(split)$job_ids |> length(), length(sent))
  # Every pair is asked about together in some Job.
  together <- function(a, b) any(vapply(sent, function(s) all(c(a, b) %in% unlist(s$alids)), NA))
  expect_true(all(outer(v, v, Vectorize(together))))
  # The values are the fake LD Store's own.
  expect_equal(whole[v[1], v[2]], fake_ld_r(v[1], v[2], fake_ld_stores()[[1]]))
  expect_equal(rownames(split), v)
})

test_api("fill_below_t_far sets to 0 only the pairs within far_bp that the LD Store knows are below its threshold", {
  raw <- og_ld_matrix(src, vignette_variants, ld = "EUR")
  filled <- og_ld_matrix(src, vignette_variants, ld = "EUR", fill_below_t_far = TRUE)
  v <- vignette_variants
  # Within near_bp nothing was missing, so the pair is unchanged.
  expect_equal(filled[v[1], v[2]], raw[v[1], v[2]])
  # 5 Mb and 6.6 Mb apart: below the Far Threshold.
  expect_equal(c(filled[v[3], v[1]], filled[v[3], v[2]], filled[v[3], v[4]], filled[v[4], v[3]]), rep(0, 4))
  # 11.6 Mb apart, and another chromosome: not measured, still NA.
  expect_true(all(is.na(filled[v[4], v[1:2]])))
  expect_true(all(is.na(filled[v[5], v[1:4]])) && all(is.na(filled[v[1:4], v[5]])))
  # Nothing else changed, the matrix stays symmetric, and the default leaves NA alone.
  expect_equal(filled[!is.na(raw)], raw[!is.na(raw)])
  expect_equal(filled, t(filled))
  expect_true(anyNA(raw))
  expect_false(any(raw == 0, na.rm = TRUE))
  expect_error(og_ld_matrix(src, vignette_variants, ld = "EUR", fill_below_t_far = NA), class = "opengwasr_argument")
})

test_api("a refusal from the LD Store surfaces with the API's message and its fields", {
  e <- expect_error(og_ld_proxies(src, snv, ld = "EUR", r2 = 0.001, kb = 10000), class = "opengwasr_ld_refusal")
  expect_s3_class(e, "opengwasr_refusal")
  expect_equal(e$reason, "threshold_below_t_far")
  expect_equal(e$store, eur)
  expect_equal(e$near_bp, 3e6)
  expect_equal(e$far_bp, 1e7)
  expect_true(e$t_far > 0.02 && e$t_far < 0.04)
  expect_match(conditionMessage(e), "r2=0.001 is below t_far", fixed = TRUE)
  # The refusal's own advice works.
  ok <- og_ld_proxies(src, snv, ld = "EUR", r2 = 0.001, kb = e$near_bp / 1000)
  expect_s3_class(ok, "data.frame")
})

test_api("og_ld_proxies() returns an LD lookup: alid, proxy_alid, r, r2, distance, strongest first", {
  px <- og_ld_proxies(src, snv, ld = eur)
  expect_equal(names(px), c("alid", "proxy_alid", "r", "r2", "distance"))
  expect_gt(nrow(px), 0)
  expect_equal(unique(px$alid), snv)
  expect_false(snv %in% px$proxy_alid)
  expect_equal(px$r2, px$r^2, tolerance = 1e-6)
  expect_true(all(px$r2 >= 0.8) && all(px$distance <= 1e7) && all(px$distance >= 0))
  expect_false(is.unsorted(rev(px$r2)))
  prov <- og_provenance(px)
  expect_equal(prov$ld, eur)
  expect_equal(prov$ld_choice, "user")
  # The thresholds were sent as asked: a tighter r2 keeps fewer.
  strict <- og_ld_proxies(src, snv, ld = "EUR", r2 = 0.99)
  expect_lt(nrow(strict), nrow(px))
  expect_true(all(strict$r2 >= 0.99))
  # Absent variants have none, and say so.
  absent <- if (backend == "fake") "1:2999:A:G" else "1:100:A:G"
  w <- expect_warning(none <- og_ld_proxies(src, absent, ld = "EUR"), class = "opengwasr_ld_absent_variants")
  expect_equal(w$ids, absent)
  expect_equal(nrow(none), 0)
  expect_equal(names(none), names(px))
  expect_error(og_ld_proxies(src, snv, ld = "EUR", r2 = "high"), class = "opengwasr_argument")
  expect_error(og_ld_proxies(src, snv, ld = "EUR", kb = c(1, 2)), class = "opengwasr_argument")
})

test_that("og_ld_proxies() rows, by the fake's stated proxies, for several variants and from Associations", {
  src <- fake_source(fake)
  a <- "5:100000:C:T"
  b <- "6:200000:C:T"
  px <- og_ld_proxies(src, c(b, a), ld = "EUR")
  expect_equal(px$alid, c(b, b, a, a))                              # in the order asked
  expect_equal(px$proxy_alid, c("6:199900:A:G", "6:200100:A:G", "5:99900:A:G", "5:100100:A:G"))
  expect_equal(px$r, c(-0.95, 0.95, -0.95, 0.95))
  expect_equal(px$distance, rep(100, 4))
  wide <- og_ld_proxies(src, a, ld = "EUR", r2 = 0.7, kb = 1)
  expect_equal(wide$distance, c(100, 100, 250, 300))                # strongest r2 first
  sent <- utils::tail(ld_bodies("/api/ld/proxies"), 1)[[1]]
  expect_equal(c(sent$r2, sent$kb), c(0.7, 1))
  expect_equal(sent$store, eur)
  # More variants than one Job takes: split, and the same rows.
  five <- sprintf("%d:100000:C:T", 1:5)
  whole <- og_ld_proxies(src, five, ld = "EUR")
  withr::local_options(opengwasr.job_limits = list(ld_proxies = 2))
  split <- og_ld_proxies(src, five, ld = "EUR")
  expect_equal(split, whole, ignore_attr = "og_provenance")
  expect_length(og_provenance(split)$job_ids, 3)
})

test_api("og_submit() works for og_ld_matrix(): the handle collects the same matrix", {
  v <- c(pair, "18:60161902:C:T")
  jobs <- og_submit(src, og_ld_matrix, v, ld = "EUR")
  expect_s3_class(jobs, "og_jobs")
  expect_length(jobs$job_ids, 1)
  collected <- og_job_result(src, jobs)
  direct <- og_ld_matrix(src, v, ld = "EUR")
  expect_equal(collected, direct, ignore_attr = "og_provenance")
  expect_equal(og_provenance(collected)$ld, eur)
  # A matrix that needs two queries to answer (rsIDs are resolved as part of the one) still is one.
  expect_error(og_submit(src, og_ld_matrix, v), class = "opengwasr_ld_unmatched")
})

test_that("the LD queries stay thin: each is a spec over the LD engine", {
  ns <- asNamespace("opengwasr")
  users <- Filter(function(n) is.function(ns[[n]]) && "ld_query" %in% all.names(body(ns[[n]])),
                  ls(ns, all.names = TRUE))
  expect_setequal(users, c("og_ld_matrix.og_api_source", "og_ld_proxies.og_api_source"))
  # Neither runs a Job, or looks anything up, on its own.
  for (m in users) {
    called <- all.names(body(ns[[m]]))
    expect_false(any(c("run_query", "run_window", "query_plan", "attach_provenance", "resolve_variants") %in% called), info = m)
  }
})

test_that("fill_below_t_far leaves a pair on another chromosome NA even when the positions are close", {
  src <- fake_source(fake)
  v <- c("7:105919152:C:T", "7:110998200:C:T", "8:105920000:A:G", "8:110998000:A:G")
  filled <- og_ld_matrix(src, v, ld = "EUR", fill_below_t_far = TRUE)
  expect_equal(filled[v[1], v[2]], 0)            # one chromosome, 5 Mb: below the Far Threshold
  expect_equal(filled[v[3], v[4]], 0)
  expect_true(all(is.na(filled[v[1:2], v[3:4]])))  # another chromosome, a few kb apart: not measured
})

test_that("provenance of an LD call: no total from lookups, no whole data frame in args, no LD Store under Store Releases", {
  src <- fake_source(fake)
  frame <- data.frame(id = cad, alid = pair, store_id = "OGS-00011", release_id = "OGS-00011-completed")
  m <- og_ld_matrix(src, frame)
  prov <- og_provenance(m)
  expect_false(is.data.frame(prov$args$variants))
  expect_equal(prov$args$variants$ids, cad)
  expect_equal(prov$args$variants$alids, pair)
  expect_equal(prov$store_releases, character())
  expect_false(any(grepl("Store Releases: .*hgdp", format(prov))))
  # rsIDs: the lookup read GWAS Store Releases and counts no rows.
  r <- og_provenance(suppressWarnings(og_ld_matrix(src, "rs1421085", ld = "EUR")))
  expect_true("OGS-00011/OGS-00011-completed" %in% r$store_releases)
  expect_false(any(grepl("hgdp", r$store_releases)))
})

test_that("an LD Store that isn't ready is listed, and choosing it says so", {
  fake_set(fake, "ld_not_ready", "true", "false")
  src <- fake_source(fake)
  stores <- og_ld_stores(src)
  expect_equal(stores$ready, stores$ancestry != "AMR")
  before <- length(ld_bodies("/api/ld/matrix"))
  for (ld in c("AMR", "hgdp1kgp-v2-AMR-k10")) {
    e <- expect_error(og_ld_matrix(src, pair, ld = ld), class = "opengwasr_ld_not_ready")
    expect_match(conditionMessage(e), "isn't ready")
    expect_s3_class(e, "opengwasr_refusal")
  }
  expect_length(ld_bodies("/api/ld/matrix"), before)
  expect_equal(og_provenance(og_ld_matrix(src, pair, ld = "EUR"))$ld, eur)
})
