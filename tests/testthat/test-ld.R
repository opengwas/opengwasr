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
  e <- expect_error(og_ld_matrix(with_ld(src, "EUR"), pair), class = "opengwasr_ld_unmatched")
  expect_s3_class(e, "opengwasr_refusal")
  expect_length(ld_bodies("/api/ld/matrix"), before)
  expect_true(all(is.na(og_ld_stores(src)$default)))
  # Naming one is always allowed.
  expect_equal(og_provenance(og_ld_matrix(with_ld(src, paste0(eur, "-b")), pair))$ld, paste0(eur, "-b"))
  # A declared default chooses between them.
  fake_set(fake, "ld_declared_default", "true", "false")
  stores <- og_ld_stores(src)
  expect_equal(stores$default[stores$ld_store == eur], TRUE)
  expect_equal(stores$default[stores$ld_store == paste0(eur, "-b")], FALSE)
  m <- og_ld_matrix(with_ld(src, "EUR"), pair)
  expect_equal(og_provenance(m)$ld, eur)
  expect_equal(og_provenance(m)$ld_choice, "ancestry_default")
  # Matched by Assigned Ancestry (no LD resource), the declared default breaks the tie, and says so.
  matched <- og_ld_matrix(src, associations_of(c(cad, cad), pair))
  expect_equal(og_provenance(matched)$ld, eur)
  expect_equal(og_provenance(matched)$ld_choice, "ancestry_default")
  withr::local_options(opengwasr.progress = TRUE)
  said <- character()
  top <- withCallingHandlers(suppressWarnings(og_tophits(src, cad, releases = "OGS-00011/OGS-00011-completed")),
                             message = function(m) {
                               if (inherits(m, "opengwasr_clumping")) said <<- c(said, conditionMessage(m))
                               invokeRestart("muffleMessage")
                             })
  expect_equal(unique(top$clump_ld), eur)
  expect_equal(unique(top$clump_ld_choice), "ancestry_default")
  expect_match(said, "the declared default LD Store of its Assigned Ancestry (EUR)", fixed = TRUE)
})

test_api("the LD Store is chosen by name, by ancestry code, or matched from the Associations' ancestry", {
  by_name <- og_ld_matrix(with_ld(src, eur), pair)
  expect_equal(og_provenance(by_name)$ld, eur)
  expect_equal(og_provenance(by_name)$ld_choice, "user")
  by_ancestry <- og_ld_matrix(with_ld(src, "EUR"), pair)
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

test_api("an unknown `ld_store` is an argument error, and a plain vector needs an LD resource", {
  expect_error(og_ld_matrix(with_ld(src, "MARS"), pair), class = "opengwasr_argument")
  expect_error(with_ld(src, 1), class = "opengwasr_argument")
  e <- expect_error(og_ld_matrix(src, pair), class = "opengwasr_ld_unmatched")
  expect_s3_class(e, "opengwasr_refusal")
  expect_true(all(c("EUR", "AFR") %in% e$ld_stores$ancestry))
  expect_error(og_ld_proxies(src, snv), class = "opengwasr_ld_unmatched")
})

test_that("Unassigned and mixed ancestries are refused before any LD request; the Source's LD Store answers them", {
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
  # The Source's LD Store is the user's say-so.
  said <- og_ld_matrix(with_ld(src, "AMR"), associations_of(unassigned, snv))
  expect_equal(og_provenance(said)$ld, "hgdp1kgp-v2-AMR-k10")
})

test_api("og_ld_matrix() is named by ALID, symmetric, with 1 on the diagonal and NA for pairs not held", {
  v <- c("16:53772541:A:G", snv, "18:60161902:C:T")
  m <- og_ld_matrix(with_ld(src, "EUR"), v)
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
  w <- og_ld_matrix(with_ld(src, "EUR"), c("7:105919152:C:T", "7:105920689:A:G", "7:110998200:C:T"))
  if (backend == "live") expect_equal(w[1, 2], -0.1819961, tolerance = 1e-5)
  expect_true(all(is.na(w[3, 1:2])))
})

test_api("variants the LD Store doesn't hold are left out with a classed warning, and listed in provenance", {
  absent <- if (backend == "fake") "1:2999:A:G" else "1:100:A:G"
  w <- expect_warning(og_ld_matrix(with_ld(src, "EUR"), c(pair, absent)), class = "opengwasr_ld_absent_variants")
  expect_equal(w$count, 1)
  expect_equal(w$ids, absent)
  expect_equal(w$ld_store, eur)
  m <- suppressWarnings(og_ld_matrix(with_ld(src, "EUR"), c(pair, absent)))
  expect_equal(rownames(m), pair)
  expect_equal(colnames(m), pair)
  expect_equal(og_provenance(m)$absent, absent)
  # Everything absent: an empty matrix, no Job's worth of rows.
  none <- suppressWarnings(og_ld_matrix(with_ld(src, "EUR"), absent))
  expect_equal(dim(none), c(0, 0))
  expect_equal(og_provenance(none)$absent, absent)
})

test_that("rsIDs are resolved to every ALID they name; one that resolves to none is left out with the absent", {
  src <- fake_source(fake)
  warned <- NULL
  m <- withCallingHandlers(
    og_ld_matrix(with_ld(src, "EUR"), c("rs1421085", "rs6567160", "rs1")),
    opengwasr_ld_absent_variants = function(w) { warned <<- w; invokeRestart("muffleWarning") })
  expect_equal(warned$ids, "rs1")
  expect_setequal(rownames(m), c("16:53767042:A:G", "16:53767042:C:T", "18:60161902:C:T"))
  expect_equal(og_provenance(m)$absent, "rs1")
  expect_error(og_ld_matrix(with_ld(src, "EUR"), "16:53767042:T:C"), class = "opengwasr_argument")  # not canonical
})

test_that("more variants than one Job takes are split, and the matrix equals the unsplit one", {
  expect_equal(job_limits()$ld_matrix, 500)
  expect_equal(job_limits()$ld_proxies, 1000)
  src <- fake_source(fake)
  # Variants on two chromosomes, 2.5 Mb apart on the first: near, far and beyond-far pairs.
  v <- c(sprintf("1:%d:A:G", 10000 + 2.5e6 * (0:5)), "2:10000:A:G", "2:20000:A:G")
  # One of them is absent from the LD Store: absent whichever Job finds out.
  whole <- suppressWarnings(og_ld_matrix(with_ld(src, "EUR"), c(v, "1:1999:A:G")))
  before <- length(ld_bodies("/api/ld/matrix"))
  withr::local_options(opengwasr.job_limits = list(ld_matrix = 4))
  split <- suppressWarnings(og_ld_matrix(with_ld(src, "EUR"), c(v, "1:1999:A:G")))
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
  raw <- og_ld_matrix(with_ld(src, "EUR"), vignette_variants)
  filled <- og_ld_matrix(with_ld(src, "EUR"), vignette_variants, fill_below_t_far = TRUE)
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
  expect_error(og_ld_matrix(with_ld(src, "EUR"), vignette_variants, fill_below_t_far = NA), class = "opengwasr_argument")
})

test_api("a refusal from the LD Store surfaces with the API's message and its fields", {
  e <- expect_error(og_ld_proxies(with_ld(src, "EUR"), snv, r2 = 0.001, kb = 10000), class = "opengwasr_ld_refusal")
  expect_s3_class(e, "opengwasr_refusal")
  expect_equal(e$reason, "threshold_below_t_far")
  expect_equal(e$store, eur)
  expect_equal(e$near_bp, 3e6)
  expect_equal(e$far_bp, 1e7)
  expect_true(e$t_far > 0.02 && e$t_far < 0.04)
  expect_match(conditionMessage(e), "r2=0.001 is below t_far", fixed = TRUE)
  # The refusal's own advice works.
  ok <- og_ld_proxies(with_ld(src, "EUR"), snv, r2 = 0.001, kb = e$near_bp / 1000)
  expect_s3_class(ok, "data.frame")
})

test_api("og_ld_proxies() returns an LD lookup: alid, proxy_alid, r, r2, distance, strongest first", {
  px <- og_ld_proxies(with_ld(src, eur), snv)
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
  strict <- og_ld_proxies(with_ld(src, "EUR"), snv, r2 = 0.99)
  expect_lt(nrow(strict), nrow(px))
  expect_true(all(strict$r2 >= 0.99))
  # Absent variants have none, and say so.
  absent <- if (backend == "fake") "1:2999:A:G" else "1:100:A:G"
  w <- expect_warning(none <- og_ld_proxies(with_ld(src, "EUR"), absent), class = "opengwasr_ld_absent_variants")
  expect_equal(w$ids, absent)
  expect_equal(nrow(none), 0)
  expect_equal(names(none), names(px))
  expect_error(og_ld_proxies(with_ld(src, "EUR"), snv, r2 = "high"), class = "opengwasr_argument")
  expect_error(og_ld_proxies(with_ld(src, "EUR"), snv, kb = c(1, 2)), class = "opengwasr_argument")
})

test_that("og_ld_proxies() rows, by the fake's stated proxies, for several variants and from Associations", {
  src <- fake_source(fake)
  a <- "5:100000:C:T"
  b <- "6:200000:C:T"
  px <- og_ld_proxies(with_ld(src, "EUR"), c(b, a))
  expect_equal(px$alid, c(b, b, a, a))                              # in the order asked
  expect_equal(px$proxy_alid, c("6:199900:A:G", "6:200100:A:G", "5:99900:A:G", "5:100100:A:G"))
  expect_equal(px$r, c(-0.95, 0.95, -0.95, 0.95))
  expect_equal(px$distance, rep(100, 4))
  wide <- og_ld_proxies(with_ld(src, "EUR"), a, r2 = 0.7, kb = 1)
  expect_equal(wide$distance, c(100, 100, 250, 300))                # strongest r2 first
  sent <- utils::tail(ld_bodies("/api/ld/proxies"), 1)[[1]]
  expect_equal(c(sent$r2, sent$kb), c(0.7, 1))
  expect_equal(sent$store, eur)
  # More variants than one Job takes: split, and the same rows.
  five <- sprintf("%d:100000:C:T", 1:5)
  whole <- og_ld_proxies(with_ld(src, "EUR"), five)
  withr::local_options(opengwasr.job_limits = list(ld_proxies = 2))
  split <- og_ld_proxies(with_ld(src, "EUR"), five)
  expect_equal(split, whole, ignore_attr = "og_provenance")
  expect_length(og_provenance(split)$job_ids, 3)
})

test_api("og_submit() works for og_ld_matrix(): the handle collects the same matrix", {
  v <- c(pair, "18:60161902:C:T")
  jobs <- og_submit(with_ld(src, "EUR"), og_ld_matrix, v)
  expect_s3_class(jobs, "og_jobs")
  expect_length(jobs$job_ids, 1)
  collected <- og_job_result(src, jobs)
  direct <- og_ld_matrix(with_ld(src, "EUR"), v)
  expect_equal(collected, direct, ignore_attr = "og_provenance")
  expect_equal(og_provenance(collected)$ld, eur)
  # A matrix that needs two queries to answer (rsIDs are resolved as part of the one) still is one.
  expect_error(og_submit(src, og_ld_matrix, v), class = "opengwasr_ld_unmatched")
})

test_that("the LD queries stay thin: each is a spec over the LD engine", {
  ns <- asNamespace("opengwasr")
  users <- Filter(function(n) is.function(ns[[n]]) && "ld_query" %in% all.names(body(ns[[n]])),
                  ls(ns, all.names = TRUE))
  # A Local Source's og_ld_matrix() and og_clump() are the API Source's methods (#34).
  expect_setequal(users, c("og_ld_matrix.og_api_source", "og_ld_proxies.og_api_source", "og_clump.og_api_source",
                         "og_ld_reflookup.og_api_source", "og_ld_matrix.og_local_source", "og_clump.og_local_source"))
  expect_identical(ns$og_ld_matrix.og_local_source, ns$og_ld_matrix.og_api_source)
  expect_identical(ns$og_clump.og_local_source, ns$og_clump.og_api_source)
  # Neither runs a Job, or looks anything up, on its own.
  for (m in users) {
    called <- all.names(body(ns[[m]]))
    expect_false(any(c("run_query", "run_window", "query_plan", "attach_provenance", "resolve_variants") %in% called), info = m)
  }
})

test_that("fill_below_t_far leaves a pair on another chromosome NA even when the positions are close", {
  src <- fake_source(fake)
  v <- c("7:105919152:C:T", "7:110998200:C:T", "8:105920000:A:G", "8:110998000:A:G")
  filled <- og_ld_matrix(with_ld(src, "EUR"), v, fill_below_t_far = TRUE)
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
  r <- og_provenance(suppressWarnings(og_ld_matrix(with_ld(src, "EUR"), "rs1421085")))
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
    e <- expect_error(og_ld_matrix(with_ld(src, ld), pair), class = "opengwasr_ld_not_ready")
    expect_match(conditionMessage(e), "isn't ready")
    expect_s3_class(e, "opengwasr_refusal")
  }
  expect_length(ld_bodies("/api/ld/matrix"), before)
  expect_equal(og_provenance(og_ld_matrix(with_ld(src, "EUR"), pair))$ld, eur)
})

# --- Clumping (#13) -----------------------------------------------------------------------------

rel <- "OGS-00011/OGS-00011-completed"
clump_columns <- c("clump_ld", "clump_ld_choice", "clump_r2", "clump_kb", "clump_size")
t_far <- function(src, ancestry) { s <- og_ld_stores(src); s$t_far[s$ancestry == ancestry] }

test_that("og_tophits() clumps each Analysis in the LD Store of its Assigned Ancestry, one Job each, grouped by LD Store, at the LD Store's own thresholds", {
  fake_set(fake, "afr_analysis", "true", "false")
  src <- fake_source(fake)
  bmi <- "OGS-00011:GCST90029007"
  before <- length(ld_bodies("/api/tophits"))
  # The "Clumping ..." messages come with progress on (one per LD Store); scripts are quiet.
  withr::local_options(opengwasr.progress = TRUE)
  messages <- character()
  w <- expect_warning(
    withCallingHandlers(top <- og_tophits(src, c(cad, afr_analysis, bmi), releases = rel),
                        message = function(m) {
                          if (inherits(m, "opengwasr_clumping")) messages <- c(messages, conditionMessage(m))
                          messages <<- messages
                          invokeRestart("muffleMessage")
                        }),
    class = "opengwasr_clump_absent")
  expect_length(messages, 2)
  expect_match(messages[[2]], "hgdp1kgp-v2-AFR-k10, matched to its Assigned Ancestry (AFR)", fixed = TRUE)
  # One Job per Analysis, submitted grouped by LD Store (EUR, EUR, AFR), each sent its LD Store, its
  # t_far and its far window.
  sent <- ld_bodies("/api/tophits")
  sent <- sent[seq_along(sent) > before]
  expect_equal(lapply(sent, function(b) unlist(b$analyses)), list(cad, bmi, afr_analysis))
  eur_clump <- list(store = eur, r2 = t_far(src, "EUR"), kb = 10000)
  expect_equal(lapply(sent, `[[`, "clump"),
               list(eur_clump, eur_clump, list(store = "hgdp1kgp-v2-AFR-k10", r2 = t_far(src, "AFR"), kb = 10000)))
  # The answer is in the order asked for. The leads (the fake's greedy clumping of fake_ld_r()) have
  # clump_size from the API and the clumping provenance columns after the Association columns.
  expect_equal(unique(top$id), c(cad, afr_analysis, bmi))
  expect_equal(top$alid[1:3], c("9:22098620:A:G", "9:22098575:A:G", "1:5000:A:G"))
  expect_equal(utils::tail(names(top), 5), clump_columns)
  expect_equal(top$clump_size[1:3], c(3L, 2L, 2L))
  expect_equal(top$clump_ld, ifelse(top$id == afr_analysis, "hgdp1kgp-v2-AFR-k10", eur))
  expect_equal(unique(top$clump_ld_choice), "matched")
  expect_equal(unique(top$clump_kb), 10000)
  # The API's counts, per Analysis in the order asked for; the one top hit the AFR LD Store doesn't hold is warned about.
  prov <- og_provenance(top)
  expect_equal(prov$clump$id, c(cad, afr_analysis, bmi))
  expect_equal(prov$clump[1:2, c("hits", "absent", "leads")], data.frame(hits = c(5, 3), absent = c(0, 1), leads = c(2, 1)))
  expect_equal(w$count, 1)
  expect_equal(w$ids, afr_analysis)
  expect_match(format(prov), "Clumped: OGS-00011:GCST003116 in hgdp1kgp-v2-EUR-k10 (matched)", fixed = TRUE, all = FALSE)
  # `limit` counts the leads (the API's), and says so when it cuts them off; og_submit() collects the
  # very same answer, and says nothing with progress off.
  withr::local_options(opengwasr.progress = FALSE)
  w <- expect_warning(og_tophits(src, cad, limit = 1, releases = rel), class = "opengwasr_truncated")
  expect_match(conditionMessage(w), "more than 1 index variants", fixed = TRUE)
  expect_silent(handle <- og_submit(src, og_tophits, c(cad, afr_analysis, bmi), releases = rel))
  expect_equal(suppressWarnings(og_job_result(src, handle)), top, ignore_attr = TRUE)
})

test_api("og_tophits(): the Source's `ld_store` names an LD Store or an ancestry; an Unassigned Analysis is refused before any Job; the API refuses r2 below t_far beyond 3 Mb", {
  cad_top <- function(s) suppressMessages(suppressWarnings(og_tophits(s, cad, limit = 2, releases = rel)))
  by_name <- cad_top(with_ld(src, eur))
  by_ancestry <- cad_top(with_ld(src, "EUR"))
  expect_equal(unique(by_name$clump_ld_choice), "user")
  expect_equal(unique(by_ancestry$clump_ld_choice), "ancestry_default")
  expect_equal(by_name$alid, by_ancestry$alid)
  before <- if (backend == "fake") length(ld_bodies("/api/tophits"))
  e <- expect_error(og_tophits(src, c(cad, unassigned)), class = "opengwasr_ld_unmatched")
  expect_match(conditionMessage(e), unassigned, fixed = TRUE)
  if (backend == "fake") expect_length(ld_bodies("/api/tophits"), before)
  said <- suppressMessages(suppressWarnings(og_tophits(with_ld(src, "AMR"), unassigned, limit = 2)))
  expect_equal(og_provenance(said)$clump$ld, "hgdp1kgp-v2-AMR-k10")
  e <- expect_error(suppressMessages(og_tophits(src, cad, r2 = 0.001, kb = 10000, releases = rel)),
                    class = "opengwasr_ld_refusal")
  expect_s3_class(e, "opengwasr_refusal")
  expect_equal(e$reason, "threshold_below_t_far")
  expect_equal(e$near_bp, 3000000)
  # Sent as given, never adjusted to something the LD Store could answer.
  if (backend == "fake") {
    expect_equal(utils::tail(ld_bodies("/api/tophits"), 1)[[1]]$clump, list(store = eur, r2 = 0.001, kb = 10000))
  }
})

test_api("og_clump() clumps a data frame of Associations as one set, in the LD Store of its Analyses' ancestry", {
  top <- suppressWarnings(og_tophits(src, cad, clump = FALSE, limit = 5, releases = rel))
  joint <- suppressWarnings(og_clump(src, top))
  expect_true(all(joint$alid %in% top$alid))
  expect_equal(utils::tail(names(joint), 5), clump_columns)
  expect_equal(unique(joint$clump_ld), eur)
  expect_equal(unique(joint$clump_ld_choice), "matched")
  expect_equal(unique(joint$clump_r2), t_far(src, "EUR"))
  prov <- og_provenance(joint)
  expect_true(is.na(prov$clump$id))
  expect_equal(prov$clump$leads, nrow(joint))
  expect_equal(sum(joint$clump_size) + prov$clump$absent, nrow(top))
  expect_equal(prov$query, "og_clump()")
  if (backend == "fake") {
    expect_equal(joint$alid, c("9:22098620:A:G", "9:22098575:A:G"))
    expect_equal(joint$clump_size, c(3L, 2L))
    sent <- utils::tail(ld_bodies("/api/ld/clump"), 1)[[1]]
    expect_equal(unlist(sent$scores), abs(top$z))
    expect_equal(sent[c("store", "r2", "kb")], list(store = eur, r2 = t_far(src, "EUR"), kb = 10000))
  }
})

test_that("og_clump(): Associations of mixed or Unassigned ancestry are refused unless the Source has an LD Store; a variant in several Analyses is ranked by its largest |z|", {
  fake_set(fake, "afr_analysis", "true", "false")
  src <- fake_source(fake)
  before <- length(ld_bodies("/api/ld/clump"))
  mixed <- data.frame(id = c(cad, afr_analysis), alid = pair, z = c(5, -9))
  expect_error(og_clump(src, mixed), class = "opengwasr_ld_unmatched")
  expect_error(og_clump(src, data.frame(id = unassigned, alid = snv, z = 3)), class = "opengwasr_ld_unmatched")
  expect_error(og_clump(src, mixed[c("alid", "z")]), class = "opengwasr_ld_unmatched")
  expect_length(ld_bodies("/api/ld/clump"), before)
  expect_error(og_clump(with_ld(src, "EUR"), mixed["alid"]), class = "opengwasr_argument")
  expect_error(og_clump(with_ld(src, "EUR"), mixed, r2 = "0.1"), class = "opengwasr_argument")
  twice <- rbind(mixed, data.frame(id = afr_analysis, alid = snv, z = 2))
  kept <- og_clump(with_ld(src, "EUR"), twice)
  expect_equal(unlist(utils::tail(ld_bodies("/api/ld/clump"), 1)[[1]]$scores), c(5, 9))
  expect_equal(unique(kept$clump_ld_choice), "ancestry_default")
  expect_true(all(twice$id[twice$alid %in% kept$alid] %in% kept$id))
})

test_that("every r2 in a request body, at any depth, is sent rounded up to 15 digits, never down", {
  t_far <- 0.03490787269681742  # the dev Data API's AFR t_far: 17 digits, which 15 would round down
  body <- list(r2 = t_far, clump = list(store = eur, r2 = t_far, kb = 10000), scores = list(t_far))
  # As api_request() sends it: 15 significant digits.
  sent <- jsonlite::fromJSON(jsonlite::toJSON(round_up_r2(body), auto_unbox = TRUE, digits = NA))
  expect_gte(sent$r2, t_far)
  expect_gte(sent$clump$r2, t_far)
  expect_identical(signif(sent$clump$r2, 15), sent$clump$r2)
  expect_equal(sent$scores, signif(t_far, 15))  # only an `r2` is rounded up
})

test_api("an explicit r2 equal to an LD Store's t_far is sent at or above it, in top hits, clumping and proxies", {
  t_far <- 0.03490787269681742
  hits <- data.frame(id = cad, alid = pair, z = c(5, 4))
  og_clump(with_ld(src, eur), hits, r2 = t_far, kb = 10000)
  og_ld_proxies(with_ld(src, eur), snv, r2 = t_far, kb = 10000)
  suppressWarnings(og_tophits(with_ld(src, eur), cad, r2 = t_far, kb = 10000, limit = 2, releases = rel))
  for (endpoint in c("/api/ld/clump", "/api/ld/proxies")) {
    sent <- ld_bodies(endpoint)
    expect_gte(sent[[length(sent)]]$r2, t_far)
  }
  sent <- ld_bodies("/api/tophits")
  expect_gte(sent[[length(sent)]]$clump$r2, t_far)
}, live = FALSE)

test_api("og_ld_reflookup() says which variants the LD Store holds, without a warning for the absent", {
  absent <- if (backend == "fake") "1:2999:A:G" else "1:100:A:G"
  expect_no_warning(r <- og_ld_reflookup(with_ld(src, "EUR"), c(snv, absent, "rs1421085")))
  expect_equal(names(r), c("alid", "rsid", "present"))
  expect_equal(r$alid[1:2], c(snv, absent))
  expect_equal(r$present[1:2], c(TRUE, FALSE))
  expect_equal(r$rsid[1:2], c(NA_character_, NA_character_))
  expect_true("rs1421085" %in% r$rsid)
  expect_true(any(r$present[r$rsid %in% "rs1421085"]))  # an rsID can name an ALID the LD Store lacks
  prov <- og_provenance(r)
  expect_equal(prov$ld, eur)
  expect_true(absent %in% prov$absent)
  # An rsID no GWAS Store holds is a row, not a gap.
  none <- og_ld_reflookup(with_ld(src, "EUR"), "rs1")
  expect_equal(none$rsid, "rs1")
  expect_true(is.na(none$alid))
  expect_false(none$present)
})

test_that("og_ld_reflookup() takes the rsid column of Associations, and splits", {
  src <- fake_source(fake)
  data <- data.frame(id = cad, alid = c(snv, "5:100000:C:T"), rsid = c("rs6567160", "rs9"))
  r <- og_ld_reflookup(with_ld(src, "EUR"), data)
  expect_equal(r$rsid, c("rs6567160", "rs9"))
  expect_equal(r$alid, data$alid)
  expect_equal(job_limits()$ld_reflookup, 10000)
  withr::local_options(opengwasr.job_limits = list(ld_reflookup = 2))
  v <- c(snv, "5:100000:C:T", "6:200000:C:T")
  r3 <- og_ld_reflookup(with_ld(src, "EUR"), v)
  expect_equal(r3$alid, v)
  expect_equal(length(ld_bodies("/api/ld/reflookup")) >= 2, TRUE)
})

test_that("og_ld_reflookup() gives one row per variant asked, in the order asked, duplicates kept", {
  src <- fake_source(fake)
  v <- c("6:200000:C:T", snv, "6:200000:C:T")
  r <- og_ld_reflookup(with_ld(src, "EUR"), v)
  expect_equal(r$alid, v)
  expect_equal(nrow(r), 3)
  expect_identical(r$present[[1]], r$present[[3]])
})

# The Source's LD resource (#34, ADR 0006, and #43): `og_api(ld_store = )` answers every LD verb, and
# is the only place LD is chosen; other LD is another Source, `og_api(inherit = src, ld_store = )`.
test_that("og_api(ld_store = ) holds the Source's LD resource and prints it; geno_store is refused until the API serves Genotype Stores", {
  src_eur <- fake_source(fake, ld_store = "EUR")
  expect_equal(og_ld_resource(src_eur), "EUR")
  expect_true("\u2022 LD resource: LD Store \"EUR\"" %in% format(src_eur))
  expect_false(any(grepl("LD resource", format(fake_source(fake)), fixed = TRUE)))
  expect_null(og_ld_resource(fake_source(fake)))
  e <- expect_error(fake_source(fake, geno_store = "OGL-00001"), class = "opengwasr_ld_resource_unavailable")
  expect_equal(e$resource, "geno_store")
  expect_s3_class(e, "opengwasr_refusal")
  expect_false(inherits(e, "opengwasr_local_unsupported"))
  expect_error(fake_source(fake, ld_store = c("EUR", "AFR")), class = "opengwasr_argument")
  expect_error(fake_source(fake, ld_store = NA_character_), class = "opengwasr_argument")
  expect_error(fake_source(fake, ld_store = 1), class = "opengwasr_argument")
  # A PLINK Reference is a Local Source's LD resource: the API Source holds LD Stores only.
  ref <- structure(list(path = "ref", plink_bin = "plink"), class = "og_plink_reference")
  expect_error(fake_source(fake, ld_store = ref), class = "opengwasr_argument")
})

test_that("the LD verbs take no `ld`: an `ld =` call fails", {
  src <- fake_source(fake)
  data <- data.frame(id = cad, alid = pair, z = c(5, 4))
  # R's own unused-argument error, before anything is sent.
  before <- length(raw_api(fake$url(), fake_key, "_fake/bodies"))
  expect_error(og_tophits(src, cad, ld = "EUR"), regexp = "unused argument")
  expect_error(og_clump(src, data, ld = "EUR"), regexp = "unused argument")
  expect_error(og_ld_matrix(src, pair, ld = "EUR"), regexp = "unused argument")
  expect_error(og_ld_proxies(src, snv, ld = "EUR"), regexp = "unused argument")
  expect_error(og_ld_reflookup(src, snv, ld = "EUR"), regexp = "unused argument")
  expect_length(raw_api(fake$url(), fake_key, "_fake/bodies"), before)
  for (verb in list(og_tophits, og_clump, og_ld_matrix, og_ld_proxies, og_ld_reflookup)) {
    expect_false("ld" %in% names(formals(verb)))
  }
  # No Source is no Source: the `NULL` methods for a PLINK Reference went with `ld`.
  expect_error(og_clump(NULL, data, r2 = 0.1, kb = 1), class = "opengwasr_source_required")
  expect_error(og_ld_matrix(NULL, "rs1"), class = "opengwasr_source_required")
})

test_that("og_api(inherit = src, ld_store = ) keeps src's URL, key, timeout, max_in_flight and caller, and swaps the LD resource", {
  src <- fake_source(fake, timeout = 99, max_in_flight = 7, caller = "Tool/1.0", ld_store = "EUR")
  # The key is the inherited Source's, whatever the environment now says.
  afr <- withr::with_envvar(c(OPENGWAS_URL = "http://elsewhere.invalid", OPENGWAS_DEV_KEY = "another-key"),
                            og_api(inherit = src, ld_store = "AFR"))
  expect_s3_class(afr, "og_api_source")
  expect_equal(afr$url, src$url)
  expect_equal(afr$credentials(), src$credentials())
  expect_equal(afr$signed_in_with, src$signed_in_with)
  expect_equal(afr$timeout, 99)
  expect_equal(afr$max_in_flight, 7L)
  expect_equal(afr$caller, "Tool/1.0")
  expect_equal(og_ld_resource(afr), "AFR")
  expect_equal(og_ld_resource(src), "EUR")  # unchanged
  # Without an LD argument the LD resource is inherited too; any given argument replaces its own.
  same <- og_api(inherit = src, timeout = 5)
  expect_equal(og_ld_resource(same), "EUR")
  expect_equal(c(same$timeout, same$max_in_flight), c(5, 7))
  expect_null(og_ld_resource(og_api(inherit = src, ld_store = NULL)))
  # An explicit NULL drops the inherited value, as for the LD resource.
  no_caller <- og_api(inherit = src, caller = NULL)
  expect_null(no_caller$caller)
  expect_equal(c(no_caller$timeout, no_caller$max_in_flight), c(99, 7))
  expect_equal(og_ld_resource(no_caller), "EUR")
  # It answers on the fake with src's key: the AFR LD Store.
  m <- suppressWarnings(og_ld_matrix(afr, pair))
  expect_equal(og_provenance(m)$ld, "hgdp1kgp-v2-AFR-k10")
  expect_equal(og_provenance(m)$ld_from, "source")
  # The constructor's own checks apply to what is given.
  expect_error(og_api(inherit = src, timeout = -1), class = "opengwasr_argument")
  expect_error(og_api(inherit = src, ld_store = 1), class = "opengwasr_argument")
  e <- expect_error(og_api(inherit = src, geno_store = "OGL-00001"), class = "opengwasr_ld_resource_unavailable")
  expect_equal(e$resource, "geno_store")
  # Another URL reads the key again, as og_api() does.
  e <- withr::with_envvar(c(OPENGWAS_DEV_KEY = NA), expect_error(og_api(inherit = src, url = "http://x.invalid"), class = "opengwasr_auth"))
})

test_that("og_api(inherit = ) must be an API Source", {
  e <- expect_error(og_api(inherit = list(url = "x")), class = "opengwasr_argument")
  expect_equal(e$arg, "inherit")
  ldonly <- structure(list(releases = data.frame(), ld = NULL), class = "og_local_source")
  expect_error(og_api(inherit = ldonly), class = "opengwasr_argument")
})

test_that("og_tophits() clumps in the API Source's LD resource, sending the same body whichever way the Source was made", {
  src <- fake_source(fake)
  src_eur <- fake_source(fake, ld_store = "EUR")
  top <- function(s, ...) suppressWarnings(og_tophits(s, cad, limit = 2, releases = rel, ...))
  by_source <- top(src_eur)
  sent_source <- utils::tail(ld_bodies("/api/tophits"), 1)[[1]]
  by_inherit <- top(with_ld(src, "EUR"))
  sent_inherit <- utils::tail(ld_bodies("/api/tophits"), 1)[[1]]
  expect_equal(sent_source, sent_inherit)
  expect_equal(sent_source$clump$store, eur)
  expect_equal(by_source, by_inherit, ignore_attr = TRUE)
  expect_equal(unique(by_source$clump_ld), eur)
  expect_equal(unique(by_source$clump_ld_choice), "ancestry_default")
  expect_equal(og_provenance(by_source)$clump, og_provenance(by_inherit)$clump)
  expect_equal(og_provenance(by_source)$clump$clumps, "computed")
  expect_equal(c(og_provenance(by_source)$ld_from, og_provenance(by_inherit)$ld_from), c("source", "source"))
  expect_equal(og_provenance(top(src))$ld_from, "ancestry")
  # An Unassigned Analysis, refused on `src`, is clumped in the Source's LD Store.
  expect_error(og_tophits(src, unassigned), class = "opengwasr_ld_unmatched")
  in_eur <- suppressWarnings(og_tophits(src_eur, unassigned, limit = 2))
  expect_equal(og_provenance(in_eur)$clump$ld, eur)
  in_amr <- suppressWarnings(og_tophits(with_ld(src_eur, "AMR"), unassigned, limit = 2))
  expect_equal(og_provenance(in_amr)$clump$ld, "hgdp1kgp-v2-AMR-k10")
  # src_eur is unchanged by the Source made from it.
  again <- suppressWarnings(og_tophits(src_eur, unassigned, limit = 2))
  expect_equal(og_provenance(again)$clump$ld, eur)
  # A name is the user's choice.
  by_name <- top(fake_source(fake, ld_store = eur))
  expect_equal(unique(by_name$clump_ld_choice), "user")
  # The progress message says where the LD came from.
  withr::local_options(opengwasr.progress = TRUE)
  m <- expect_message(top(src_eur), class = "opengwasr_clumping")
  expect_equal(m$ld_from, "source")
  expect_match(conditionMessage(m), "as the Source's LD resource asks", fixed = TRUE)
  expect_equal(suppressWarnings(expect_message(og_tophits(src, cad, limit = 2, releases = rel), class = "opengwasr_clumping"))$ld_from, "ancestry")
})

test_that("og_ld_matrix(), og_ld_proxies(), og_ld_reflookup() and og_clump() read the API Source's LD resource", {
  src <- fake_source(fake)
  src_eur <- fake_source(fake, ld_store = "EUR")
  # A plain vector has no ancestry to match: refused without an LD resource, answered with one.
  expect_error(og_ld_matrix(src, pair), class = "opengwasr_ld_unmatched")
  m <- og_ld_matrix(src_eur, pair)
  expect_equal(og_provenance(m)$ld, eur)
  expect_equal(og_provenance(m)$ld_from, "source")
  expect_equal(og_provenance(m)$ld_choice, "ancestry_default")
  m_afr <- suppressWarnings(og_ld_matrix(with_ld(src_eur, "hgdp1kgp-v2-AFR-k10"), pair))
  expect_equal(og_provenance(m_afr)$ld, "hgdp1kgp-v2-AFR-k10")
  expect_equal(og_provenance(m_afr)$ld_choice, "user")
  expect_equal(og_provenance(m_afr)$ld_from, "source")
  expect_equal(og_provenance(og_ld_matrix(src, associations_of(cad, pair)))$ld_from, "ancestry")
  # Associations of an Unassigned Analysis: no ancestry is matched when the Source has an LD Store.
  expect_error(og_ld_matrix(src, associations_of(unassigned, pair)), class = "opengwasr_ld_unmatched")
  expect_equal(og_provenance(og_ld_matrix(src_eur, associations_of(unassigned, pair)))$ld, eur)
  px <- og_ld_proxies(src_eur, snv)
  expect_equal(og_provenance(px)$ld, eur)
  expect_equal(og_provenance(px)$ld_from, "source")
  rl <- og_ld_reflookup(src_eur, snv)
  expect_equal(og_provenance(rl)$ld, eur)
  expect_equal(og_provenance(og_ld_reflookup(src, associations_of(cad, snv)))$ld_from, "ancestry")
  data <- data.frame(alid = pair, z = c(5, 4))  # no `id`: no ancestry to match
  expect_error(og_clump(src, data), class = "opengwasr_ld_unmatched")
  joint <- suppressWarnings(og_clump(src_eur, data))
  expect_equal(unique(joint$clump_ld), eur)
  expect_equal(og_provenance(joint)$clump$clumps, "computed")
  expect_equal(og_provenance(joint)$ld_from, "source")
  e <- expect_error(og_clump(src, data), class = "opengwasr_ld_unmatched")
  expect_match(conditionMessage(e), "og_api(inherit = src, ld_store = ", fixed = TRUE)
})

# og_ld_resource() (#36): what a package built on opengwasr reads to honour a Source's LD resource.
test_that("og_ld_resource() gives the API Source's LD resource, NULL without one, and refuses a non-Source", {
  expect_null(og_ld_resource(fake_source(fake)))
  expect_equal(og_ld_resource(fake_source(fake, ld_store = "EUR")), "EUR")
  expect_equal(og_ld_resource(fake_source(fake, ld_store = eur)), eur)
  expect_error(og_ld_resource(NULL), class = "opengwasr_source_required")
  expect_error(og_ld_resource("EUR"), class = "opengwasr_argument")
  expect_error(og_ld_resource(list(ld = "EUR")), class = "opengwasr_argument")
})
