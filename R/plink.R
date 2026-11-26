# PLINK References: the user's own PLINK binary fileset as another place LD is read from.
#
# A Local Source carries one as its LD resource (`og_local(bfile = , plink = )`), which
# plink_reference() builds: choose_ld_store() in R/ld-stores.R is the one place it is interpreted.
# This file is the whole of the PLINK side: finding the binary, running it, the two things it is
# asked (an LD matrix, a clumping), matching variants to the reference by rsID (ADR 0004), and the
# plan and top-hit assembly of a query that reads one. The other files only branch on a PLINK
# Reference where the LD is chosen or its result is handed over. Nothing here calls the API: the LD
# query engine (R/ld-query.R) and the clumping contribution (R/clump.R) decide what to ask, and they
# call these.
#
# Variants are matched to the reference by rsID, because PLINK References are usually on GRCh37
# while ALIDs are on GRCh38. PLINK has no option to rank by |z|, so the variants are ranked by |z| in
# R and PLINK is given the ranks as p-values (plink_clump()). PLINK 1.9 and PLINK 2 (2.0.0-a.6.0 or
# later) are each asked for the same thing in their own words (plink_dialects); the version is read
# once, when the reference is made, and kept on it and in the provenance of what it computes.

# The PLINK Reference of `og_local(bfile = , plink = )`: the fileset's prefix `path` and the PLINK
# `plink_bin`, checked (the fileset exists; PLINK is found, or the one named is supported). `args`
# names them as the caller does, for the errors, which carry the one at fault as `arg`. Returns an
# `og_plink_reference`: `path` (absolute, with forward slashes on every platform), `plink_bin` (the
# binary's absolute path) and `plink_version` (as PLINK reports it).
plink_reference <- function(path, plink_bin, args = c(path = "bfile", plink_bin = "plink"),
                            call = rlang::caller_env()) {
  # Kept: client-side, on the user's own files. The API isn't involved.
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    og_abort(paste0("`", args[["path"]], "` must be the prefix of a PLINK fileset, such as \"~/ld/1kg/EUR\"."),
             class = "opengwasr_argument", arg = args[["path"]], call = call)
  }
  prefix <- path.expand(path)
  files <- paste0(prefix, c(".bed", ".bim", ".fam"))
  missing <- files[!file.exists(files)]
  if (length(missing) > 0) {
    og_abort(c(paste0("`", args[["path"]], "` isn't a PLINK fileset: ", paste(basename(missing), collapse = ", "),
                      if (length(missing) == 1) " is" else " are", " missing next to \"", prefix, "\"."),
               i = "Give the prefix of the `.bed`, `.bim` and `.fam` files, without the extension."),
             class = "opengwasr_plink_reference_missing", files = missing, arg = args[["path"]], call = call)
  }
  found <- find_plink(plink_bin, call, arg = args[["plink_bin"]])
  structure(list(path = file.path(normalise_path(dirname(prefix)), basename(prefix)),
                 plink_bin = found$bin, plink_version = found$version),
            class = "og_plink_reference")
}

#' @export
format.og_plink_reference <- function(x, ...) {
  c("<og_plink_reference>", paste0("  ", x$path), plink_binary_line(x))
}

# The PLINK a reference runs, as printed under it here and under a Local Source.
plink_binary_line <- function(ref) paste0("  PLINK ", ref$plink_version %||% "(version not read yet)", ": ", ref$plink_bin)

#' @export
print.og_plink_reference <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

# The PLINK binary, `list(bin, version)`: `plink_bin` if given (refused if it isn't PLINK 1.9 or a
# supported PLINK 2), else the first supported of genetics.binaRies', `plink` and `plink2` on the
# PATH. A candidate the search finds that isn't supported (PuTTY's `plink`, PLINK 1.07) is skipped;
# if none is supported, the error lists each, in `skipped`.
# Kept (here and in plink_run()): PLINK runs on this machine; the API isn't involved.
find_plink <- function(plink_bin, call = rlang::caller_env(), arg = "plink_bin") {
  if (!is.null(plink_bin)) {
    path <- plink_path(plink_bin)
    if (is.na(path)) {
      og_abort(paste0("`", arg, "` isn't an executable file or a command on your `PATH`: ", format(plink_bin), "."),
               class = c("opengwasr_plink_not_found", "opengwasr_argument"), arg = arg, call = call)
    }
    return(list(bin = path, version = plink_version(path, call, arg)))
  }
  skipped <- data.frame(plink_bin = character(), version = character(), stringsAsFactors = FALSE)
  for (found in c(packaged_plink(), unname(Sys.which(c("plink", "plink2"))))) {
    path <- plink_path(found)
    if (is.na(path) || path %in% skipped$plink_bin) next
    version <- plink_probe(path)
    if (!is.null(plink_dialect_of(version))) return(list(bin = path, version = version))
    skipped[nrow(skipped) + 1, ] <- list(path, version)
  }
  og_abort(c(if (nrow(skipped) == 0) "Can't find PLINK 1.9 or 2, which a PLINK Reference needs."
             else "Can't find PLINK 1.9 or 2, which a PLINK Reference needs. These were found, and skipped:",
             stats::setNames(paste0(skipped$plink_bin, ": ", plink_found_as(skipped$version)), rep("*", nrow(skipped))),
             i = "Install the genetics.binaRies package: `remotes::install_github(\"MRCIEU/genetics.binaRies\")`.",
             i = paste0("Or install PLINK 1.9 (https://www.cog-genomics.org/plink/1.9/) or PLINK 2 (https://www.cog-genomics.org/plink/2.0/) ",
                        "and put it on your `PATH` as `plink` or `plink2`, or pass its path or command as `", arg, "`.")),
           class = "opengwasr_plink_not_found", skipped = skipped, supported = plink_supported(), arg = arg, call = call)
}

# An executable's absolute path, or NA. As a shell decides: a value with a path separator is a path,
# one without is a command, looked up on the PATH even when a file of that name is here.
plink_path <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) return(NA_character_)
  separator <- if (.Platform$OS.type == "windows") "[/\\\\]" else "/"
  path <- if (grepl(separator, x)) path.expand(x) else unname(Sys.which(x))
  if (!nzchar(path) || !file.exists(path) || dir.exists(path) || unname(file.access(path, 1)) != 0) return(NA_character_)
  normalise_path(path)
}

# genetics.binaRies' PLINK, if the package is installed, else NULL. The package is optional, and not
# on CRAN, so it is looked up by name rather than declared.
packaged_plink <- function() {
  pkg <- "genetics.binaRies"
  if (!requireNamespace(pkg, quietly = TRUE)) return(NULL)
  tryCatch(get("get_plink_binary", envir = asNamespace(pkg))(), error = function(e) NULL)
}

# The version `bin` reports to `--version`, as it reports it ("1.90b6.21", "2.0.0-b.1.4"), or NA when
# it reports none (it isn't PLINK).
plink_probe <- function(bin) {
  out <- tryCatch(suppressWarnings(system2(bin, "--version", stdout = TRUE, stderr = TRUE, timeout = 60)),
                  error = function(e) character())
  # "PLINK v1.90b6.21 64-bit (19 Oct 2020)", "PLINK v2.0.0-b.1.4 64-bit ...", 1.07's "|  PLINK!  |  v1.07  |".
  at <- regmatches(out, regexec("PLINK[! |]*v([0-9][^ |]*)", out))
  at <- Filter(function(m) length(m) == 2, at)
  if (length(at) > 0) at[[1]][[2]] else NA_character_
}

# The version of `bin`, a PLINK the user named (read once, when a PLINK Reference is made, or on first
# use of one made without it): refused unless plink_dialect_of() knows it. The refusal suggests a
# supported `plink` or `plink2` on the PATH, if there is one.
plink_version <- function(bin, call = rlang::caller_env(), arg = "plink_bin") {
  version <- plink_probe(bin)
  if (!is.null(plink_dialect_of(version))) return(version)
  suggest <- NULL
  for (command in c("plink", "plink2")) {
    other <- plink_path(command)
    if (!is.na(other) && other != bin && !is.null(plink_dialect_of(plink_probe(other)))) {
      suggest <- command
      break
    }
  }
  og_abort(c(paste0("opengwasr works with PLINK ", paste(plink_supported(), collapse = " or "), ", but ", bin, " ",
                    plink_found_as(version), "."),
             if (!is.null(suggest)) c(i = paste0("A supported PLINK is on your `PATH`: `", arg, " = \"", suggest, "\"`.")),
             i = paste0("Install PLINK 1.9 or PLINK 2 (https://www.cog-genomics.org/plink/), and pass its path or command as `", arg, "`.")),
           class = "opengwasr_plink_unsupported", version = version, supported = plink_supported(),
           suggest = suggest, plink_bin = bin, arg = arg, call = call)
}

plink_found_as <- function(version) {
  ifelse(is.na(version), "reports no PLINK version", paste0("is PLINK ", version))
}

# What each supported PLINK is asked, in its own words, for the same answer
# (tests/testthat/test-plink-versions.R shows they agree on the committed fileset, and with PLINK's
# rule worked out in R). `series` matches the version it reports; `min`, if any, is the first build
# that has every flag used here.
# - `r`: the signed r of every pair, a square text matrix in the reference's order, its sign referring
#   to the `.bim`'s first allele of each variant. 1.9 keeps that allele's order with
#   `--keep-allele-order`; PLINK 2 always keeps the order (first allele ALT, second REF), and
#   `ref-based` signs r by the REF alleles, the same sign as by the ALT ones. Without it, PLINK 2
#   signs by the major alleles.
# - `clumps`: `--clump`'s report and its ID column. PLINK 2's default r² for clumping (from haplotype
#   frequencies, estimated from unphased genotypes) is the one 1.9 uses; `--clump-unphased` would
#   take the squared genotype correlation instead, and give different clumps.
# PLINK 2 gained `--clump` in September 2023 and `--r-unphased` in December 2023, both in alpha 6
# development builds, which report "2.00a6"; `ref-based` came in January 2024. 2.0.0-a.6.0 (November
# 2024) is the first numbered build with all three, and the earlier ones ("2.00a<n>") are refused.
plink_dialects <- list(
  list(name = "1.9", series = "^1\\.9", min = NULL,
       r = c("--r", "square", "--keep-allele-order"), r_file = ".ld", clumps_file = ".clumped", id = "SNP"),
  list(name = "2", series = "^2\\.0", min = "2.0.0-a.6.0",
       r = c("--r-unphased", "square", "ref-based"), r_file = ".unphased.vcor1", clumps_file = ".clumps", id = "ID")
)

# The supported versions, as a refusal names them.
plink_supported <- function() {
  vapply(plink_dialects, function(d) if (is.null(d$min)) d$name else paste0(d$name, " (", d$min, " or later)"), "")
}

# The dialect of a PLINK that reports `version`, or NULL if none speaks it.
plink_dialect_of <- function(version) {
  if (length(version) != 1 || is.na(version)) return(NULL)
  for (d in plink_dialects) {
    if (grepl(d$series, version) && (is.null(d$min) || plink_at_least(version, d$min))) return(d)
  }
  NULL
}

# Whether `version` is `min` or later, both semantic versions as PLINK 2 has written them since
# 2.0.0-a.6.0 ("2.0.0-a.6.9LM", "2.0.0-b.1.4"; a release with no "-a."/"-b." comes after them).
# Anything else (PLINK 2's earlier "2.00a5.12") isn't.
plink_at_least <- function(version, min) {
  key <- function(v) {
    m <- regmatches(v, regexec("^([0-9]+)\\.([0-9]+)\\.([0-9]+)(-([a-z]+)\\.([0-9]+)\\.([0-9]+))?", v))[[1]]
    if (length(m) == 0) return(NULL)
    stage <- if (nzchar(m[[6]])) match(m[[6]], c("a", "b", "rc")) else 9
    as.numeric(c(m[2:4], stage, if (nzchar(m[[6]])) m[7:8] else c(0, 0)))
  }
  v <- key(version)
  if (is.null(v) || anyNA(v)) return(FALSE)
  m <- key(min)
  differ <- which(v != m)
  length(differ) == 0 || v[[differ[[1]]]] > m[[differ[[1]]]]
}

# What `ref`'s PLINK is asked. A reference made before its version was recorded (one saved with
# saveRDS() by an earlier opengwasr) has it read now, and refused as plink_version() refuses.
plink_dialect <- function(ref, call = rlang::caller_env()) {
  plink_dialect_of(plink_with_version(ref, call)$plink_version)
}

# `ref`, with `plink_version` read if it has none.
plink_with_version <- function(ref, call = rlang::caller_env()) {
  if (is.null(ref$plink_version)) ref$plink_version <- plink_version(ref$plink_bin, call)
  ref
}

# The reference's variants, from its `.bim`: `rsid`, `a1`, `a2`, in file order. Alleles are read as
# text (a column of all "T" would otherwise be logical).
plink_bim <- function(ref) {
  bim <- utils::read.table(paste0(ref$path, ".bim"), header = FALSE, quote = "", comment.char = "",
                           colClasses = c("character", "character", "NULL", "NULL", "character", "character"),
                           col.names = c("chromosome", "rsid", "cm", "bp", "a1", "a2"),
                           stringsAsFactors = FALSE)[c("rsid", "a1", "a2")]
  bim$rsid <- tolower(bim$rsid)
  bim
}

# The one runner: writes `files` (named character vectors, one line each) into a temporary
# directory, runs PLINK on the reference with `flags(file)` (`file(name)` is a written file's path),
# gives `read(out)` the output prefix, and removes the directory whatever happens.
plink_run <- function(ref, files, flags, read, call = rlang::caller_env()) {
  dir <- tempfile("opengwasr-plink-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  for (name in names(files)) writeLines(files[[name]], file.path(dir, name))
  out <- file.path(dir, "out")
  # One thread: PLINK otherwise takes every core, for runs of milliseconds.
  args <- c("--bfile", ref$path, flags(function(name) file.path(dir, name)), "--threads", "1", "--out", out)
  log <- suppressWarnings(system2(ref$plink_bin, shQuote(args), stdout = TRUE, stderr = TRUE))
  status <- attr(log, "status") %||% 0L
  if (status != 0) {
    og_abort(c(paste0("PLINK failed (exit status ", status, ") on the PLINK Reference \"", ref$path, "\"."),
               x = utils::tail(log[nzchar(log)], 3),
               i = paste0("PLINK is ", ref$plink_bin, ".")),
             class = "opengwasr_plink_failed", status = status, log = log, call = call)
  }
  read(out)
}

# The r between every pair of `rsids` (all in the reference): a square matrix named
# `<rsid>_<a1>_<a2>` with the reference's alleles, in the order of `rsids`. The sign of r refers to
# the first allele in each name (as ieugwasr's ld_matrix_local() did, with `--keep-allele-order`).
plink_ld_matrix <- function(ref, rsids, call = rlang::caller_env()) {
  bim <- plink_bim(ref)
  in_file <- which(bim$rsid %in% rsids)  # PLINK writes the matrix in the reference's order
  if (length(in_file) == 0) return(matrix(NA_real_, 0, 0))
  dialect <- plink_dialect(ref)
  m <- plink_run(ref, list(extract = rsids),
                 function(file) c("--extract", file("extract"), dialect$r),
                 function(out) {
                   # An undefined r (a variant with no variation) is written as "nan", by either PLINK.
                   as.matrix(utils::read.table(paste0(out, dialect$r_file), header = FALSE,
                                               na.strings = c("nan", "-nan", "NA"), colClasses = "numeric"))
                 }, call)
  asked <- order(match(bim$rsid[in_file], rsids), seq_along(in_file))
  names <- paste(bim$rsid[in_file], bim$a1[in_file], bim$a2[in_file], sep = "_")[asked]
  m <- unname(m[asked, asked, drop = FALSE])
  dimnames(m) <- list(names, names)
  m
}

# Clump rows by PLINK: each row's `rsid` (all in the reference) and `z`, an rsID on several rows
# scored by its largest |z|. PLINK's own greedy rule: each index variant takes every unclumped
# variant within `kb` whose r² with it is above `r2`. PLINK ranks by p-value, so it is given each
# variant's rank / (n + 1) (the strongest the smallest, ties in order of first appearance): in (0,
# 1), so `--clump-p1 1 --clump-p2 1` keep every variant, and unlike a p-value from |z| it never
# underflows. Returns `leads`, the index variants' rsIDs in order of first appearance, and `size`,
# for each row, the size of its rsID's clump (itself included) if it leads, else NA.
plink_clump <- function(ref, rsid, z, r2, kb, call = rlang::caller_env()) {
  rsids <- unique(rsid)
  n <- length(rsids)
  if (n <= 1) return(list(leads = rsids, size = rep(1L, length(rsid))))
  score <- as.numeric(tapply(abs(z), factor(rsid, levels = rsids), max))
  p <- rank(-score, ties.method = "first", na.last = TRUE) / (n + 1)
  dialect <- plink_dialect(ref)
  # The scores' header names the ID column "SNP", which PLINK 2's --clump also looks for.
  clumped <- plink_run(ref, list(scores = c("SNP P", sprintf("%s %.15g", rsids, p))),
                       function(file) c("--clump", file("scores"), "--clump-p1", 1, "--clump-p2", 1,
                                        "--clump-r2", format(r2, digits = 15), "--clump-kb", format(kb, digits = 15)),
                       function(out) {
                         file <- paste0(out, dialect$clumps_file)
                         if (!file.exists(file)) {
                           og_abort("PLINK wrote no clumps. This is a bug in opengwasr.", class = "opengwasr_internal", call = call)
                         }
                         # 1.9's `.clumped` is space-aligned; PLINK 2's `.clumps` is tab-separated, its
                         # header starting "#CHROM".
                         utils::read.table(file, header = TRUE, stringsAsFactors = FALSE, comment.char = "",
                                           check.names = FALSE,
                                           colClasses = stats::setNames(c("character", "integer"), c(dialect$id, "TOTAL")))
                       }, call)
  ids <- clumped[[dialect$id]]
  # TOTAL counts the others in the clump.
  list(leads = rsids[rsids %in% ids], size = clumped$TOTAL[match(rsid, ids)] + 1L)
}

# `r2` and `kb` are the user's with a PLINK Reference: it records no defaults to take instead.
check_plink_settings <- function(ld, r2, kb, call = rlang::caller_env()) {
  if (!is_plink_reference(ld) || (!is.null(r2) && !is.null(kb))) return(invisible())
  og_abort(c("A PLINK Reference has no recorded `r2` and `kb`, so clumping with one needs both.",
             i = "For example `r2 = 0.001, kb = 10000`, as ieugwasr's defaults."),
           class = c("opengwasr_plink_settings_required", "opengwasr_argument"), call = call)
}

# One message: how many of the variants matched the reference by rsID. `what` names them.
inform_plink_unmatched <- function(total, left_out, what = "variants") {
  if (left_out == 0) return(invisible())
  rlang::inform(c(i = paste0(big_number(total - left_out), " of ", big_number(total), " ", what,
                             " matched the PLINK Reference by rsID; ", big_number(left_out),
                             " not in it ", if (left_out == 1) "was" else "were", " left out.")),
                class = "opengwasr_plink_unmatched", count = left_out, total = total)
}

# With progress on (`opengwasr.progress`), which PLINK runs.
inform_plink_binary <- function(ref) {
  if (!progress_enabled()) return(invisible())
  rlang::inform(c(i = paste0("Using PLINK ", ref$plink_version, " at ", ref$plink_bin, ".")), class = "opengwasr_plink_binary")
}

is_plink_reference <- function(x) inherits(x, "og_plink_reference")

# A Local Source's PLINK Reference: the variants are matched to it by rsID now, so the message and
# any refusal come before PLINK runs. An ALID's rsID is its row's `rsid` when `variants` is a data
# frame of Associations that has one, else the Source's Store Releases' variant table says (one
# lookup by position); an LD-only Local Source has none, so such ALIDs are refused.
# `kinds` and `chosen` are what ld_prepare() classified and choose_ld_store() decided. Returns what ld_prepare() does, with `plink = list(ref, map, present)`: `map` has one row per
# variant given (`alid`, NA for an rsID; `rsid`, NA for none) and `present` says which the reference holds.
ld_prepare_plink <- function(src, variants, kinds, chosen, deadline, call) {
  ref <- plink_with_version(chosen$plink, call)  # one made before its version was recorded: read it now
  jobs <- list()
  if (is.data.frame(variants)) {
    rsid <- if (is.character(variants$rsid)) variants$rsid[match(kinds$alids, variants$alid)]
    map <- data.frame(alid = kinds$alids, rsid = rsid %||% rep(NA_character_, length(kinds$alids)),
                      stringsAsFactors = FALSE)
    lookup_for <- if (is.null(rsid)) kinds$alids
  } else {
    map <- data.frame(alid = c(kinds$alids, rep(NA_character_, length(kinds$rsids))),
                      rsid = c(rep(NA_character_, length(kinds$alids)), kinds$rsids), stringsAsFactors = FALSE)
    lookup_for <- kinds$alids
  }
  if (length(lookup_for) > 0) {
    check_gwas_source(src, call, alids = lookup_for)
    found <- plink_rsids_of(src, lookup_for, deadline, call)
    map$rsid[match(lookup_for, map$alid)] <- found$rsid[match(lookup_for, found$alid)]
    jobs <- found$jobs
  }
  map$rsid <- rsid_or_na(tolower(map$rsid))
  present <- !is.na(map$rsid) & map$rsid %in% plink_bim(ref)$rsid
  inform_plink_binary(ref)
  inform_plink_unmatched(nrow(map), sum(!present))
  list(alids = kinds$alids, store = chosen$store, choice = chosen$choice, from = chosen$from, unresolved = character(),
       jobs = jobs, plink = list(ref = ref, map = map, present = present))
}

# The rsID of each ALID, from the Source's GWAS Stores' variant tables (one lookup by position, in the sole
# Store Release of each GWAS Store: several is refused, as for any rsID resolution). `rsid` is NA
# where none holds the ALID or it has no rsID ("." in the API). Also the lookup's Jobs.
plink_rsids_of <- function(src, alids, deadline, call) {
  releases <- resolve_releases(src, NULL, call)
  at <- alid_parts(alids)
  positions <- unique(paste0(at$chromosome, ":", at$position))
  out <- run_window(src, lookup_requests("api/variants/chrpos", "positions", positions, releases, "positions"),
                    deadline, call)
  rows <- result_rows(out, empty_lookup())
  rows$rsid <- rsid_or_na(rows$rsid)
  rows <- rows[!duplicated(rows$alid) & !is.na(rows$rsid), , drop = FALSE]
  list(alid = rows$alid, rsid = rows$rsid, jobs = lookup_jobs(lapply(out, `[[`, "job")))
}

# The plan of a query over a PLINK Reference: no requests. PLINK runs when the plan is finished,
# with what was pinned now (the variants matched).
ld_query_plink <- function(prep, spec, method) {
  plink <- prep$plink
  left_out <- with(plink$map, ifelse(is.na(alid), rsid, alid))[!plink$present]
  ctx <- list(alids = prep$alids, absent = left_out, store = prep$store, choice = prep$choice,
              matched = plink$map[plink$present, , drop = FALSE], plink = plink$ref, sets = prep$sets)
  jobs <- prep$jobs
  list(requests = list(), releases = NULL, finish = function(results) {
    done <- spec$local(ctx)
    list(value = done$value, jobs = jobs,
         provenance = c(list(ld = prep$store$ld_store, ld_choice = prep$choice, ld_from = prep$from, absent = left_out,
                             plink_bin = plink$ref$plink_bin, plink_version = plink$ref$plink_version), done$provenance))
  })
}

# The same contribution for a PLINK Reference: the Jobs fetch unclumped top hits (the most the API
# gives per Analysis, several Analyses to a Job), and `finish` clumps each Analysis's on this
# machine, then applies the user's `limit` to its index variants (strongest first), flagging the
# Jobs whose Analyses it cut.
plink_clump_contribution <- function(chosen, clump, info, call) {
  ref <- plink_with_version(chosen$plink, call)  # one made before its version was recorded: read it now
  inform_plink_binary(ref)
  if (progress_enabled()) {
    rlang::inform(c(i = paste0("Clumping ", length(info$id), if (length(info$id) == 1) " Analysis" else " Analyses",
                               " in PLINK Reference ", ref$path, ": ",
                               clump_setting_text(clump$r2, clump$kb), ".")),
                  class = "opengwasr_clumping", ld_from = chosen$from)
  }
  most <- max_tophits_per_analysis
  list(
    along = "scope",
    analyses_per_job = tophits_analyses_per_job(most),
    body = function(chunk) list(limit = most),
    truncated = function(ids) {
      truncated_message(analyses_who(ids), most, "top hits", clump$threshold,
                        cut = paste0(", the most the API returns, so clumping saw only the strongest ", big_number(most)),
                        advice = "Raise the threshold's strictness, or clump on the API Source, whose LD Stores clump every top hit.")
    },
    finish = function(rows, results, pieces, call) {
      in_ref <- !is.na(rows$rsid) & tolower(rows$rsid) %in% plink_bim(ref)$rsid
      keep <- rep(FALSE, nrow(rows))
      size <- rep(1L, nrow(rows))
      summary <- clump_summary(info$id, chosen$store$ld_store, chosen$choice, clump$r2, clump$kb, 0, 0, 0)
      too_many <- character()
      for (i in seq_along(info$id)) {
        mine <- rows$id == info$id[[i]]
        at <- which(mine & in_ref)
        summary$hits[[i]] <- sum(mine)
        summary$absent[[i]] <- sum(mine) - length(at)
        if (length(at) == 0) next
        rsid <- tolower(rows$rsid[at])
        clumped <- plink_clump(ref, rsid, rows$z[at], clump$r2, clump$kb, call)
        leads <- clumped$leads  # in the API's order: strongest first
        summary$leads[[i]] <- length(leads)
        if (length(leads) > clump$limit) {
          too_many <- c(too_many, info$id[[i]])
          leads <- leads[seq_len(clump$limit)]
        }
        keep[at[rsid %in% leads]] <- TRUE
        size[at] <- clumped$size
      }
      inform_plink_unmatched(sum(summary$hits), sum(summary$absent), "top hits")
      if (length(too_many) > 0) {
        og_warn(truncated_message(analyses_who(too_many), clump$limit, "index variants", clump$threshold),
                class = "opengwasr_truncated", n_analyses = length(too_many), call = call)
      }
      out <- rows[keep, , drop = FALSE]
      rownames(out) <- NULL
      list(value = clump_columns(out, chosen$store$ld_store, chosen$choice, clump$r2, clump$kb, size[keep]),
           provenance = list(clump = summary, plink_bin = ref$plink_bin, plink_version = ref$plink_version,
                             ld_from = chosen$from),
           # `limit` cut index variants off: provenance says so, on the Jobs of those Analyses.
           truncated = vapply(pieces, function(p) any(info$id[p$scope] %in% too_many), NA))
    }
  )
}
