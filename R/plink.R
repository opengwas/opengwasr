# PLINK References: the user's own PLINK binary fileset as another place LD is read from.
#
# `og_plink_reference()` makes one, and every LD function takes it through the same `ld` argument
# as an LD Store's name, or a Local Source carries it as its LD resource (`og_local(bfile = )`):
# choose_ld_store() in R/ld-stores.R is the one place either is interpreted.
# This file is the whole of the PLINK side: finding the binary, running it, the two things it is
# asked (an LD matrix, a clumping), matching variants to the reference by rsID (ADR 0004), and the
# plan and top-hit assembly of a query that reads one. The other files only branch on a PLINK
# Reference where `ld` is chosen or its result is handed over. Nothing here calls the API: the LD query engine
# (R/ld-query.R) and the clumping contribution (R/clump.R) decide what to ask, and they call these.

#' A PLINK Reference: your own genotypes for local LD
#'
#' `og_plink_reference()` names a PLINK binary fileset (`.bed`, `.bim`, `.fam`) on your machine, and
#' the PLINK that will read it, to pass as `ld` to [og_ld_matrix()], [og_clump()] and
#' [og_tophits()] for one call, with either Source. A Local Source can also carry one as its LD
#' resource, `og_local(gwas, plink = , bfile = )`, which builds it with this function. See
#' `vignette("ld")`.
#'
#' Variants are matched to the reference by rsID, because PLINK References are usually on GRCh37
#' while ALIDs are on GRCh38: a variant with no rsID, or whose rsID isn't in the `.bim`, is left out,
#' and a message (class `opengwasr_plink_unmatched`, with `count`) says how many. [og_clump()] and
#' [og_tophits()] need `r2` and `kb`: a PLINK Reference has no recorded defaults. PLINK has no
#' option to rank by |z|, so the variants are ranked by |z| in R and PLINK is given the ranks as
#' p-values (rank / (n + 1), the strongest variant the smallest); it clumps with `--clump-p1 1`
#' and `--clump-p2 1`, so only `r2` and `kb` decide.
#'
#' @param path The fileset's prefix, without `.bed`, `.bim` or `.fam`.
#' @param plink_bin The path of a PLINK 1.9 binary. `NULL` looks for one in the genetics.binaRies
#'   package if it is installed, then on your `PATH`; if there is none, it stops (class
#'   `opengwasr_plink_not_found`) and says how to install it.
#' @return An object of class `og_plink_reference`: `path` (absolute) and `plink_bin`.
#' @examples
#' \dontrun{
#' ref <- og_plink_reference("~/ld/1kg/EUR")
#' og_clump(src, top, ld = ref, r2 = 0.001, kb = 10000)
#' }
#' @export
og_plink_reference <- function(path, plink_bin = NULL) {
  # Kept: client-side, on the user's own files. The API isn't involved.
  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    og_abort("`path` must be the prefix of a PLINK fileset, such as \"~/ld/1kg/EUR\".", class = "opengwasr_argument")
  }
  prefix <- path.expand(path)
  files <- paste0(prefix, c(".bed", ".bim", ".fam"))
  missing <- files[!file.exists(files)]
  if (length(missing) > 0) {
    og_abort(c(paste0("`path` isn't a PLINK fileset: ", paste(basename(missing), collapse = ", "),
                      if (length(missing) == 1) " is" else " are", " missing next to \"", prefix, "\"."),
               i = "Give the prefix of the `.bed`, `.bim` and `.fam` files, without the extension."),
             class = "opengwasr_plink_reference_missing", files = missing)
  }
  structure(list(path = file.path(normalizePath(dirname(prefix)), basename(prefix)), plink_bin = find_plink(plink_bin)),
            class = "og_plink_reference")
}

#' @export
format.og_plink_reference <- function(x, ...) {
  c("<og_plink_reference>", paste0("  ", x$path), paste0("  PLINK: ", x$plink_bin))
}

#' @export
print.og_plink_reference <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

# The PLINK binary: `plink_bin`, else genetics.binaRies' if it is installed, else the one on PATH.
# Kept (here and in plink_run()): PLINK runs on this machine; the API isn't involved.
find_plink <- function(plink_bin, call = rlang::caller_env()) {
  usable <- function(p) is.character(p) && length(p) == 1 && !is.na(p) && nzchar(p) && file.exists(p) &&
    unname(file.access(p, 1)) == 0
  if (!is.null(plink_bin)) {
    if (!usable(plink_bin)) {
      og_abort(paste0("`plink_bin` isn't an executable file: ", format(plink_bin), "."),
               class = c("opengwasr_plink_not_found", "opengwasr_argument"), call = call)
    }
    return(normalizePath(plink_bin))
  }
  # The package is optional, and not on CRAN, so it is looked up by name rather than declared.
  pkg <- "genetics.binaRies"
  if (requireNamespace(pkg, quietly = TRUE)) {
    packaged <- tryCatch(get("get_plink_binary", envir = asNamespace(pkg))(), error = function(e) NULL)
    if (usable(packaged)) return(normalizePath(packaged))
  }
  found <- unname(Sys.which("plink"))
  if (usable(found)) return(normalizePath(found))
  og_abort(c("Can't find PLINK, which a PLINK Reference needs.",
             i = "Install the genetics.binaRies package: `remotes::install_github(\"MRCIEU/genetics.binaRies\")`.",
             i = "Or install PLINK 1.9 (https://www.cog-genomics.org/plink/1.9/) and put it on your `PATH`, or pass its path as `og_plink_reference(path, plink_bin = )`."),
           class = "opengwasr_plink_not_found", call = call)
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
  args <- c("--bfile", ref$path, flags(function(name) file.path(dir, name)), "--out", out)
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
# the first allele in each name (`--keep-allele-order`, as ieugwasr's ld_matrix_local() did).
plink_ld_matrix <- function(ref, rsids, call = rlang::caller_env()) {
  bim <- plink_bim(ref)
  in_file <- which(bim$rsid %in% rsids)  # PLINK writes the matrix in the reference's order
  if (length(in_file) == 0) return(matrix(NA_real_, 0, 0))
  m <- plink_run(ref, list(extract = rsids),
                 function(file) c("--extract", file("extract"), "--r", "square", "--keep-allele-order"),
                 function(out) {
                   # An undefined r (a variant with no variation) is written as "nan".
                   as.matrix(utils::read.table(paste0(out, ".ld"), header = FALSE,
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
  clumped <- plink_run(ref, list(scores = c("SNP P", sprintf("%s %.15g", rsids, p))),
                       function(file) c("--clump", file("scores"), "--clump-p1", 1, "--clump-p2", 1,
                                        "--clump-r2", format(r2, digits = 15), "--clump-kb", format(kb, digits = 15)),
                       function(out) {
                         file <- paste0(out, ".clumped")
                         if (!file.exists(file)) {
                           og_abort("PLINK wrote no clumps. This is a bug in opengwasr.", class = "opengwasr_internal", call = call)
                         }
                         utils::read.table(file, header = TRUE, stringsAsFactors = FALSE,
                                           colClasses = c(SNP = "character", TOTAL = "integer"))
                       }, call)
  # TOTAL counts the others in the clump.
  list(leads = rsids[rsids %in% clumped$SNP], size = clumped$TOTAL[match(rsid, clumped$SNP)] + 1L)
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
  rlang::inform(c(i = paste0("Using PLINK at ", ref$plink_bin, ".")), class = "opengwasr_plink_binary")
}

is_plink_reference <- function(x) inherits(x, "og_plink_reference")

# A PLINK Reference as `ld`: the variants are matched to it by rsID now, so the message and any
# refusal come before PLINK runs. An ALID's rsID is its row's `rsid` when `variants` is a data frame
# of Associations that has one, else the GWAS Stores' variant table says (one lookup by position).
# `kinds` and `chosen` are what ld_prepare() classified and choose_ld_store() decided. Returns what ld_prepare() does, with `plink = list(ref, map, present)`: `map` has one row per
# variant given (`alid`, NA for an rsID; `rsid`, NA for none) and `present` says which the reference holds.
ld_prepare_plink <- function(src, variants, kinds, chosen, deadline, call) {
  ref <- chosen$plink
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
    if (inherits(src, "og_no_source")) {
      source_required(call, paste0(big_number(length(lookup_for)), if (length(lookup_for) == 1) " ALID has" else " ALIDs have",
                                   " no rsID, so its rsID has to be looked up: ", some_ids(lookup_for, 3)))
    }
    found <- plink_rsids_of(src, lookup_for, deadline, call)
    map$rsid[match(lookup_for, map$alid)] <- found$rsid[match(lookup_for, found$alid)]
    jobs <- found$jobs
  }
  map$rsid <- rsid_or_na(tolower(map$rsid))
  present <- !is.na(map$rsid) & map$rsid %in% plink_bim(ref)$rsid
  inform_plink_binary(ref)
  inform_plink_unmatched(nrow(map), sum(!present))
  list(alids = kinds$alids, store = chosen$store, choice = chosen$choice, unresolved = character(),
       jobs = jobs, plink = list(ref = ref, map = map, present = present))
}

# The rsID of each ALID, from the GWAS Stores' variant tables (one lookup by position, in the sole
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

# A query that reads LD Stores only, given a PLINK Reference (as `ld`, or as the Source's LD resource).
refuse_plink <- function(method, src = NULL) {
  og_abort(c(paste0("`", get(".Generic", envir = method), "()` reads LD Stores only, not a PLINK Reference."),
             i = if (is.null(method$ld) && !is.null(src$ld)) {
               "The PLINK Reference is the Source's LD resource: pass an LD Store's name or an ancestry code in `ld` for this call."
             } else {
               "Pass an LD Store's name or an ancestry code in `ld`."
             }),
           class = "opengwasr_argument", call = method)
}

# The plan of a query over a PLINK Reference: no requests. PLINK runs when the plan is finished,
# with what was pinned now (the variants matched).
ld_query_plink <- function(prep, spec, method) {
  plink <- prep$plink
  left_out <- with(plink$map, ifelse(is.na(alid), rsid, alid))[!plink$present]
  ctx <- list(alids = prep$alids, absent = left_out, store = prep$store, choice = prep$choice,
              matched = plink$map[plink$present, , drop = FALSE], plink = plink$ref)
  jobs <- prep$jobs
  list(requests = list(), releases = NULL, finish = function(results) {
    done <- spec$local(ctx)
    list(value = done$value, jobs = jobs,
         provenance = c(list(ld = prep$store$ld_store, ld_choice = prep$choice, absent = left_out,
                             plink_bin = plink$ref$plink_bin), done$provenance))
  })
}

# The same contribution for a PLINK Reference: the Jobs fetch unclumped top hits (the most the API
# gives per Analysis, several Analyses to a Job), and `finish` clumps each Analysis's on this
# machine, then applies the user's `limit` to its index variants (strongest first), flagging the
# Jobs whose Analyses it cut.
plink_clump_contribution <- function(chosen, clump, info, call) {
  ref <- chosen$plink
  inform_plink_binary(ref)
  if (progress_enabled()) {
    rlang::inform(c(i = paste0("Clumping ", length(info$id), if (length(info$id) == 1) " Analysis" else " Analyses",
                               " in PLINK Reference ", ref$path, ": ",
                               clump_setting_text(clump$r2, clump$kb), ".")),
                  class = "opengwasr_clumping")
  }
  most <- max_tophits_per_analysis
  list(
    along = "scope",
    analyses_per_job = tophits_analyses_per_job(most),
    body = function(chunk) list(limit = most),
    truncated = function(ids) {
      truncated_message(analyses_who(ids), most, "top hits", clump$threshold,
                        cut = paste0(", the most the API returns, so clumping saw only the strongest ", big_number(most)),
                        advice = "Raise the threshold's strictness, or clump on the server with an LD Store in `ld`, which clumps every top hit.")
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
           provenance = list(clump = summary, plink_bin = ref$plink_bin),
           # `limit` cut index variants off: provenance says so, on the Jobs of those Analyses.
           truncated = vapply(pieces, function(p) any(info$id[p$scope] %in% too_many), NA))
    }
  )
}
