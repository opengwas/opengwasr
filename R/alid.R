#' Give your own summary statistics ALIDs
#'
#' `og_alid()` adds an `alid` column to a data frame of your own summary statistics, so it can be
#' joined to OpenGWAS data, and orients the alleles to match. An ALID is
#' `chromosome:position:A1:A2` on GRCh38 (see the vignette on ALIDs).
#'
#' * Alleles of one length (SNVs, and substitutions of several bases) are always oriented: they
#'   are sorted, so `A1` comes first.
#' * An indel's ALID puts the GRCh38 reference allele first, which the alleles alone can't say. An
#'   indel gets an ALID only if you declare the reference with `reference`; otherwise its `alid`
#'   is `NA`, and a message gives the count.
#' * Where the effect allele is not `A1`, the row's alleles are swapped: `beta` and `z` are
#'   negated and `eaf` is replaced by 1 - `eaf`. Only columns the data has are adjusted.
#' * Chromosome labels are normalised as the GWAS Stores do (`chr` prefix removed, `23` to `X`,
#'   `M` to `MT`, ...), and alleles are upper-cased.
#' * `data` must not already have an `alid` column, and no two arguments may name one column.
#' * A row that names no variant (a missing chromosome, an allele that is not A, C, G or T, a
#'   position that is not a finite positive whole number, or identical alleles) gets `alid = NA`, and
#'   a message gives the count. Its values are left exactly as given.
#'
#' `og_alid()` assumes two things it can't check without the reference genome: alleles are on the
#' forward strand, and indels are left-aligned, as in a normalised VCF.
#'
#' @param data A data frame.
#' @param chromosome,position,effect_allele,other_allele Names of the columns holding the GRCh38
#'   chromosome and position and the two alleles.
#' @param eaf,beta,z Names of the columns holding the effect allele frequency, effect size and
#'   z-score. Each is adjusted when swapping alleles if the data has it; a name you give that the
#'   data lacks is an error.
#'   Give `NULL` to say the data has no such column. A named column must be numeric. Negating
#'   an integer `beta` or `z` keeps it integer; `1 - eaf` makes an integer `eaf` double.
#' @param reference `NULL` (indels get no ALID), `"other_allele"` or `"effect_allele"` (that column
#'   is always the reference allele), or the name of a column holding each row's reference allele.
#'   Used only for indels.
#' @return `data`, with an `alid` column added. In rows that have an ALID, the chromosome and
#'   alleles are normalised and `beta`, `z` and `eaf` adjusted where alleles were swapped; every
#'   other row, and every other column, is exactly as given. Each message has a class
#'   (`opengwasr_alid_swapped`, `opengwasr_alid_undeclared_reference`, `opengwasr_alid_unnamed`)
#'   and a `count` field.
#' @examples
#' my_gwas <- data.frame(
#'   chromosome    = c("16", "chr18", "5", "1"),
#'   position      = c(53767042, 60161902, 75701728, 827587),
#'   effect_allele = c("T", "c", "AAAAT", "G"),
#'   other_allele  = c("C", "T", "A", "GGGGCC"),
#'   eaf           = c(0.58, 0.24, 0.13, 0.04),
#'   beta          = c(-0.076, 0.055, -0.025, 0.12)
#' )
#' og_alid(my_gwas)
#' og_alid(my_gwas, reference = "other_allele")
#' @export
og_alid <- function(data, chromosome = "chromosome", position = "position",
                    effect_allele = "effect_allele", other_allele = "other_allele",
                    eaf = "eaf", beta = "beta", z = "z", reference = NULL) {
  # Kept: every check here (and in check_column_name(), check_reference()) is of the user's own data,
  # on this machine; the API never sees it.
  if (!is.data.frame(data)) {
    og_abort("`data` must be a data frame.", "opengwasr_argument")
  }
  if ("alid" %in% names(data)) {
    og_abort(c("`data` already has an `alid` column.",
               i = "Rename or drop it, so `og_alid()` doesn't overwrite it."),
             "opengwasr_argument")
  }
  # The four identity columns are required; a statistic is skipped when NULL, or absent at its
  # default name. A statistic column you name that `data` lacks is an error.
  required <- list(chromosome = chromosome, position = position, effect_allele = effect_allele,
                   other_allele = other_allele)
  for (arg in names(required)) check_column_name(data, arg, required[[arg]])
  stats <- list(eaf = eaf, beta = beta, z = z)
  explicit <- c(eaf = !missing(eaf), beta = !missing(beta), z = !missing(z))
  adjust <- character()
  for (arg in names(stats)) {
    name <- stats[[arg]]
    if (is.null(name)) next
    if (!explicit[[arg]] && !name %in% names(data)) next
    check_column_name(data, arg, name)
    if (!is.numeric(data[[name]])) {
      og_abort(sprintf("Column `%s`, named by `%s`, must be numeric.", name, arg),
               "opengwasr_argument")
    }
    adjust[[arg]] <- name
  }
  written <- c(unlist(required), adjust)
  if (anyDuplicated(written)) {
    dup <- written[duplicated(written)][[1]]
    og_abort(c(sprintf("Column `%s` is named by more than one argument.", dup),
               i = "Each argument must name its own column."), "opengwasr_argument")
  }
  ref_mode <- check_reference(data, reference)

  n <- nrow(data)
  labels <- as.character(data[[chromosome]])
  uniq <- unique(labels)
  chrom <- vapply(uniq, alid_chromosome, "", USE.NAMES = FALSE)[match(labels, uniq)]
  eff <- toupper(alid_trim(as.character(data[[effect_allele]])))
  oth <- toupper(alid_trim(as.character(data[[other_allele]])))
  pos <- suppressWarnings(as.numeric(as.character(data[[position]])))

  valid_base <- function(x) !is.na(x) & grepl("^[ACGT]+$", x)
  named <- !is.na(chrom) & valid_base(eff) & valid_base(oth) & eff != oth &
    is.finite(pos) & pos > 0 & pos == round(pos)
  named[is.na(named)] <- FALSE

  ref_allele <- switch(ref_mode,
    none = rep(NA_character_, n),
    other_allele = oth,
    effect_allele = eff,
    column = toupper(alid_trim(as.character(data[[reference]])))
  )
  indel <- named & nchar(eff) != nchar(oth)
  declared <- indel & !is.na(ref_allele) & (ref_allele == eff | ref_allele == oth)
  unplaced_indel <- indel & !declared
  placed <- which(named & !unplaced_indel)

  # `first` is the allele an ALID spells first: the reference of an indel, else the smaller.
  e <- eff[placed]
  o <- oth[placed]
  first <- ifelse(indel[placed], ref_allele[placed], ifelse(e < o, e, o))
  second <- ifelse(first == e, o, e)
  swap <- first != e

  alid <- rep(NA_character_, n)
  alid[placed] <- paste(chrom[placed],
                        format(pos[placed], scientific = FALSE, trim = TRUE, digits = 15),
                        first, second, sep = ":")

  # Only rows with an ALID are changed, by index: other rows and columns are left as given.
  out <- data
  set_rows <- function(col, rows, value) {
    if (is.factor(out[[col]])) out[[col]] <<- as.character(out[[col]])
    out[[col]][rows] <<- value
  }
  set_rows(chromosome, placed, chrom[placed])
  set_rows(effect_allele, placed, first)
  set_rows(other_allele, placed, second)
  swapped <- placed[swap]
  for (arg in names(adjust)) {
    nm <- adjust[[arg]]
    x <- data[[nm]][swapped]
    out[[nm]][swapped] <- if (arg == "eaf") 1 - x else -x
  }
  out$alid <- alid

  n_swapped <- length(swapped)
  if (n_swapped > 0) {
    parts <- c(
      if ("beta" %in% names(adjust)) "`{adjust[['beta']]}` negated",
      if ("z" %in% names(adjust)) "`{adjust[['z']]}` negated",
      if ("eaf" %in% names(adjust)) "`{adjust[['eaf']]}` replaced by 1 - `{adjust[['eaf']]}`")
    detail <- if (length(parts)) paste0(": ", paste(parts, collapse = ", "), ".") else "."
    alid_inform(
      paste0("Swapped the alleles of {n_swapped} variant{?s} to match {?its/their} ALID{?s}",
             detail),
      "opengwasr_alid_swapped", n_swapped, "i")
  }
  n_unplaced <- sum(unplaced_indel)
  if (n_unplaced > 0) {
    why <- if (ref_mode == "none") "no allele is declared the reference. Name it with `reference`"
           else "the declared reference is neither of its alleles"
    alid_inform(
      paste0("{n_unplaced} indel{?s} {?has/have} no ALID, because ", why, "."),
      "opengwasr_alid_undeclared_reference", n_unplaced, "!")
  }
  n_unnamed <- sum(!named)
  if (n_unnamed > 0) {
    alid_inform(
      paste0("{n_unnamed} row{?s} {?has/have} no ALID, because {?it doesn't/they don't} name a ",
             "variant (missing chromosome, alleles other than A, C, G and T, or an invalid ",
             "position)."),
      "opengwasr_alid_unnamed", n_unnamed, "!")
  }
  out
}

# A classed message with a `count` field. Column names are passed to cli as data, so braces in
# them are safe.
alid_inform <- function(text, class, count, bullet) {
  msg <- stats::setNames(text, bullet)
  cli::cli_inform(msg, class = class, count = count, .envir = rlang::caller_env())
}

alid_trim <- function(x) gsub("(*UCP)^[\\s\\x1c-\\x1f]+|[\\s\\x1c-\\x1f]+$", "", x, perl = TRUE)

check_column_name <- function(data, arg, name) {
  if (!is.character(name) || length(name) != 1 || is.na(name)) {
    og_abort(sprintf("`%s` must be the name of one column.", arg), "opengwasr_argument",
             call = rlang::caller_env(2))
  }
  if (!name %in% names(data)) {
    og_abort(c(sprintf("`data` has no column `%s`, named by `%s`.", name, arg),
               i = "Name the column with the argument."),
             "opengwasr_argument", call = rlang::caller_env(2))
  }
}

check_reference <- function(data, reference) {
  if (is.null(reference)) return("none")
  if (!is.character(reference) || length(reference) != 1 || is.na(reference)) {
    og_abort("`reference` must be `NULL`, \"other_allele\", \"effect_allele\" or a column name.",
             "opengwasr_argument", call = rlang::caller_env(2))
  }
  if (reference %in% c("other_allele", "effect_allele")) return(reference)
  if (!reference %in% names(data)) {
    og_abort(c(sprintf("`reference` names nothing: `data` has no column `%s`.", reference),
               i = "Use \"other_allele\", \"effect_allele\" or the name of a column of reference alleles."),
             "opengwasr_argument", call = rlang::caller_env(2))
  }
  "column"
}

# The canonical chromosome label (opengwasdb's `normalise_chromosome`), or NA for a missing one.
alid_chromosome <- function(x) {
  x <- alid_trim(x)
  if (is.na(x)) return(NA_character_)
  if (tolower(substr(x, 1, 3)) == "chr") x <- substr(x, 4, nchar(x))
  up <- toupper(x)
  alias <- c("23" = "X", X = "X", "24" = "Y", Y = "Y", "25" = "MT", "26" = "MT", M = "MT", MT = "MT")
  if (up %in% names(alias)) return(alias[[up]])
  if (x %in% as.character(1:22)) return(x)
  if (up %in% c("", ".", "NA", "NAN", "NONE")) return(NA_character_)
  if (grepl("^[0-9]+$", x)) return(NA_character_)
  x
}
