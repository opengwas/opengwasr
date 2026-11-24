# Associations of variants in Analyses

Returns the Associations of `variants` in `analyses`: for example, the
outcome data for your instruments. Variants are rsIDs or ALIDs (see
[`vignette("opengwasr")`](https://opengwas.github.io/opengwasr/articles/opengwasr.md)),
mixed. The query runs as Jobs on the API, and `og_associations()` waits
for them (see
[`vignette("large-queries-and-jobs")`](https://opengwas.github.io/opengwasr/articles/large-queries-and-jobs.md)).

## Usage

``` r
og_associations(
  src,
  variants,
  analyses,
  observed_only = FALSE,
  original_units = FALSE,
  releases = NULL
)
```

## Arguments

- src:

  A Source, from
  [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md).

- variants:

  rsIDs and ALIDs, such as `c("rs1421085", "16:53767042:C:T")`. An ALID
  must be in canonical form (see
  [`og_alid()`](https://opengwas.github.io/opengwasr/reference/og_alid.md));
  anything else is an `opengwasr_argument` error.

- analyses:

  Analysis identifiers (see
  [`og_analyses()`](https://opengwas.github.io/opengwasr/reference/og_analyses.md)).

- observed_only:

  Leave out Imputed Associations (`association_status == "imputed"`).

- original_units:

  Give `beta` and `se` in the trait's original units, as in
  [`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md).

- releases:

  Store Releases to use, as in
  [`og_analyses()`](https://opengwas.github.io/opengwasr/reference/og_analyses.md).

## Value

A data frame with
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md)
attached.

## Details

An rsID is resolved to every ALID it names in the Analyses' GWAS Stores
(a multiallelic site names several), and you get every match; opengwasr
never picks one. An rsID no GWAS Store holds has no Associations: you
get a warning of class `opengwasr_unresolved_variants`, with the number
as `count` and the rsIDs as `ids`. A variant an Analysis doesn't hold
has no row. There is no LD-proxy option (ADR 0004): Reference-Completed
Store Releases already hold an Imputed Association for most variants a
source didn't report.

The columns are those of
[`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md).
Every row has an `alid`; its `rsid` is `NA` for a variant that has no
rsID (the API sends `"."`).

One Job takes at most 1,000 distinct variants (ALIDs, after rsIDs are
resolved), 100 Analyses and Analyses from 10 GWAS Stores. A larger
request is split into several Jobs, run a few at a time
(`og_api(max_in_flight = )`), and put back together: you get the rows an
unsplit request would give, in a stable order (Analyses in the order
asked; then chromosome 1-22, X, Y, MT and other contigs by name; then
position; then ALID, whether one Job or several answered), with every
Job in
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md).
A request the API refuses as too costly (`opengwasr_request_too_large`)
is never split to get round that: see
[`vignette("large-queries-and-jobs")`](https://opengwas.github.io/opengwasr/articles/large-queries-and-jobs.md).

## Examples

``` r
if (FALSE) { # \dontrun{
src <- og_api()
og_associations(src, c("rs1421085", "16:53767042:C:T"), "OGS-00011:GCST003116")
} # }
```
