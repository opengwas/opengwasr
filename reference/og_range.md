# Associations in a genomic range

Returns every Association of `analyses` between `start` and `end` on
`chromosome`: positions are GRCh38, one-based and inclusive. The query
runs as Jobs on the API, and `og_range()` waits for them (see
[`vignette("large-queries-and-jobs")`](https://opengwas.github.io/opengwasr/articles/large-queries-and-jobs.md)).

## Usage

``` r
og_range(
  src,
  analyses,
  chromosome,
  start,
  end,
  observed_only = FALSE,
  original_units = FALSE,
  releases = NULL
)
```

## Arguments

- src:

  A Source, from
  [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md)
  or
  [`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md).

- analyses:

  Analysis identifiers (see
  [`og_analyses()`](https://opengwas.github.io/opengwasr/reference/og_analyses.md)).

- chromosome:

  One chromosome, such as `"9"` or `"X"`.

- start, end:

  The first and last position, inclusive.

- observed_only:

  Leave out Imputed Associations.

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

One Job takes at most 10 Mb and 10 Analyses (from at most 10 GWAS
Stores). A wider range or more Analyses is split into windows of at most
10 Mb, crossed with Analyses, run a few at a time
(`og_api(max_in_flight = )`), and put back together: you get the rows an
unsplit request would give, ordered by Analysis (in the order asked),
chromosome, position and ALID, with every Job in
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md).
A range with more Associations than the API will return in one Job fails
with `opengwasr_request_too_large`; it is never split to get round that.
The columns are those of
[`og_associations()`](https://opengwas.github.io/opengwasr/reference/og_associations.md).

## Examples

``` r
if (FALSE) { # \dontrun{
src <- og_api()
og_range(src, "OGS-00011:GCST003116", chromosome = "9", start = 22090000, end = 22130000)
} # }
```
