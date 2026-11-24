# PheWAS: variants across Analyses

Returns the Associations of `variants` at p \<= `threshold`, across
every Analysis of the Store Releases (or the `analyses` you name). The
query runs as Jobs on the API, and `og_phewas()` waits for them (see
[`vignette("large-queries-and-jobs")`](https://opengwas.github.io/opengwasr/articles/large-queries-and-jobs.md)).

## Usage

``` r
og_phewas(
  src,
  variants,
  threshold = 1e-08,
  analyses = NULL,
  releases = NULL,
  limit = 1000,
  observed_only = FALSE,
  original_units = FALSE
)
```

## Arguments

- src:

  A Source, from
  [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md).

- variants:

  rsIDs and ALIDs, such as `c("rs1421085", "16:53767042:C:T")`; an ALID
  must be in canonical form (see
  [`og_alid()`](https://opengwas.github.io/opengwasr/reference/og_alid.md)).

- threshold:

  The largest p-value to return; `1` returns every Association.

- analyses:

  `NULL` (every Analysis of the Store Releases), or Analysis identifiers
  (see
  [`og_analyses()`](https://opengwas.github.io/opengwasr/reference/og_analyses.md)).

- releases:

  Store Releases to use, as in
  [`og_analyses()`](https://opengwas.github.io/opengwasr/reference/og_analyses.md).

- limit:

  The most Associations to return per ALID and group of up to 10 GWAS
  Stores, at most 10,000.

- observed_only:

  Leave out Imputed Associations.

- original_units:

  Give `beta` and `se` in the trait's original units, as in
  [`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md).

## Value

A data frame with the columns of
[`og_associations()`](https://opengwas.github.io/opengwasr/reference/og_associations.md)
and
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md)
attached.

## Details

`variants` are rsIDs and ALIDs, mixed, as for
[`og_associations()`](https://opengwas.github.io/opengwasr/reference/og_associations.md).
An rsID is resolved to every ALID it names in the Store Releases (a
multiallelic site names several), and opengwasr runs one PheWAS per ALID
and combines them: you get every match, in the order the variants were
given (an rsID's ALIDs in the order the API gave them). An rsID no Store
Release holds gives a warning of class `opengwasr_unresolved_variants`,
as for
[`og_associations()`](https://opengwas.github.io/opengwasr/reference/og_associations.md).
Every Job is in
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md).

With `analyses = NULL` (the default) every Analysis of the Store
Releases is searched. The Analyses' metadata (`beta`'s effect scale,
`n`, `ncase`) is then looked up once, after the rows, for the Analyses
that appear in them. What can be queried is the API's say: an Analysis
in the result that the catalogue doesn't list yet is returned as the API
sent it, with its metadata columns (`effect_scale`, `n`,
`sample_size_kind`, `ncase`, `ncontrol`) `NA`, and no warning.
`original_units = TRUE` can then only be refused (class
`opengwasr_original_units_unavailable`) after the search, naming the
Analyses in the result; name `analyses` and it is refused before any
query runs.

The columns are those of
[`og_associations()`](https://opengwas.github.io/opengwasr/reference/og_associations.md);
rows are in the order the Jobs returned them (one per ALID, split by
Store Release, each in the API's order). One Job takes at most 10 GWAS
Stores and 100 Analyses, and a larger request is split and put back
together as for
[`og_associations()`](https://opengwas.github.io/opengwasr/reference/og_associations.md).

`limit` applies to each ALID (not to all the variants together), and to
each group of up to 10 GWAS Stores searched (the Store Releases are
searched in groups of that size), so you can get more than `limit` rows
in all. When it cuts any off you get a warning of class
`opengwasr_truncated`, and
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md)
has `truncated = TRUE`; `total` is the number of Associations that
passed `threshold`. `observed_only` is always sent; opengwasr never
relies on the API's default.

## Examples

``` r
if (FALSE) { # \dontrun{
src <- og_api()
ph <- og_phewas(src, c("rs1421085", "16:53767042:C:T"), releases = "OGS-00011")
} # }
```
