# Find Analyses

Returns Analysis metadata, one row per identifier given, or the whole
catalogue.

## Usage

``` r
og_analyses(src, ids = NULL, releases = NULL)
```

## Arguments

- src:

  A Source, from
  [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md)
  or
  [`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md).

- ids:

  Analysis identifiers, or `NULL` for the whole catalogue.

- releases:

  Store Releases to use. `NULL` uses the one Store Release each GWAS
  Store serves, as does a GWAS Store id such as `"OGS-00011"`. The API
  doesn't say which Store Release of a GWAS Store is latest, so if a
  GWAS Store serves several, opengwasr stops with
  `opengwasr_release_selection_required` and lists them: pin one as
  `"<store_id>/<release_id>"`. A Local Source holds one Store Release of
  each GWAS Store, so never asks. A Store Release the Source doesn't
  serve or hold raises `opengwasr_release_not_found`.

## Value

A data frame: `requested_id`, `id`, then the API's metadata columns
(`store_id`, `analysis_id`, `analysis_label`, `assigned_ancestry`,
`sample_size`, `release_id`, ...).

## Details

An Analysis is named `<store_id>:<analysis_id>`, such as
`OGS-00011:GCST90029007`. A bare `analysis_id`, or an identifier from
Legacy OpenGWAS, is accepted where it names exactly one Analysis. The
`requested_id` column shows what you asked for and `id` the canonical
id.

- A legacy identifier whose analysis has a later release is answered by
  the successor, with a warning of class `opengwasr_legacy_successor`
  carrying `requested_id` and `id`.

- An identifier that could name more than one Analysis raises
  `opengwasr_ambiguous_analysis`, with the candidates in `candidates`.

- An identifier that names no Analysis, or one that has been retired,
  raises `opengwasr_analysis_not_found` (`opengwasr_analysis_retired`
  too when retired), the classes of the API's own codes. The condition
  carries the identifiers in `ids`, the API's reason for each where it
  gave one in `api_reasons` (`NA` otherwise), and, for a retired
  Analysis, every successor the API named in `superseded_by`.

## Local Source

A Local Source
([`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md))
answers from the `analyses.tsv` of each Store Release it holds, and
names Analyses the same way. It has no catalogue of Legacy OpenGWAS
identifiers: an identifier that isn't `<store_id>:<analysis_id>` and is
no `analysis_id` it holds, in any of its Store Releases, raises
`opengwasr_legacy_id_local` (with the identifiers in `ids`); translate
it once with `og_analyses(og_api(), ids)` and use the `id` that returns.

Its rows have every column of the API Source's, in the same order and of
the same types, from the Store Release's `analyses.tsv`; `outcome_type`
is derived as the catalogue derives it. Columns the catalogue adds are
NA locally (`status`, `listed`, `trait_class`, `ancestry_category`,
`label_from_source`), as is any column the Store Release has no value
for. The other columns of `analyses.tsv` follow, of the types opengwasdb
gives them: the counts (`n_hits_5e8`, `n_hits_5e6`, `n_hits_5e4`,
`completion_n_imputed_total`, `completion_n_missing_total`,
`n_missing_se_out_of_range`, `eaf_orientation_n`, `n_loci_5e8`,
`n_unclumped_5e8`) integer; `completion_median_pearson_r`,
`eaf_orientation_r` and `original_sd_dispersion` double, as is each
`ancestry_prop_<population>` (a proportion, from 0 to 1); `eaf_scope`,
`completed_against` and `eaf_orientation` text. A blank is NA of its
column's type, and a column opengwasr doesn't know is text. A value of
`analyses.tsv` the catalogue would refuse (a sample size that isn't a
whole number, say), or that isn't of its column's type (a count of
`1.5`), raises `opengwasr_invalid_analyses`, naming the Store Release,
the column (`column`) and the values (`values`).

## Examples

``` r
if (FALSE) { # \dontrun{
src <- og_api()
og_analyses(src, c("OGS-00011:GCST90029007", "ukb-b-19953"))
catalogue <- og_analyses(src)
} # }
```
