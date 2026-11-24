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
  [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md).

- ids:

  Analysis identifiers, or `NULL` for the whole catalogue.

- releases:

  Store Releases to use. `NULL` uses the one Store Release each GWAS
  Store serves, as does a GWAS Store id such as `"OGS-00011"`. The API
  doesn't say which Store Release of a GWAS Store is latest, so if a
  GWAS Store serves several, opengwasr stops with
  `opengwasr_release_selection_required` and lists them: pin one as
  `"<store_id>/<release_id>"`.

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

## Examples

``` r
if (FALSE) { # \dontrun{
src <- og_api()
og_analyses(src, c("OGS-00011:GCST90029007", "ukb-b-19953"))
catalogue <- og_analyses(src)
} # }
```
