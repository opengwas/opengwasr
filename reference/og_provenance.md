# Provenance of a query's result

`og_provenance(x)` says how `x` was produced: the call, the Store
Releases that answered, the Job ids, and the counts, including whether
`limit` cut any off (`total`, `truncated`). A Local Source runs no Jobs:
its answers record no Job ids, and no cost estimate.

## Usage

``` r
og_provenance(x)
```

## Arguments

- x:

  A data frame returned by a query function such as
  [`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md).

## Value

A list of class `og_provenance`:

- `query`: the call and its arguments, as text; `args`: the arguments as
  a list.

- `store_releases`: the Store Releases that answered, as
  `<store_id>/<release_id>`.

- `job_ids`: the API's Job ids (none for what a Local Source answered);
  `existing`: whether the API reattached you to a Job it already had.

- `cost_estimate`: the API's cost estimate for the Jobs, summed.

- `total`, `truncated`: how many Associations matched (what the API
  counted), and whether `limit` cut any off; `returned`: the rows of
  `x`.

- `retrieved`: when the answer was read, as a `POSIXct`.

- Clumped results: `clump`, a data frame with one row per Analysis
  clumped (one row, `id` `NA`, for
  [`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md)
  of one set): `id`, `ld`, `ld_choice`, `r2`, `kb`, and the API's counts
  `hits` (the variants clumped), `absent` (those not in the LD Store)
  and `leads` (index variants), and `clumps`: `"computed"` (clumped on
  the fly, as every clumping is today) or, once Store Releases carry
  them, `"stored"`. `ld_from` says where the LD came from: `"source"`
  (the Source's LD resource) or `"ancestry"` (each Analysis's Assigned
  Ancestry, on an API Source with none).

- LD results: `ld`, `ld_choice`, `ld_from` (as above) and `absent` (see
  [`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md)).
  [`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md)
  of Analyses matched to several LD Stores gives each in `ld`, and how
  each was chosen in `ld_choice`, one for one.

A data frame whose provenance was dropped (by
[`merge()`](https://rdrr.io/r/base/merge.html), say) raises an error of
class `opengwasr_no_provenance`; one whose rows changed after
[`rbind()`](https://rdrr.io/r/base/cbind.html) or subsetting keeps a
stale attribute, and gives the `opengwasr_provenance_stale` warning and
`NULL`.

## Details

It is attached to the data frame a query function returns, and describes
exactly those rows. After you
[`rbind()`](https://rdrr.io/r/base/cbind.html) or subset the rows they
are no longer what the query returned: the attribute may survive, but
`og_provenance()` then warns (class `opengwasr_provenance_stale`) and
returns `NULL` rather than give counts that no longer fit. Some
operations, such as [`merge()`](https://rdrr.io/r/base/merge.html), drop
the attribute altogether, and `og_provenance()` then stops with class
`opengwasr_no_provenance`. Keep it, or its counts, before you combine
data frames. Adding columns doesn't matter. Every Association also
carries its own `store_id` and `release_id`.

## Examples

``` r
if (FALSE) { # \dontrun{
top <- og_tophits(src, "OGS-00011:GCST003116", clump = FALSE, limit = 3)
og_provenance(top)$truncated
} # }
```
