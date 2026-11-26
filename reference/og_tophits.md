# Top hits of Analyses

Returns each Analysis's Associations at or below a p-value `threshold`,
strongest first, up to `limit` per Analysis. The query runs as a Job on
the API, and `og_tophits()` waits for it (see
[`vignette("large-queries-and-jobs")`](https://opengwas.github.io/opengwasr/articles/large-queries-and-jobs.md)).

## Usage

``` r
og_tophits(
  src,
  analyses,
  threshold = 5e-08,
  limit = 1000,
  observed_only = FALSE,
  clump = TRUE,
  r2 = NULL,
  kb = NULL,
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

- threshold:

  The largest p-value to return. Each GWAS Store indexes `5e-8`, `5e-6`
  and `5e-4` and refuses a looser one
  (`opengwasr_unsupported_threshold`).

- limit:

  The most top hits to return per Analysis, at most 10,000.

- observed_only:

  Leave out Imputed Associations.

- clump:

  Clump the top hits to one per independent signal.

- r2, kb:

  The r² above which a top hit joins an index variant's clump, and the
  window in kilobases; `NULL` for the LD Store's `t_far` and far window,
  but required with a PLINK Reference. Only with `clump = TRUE`.

- original_units:

  Give `beta` and `se` in the trait's original units: Analyses stored
  per SD are multiplied by their recorded phenotype SD (`original_sd`),
  and log-odds Analyses are unchanged. It refuses, with class
  `opengwasr_original_units_unavailable` and before any query runs, when
  an Analysis is stored per SD and isn't a continuous trait with an
  `original_sd`; `ids` and `reason` on the error say which and why.

- releases:

  Store Releases to use, as in
  [`og_analyses()`](https://opengwas.github.io/opengwasr/reference/og_analyses.md).

## Value

A data frame with the columns above and
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md)
attached.

## Details

By default (`clump = TRUE`) the top hits are clumped on the API, one
index variant per independent signal (see
[`vignette("ld")`](https://opengwas.github.io/opengwasr/articles/ld.md)):
every top hit at `threshold` is clumped, and `limit` then caps the index
variants. Each Analysis is clumped in the Source's LD Store
(`og_api(ld_store = )`), or, if it has none, in the LD Store matching
its Assigned Ancestry; an Unassigned Analysis is then refused
(`opengwasr_ld_unmatched`), before any Job runs. The Jobs, one per
Analysis, are submitted grouped by LD Store; with progress on (option
`opengwasr.progress`), a message of class `opengwasr_clumping` says
which LD Store each Analysis is clumped in. `r2` and `kb` default to the
LD Store's Far Threshold (`t_far`) and its far window (`far_bp`, 10,000
kb), and are always sent. A pair the LD Store can't answer exactly is
refused by the API (`opengwasr_ld_refusal`). Top hits the LD Store
doesn't hold can't be clumped: they are left out, with a warning of
class `opengwasr_clump_absent` (`count`). Clumped rows add `clump_ld`,
`clump_ld_choice`, `clump_r2`, `clump_kb` and `clump_size`, and
`og_provenance(x)$clump` has each Analysis's `hits`, `absent` and
`leads`. Pass `clump = FALSE` for every top hit.

The LD is the Source's LD resource (`og_api(ld_store = )`,
`og_local(bfile = )`), and nothing else: for other LD, pass another
Source, such as `og_api(inherit = src, ld_store = "EUR")`. A Local
Source with none is refused (`opengwasr_ld_resource_required`), naming
the LD resources and `clump = FALSE`: Store Releases have no stored
Clumps yet, so clumping is computed on the fly, and
`og_provenance(x)$clump$clumps` is `"computed"`. An LD-only Local Source
(`og_local(bfile = )` with no `gwas`) holds no Associations, so it is
refused (`opengwasr_gwas_store_required`).

With a PLINK Reference as a Local Source's LD resource
(`og_local(gwas = , bfile = )`) the clumping is local: each Analysis's
top hits are fetched unclumped (up to the API's most, 10,000), matched
to the reference by rsID (those not in it are left out, with a message)
and clumped by PLINK (whose LD isn't adjusted for principal components:
a warning of class `opengwasr_plink_unadjusted` says so, once per PLINK
Reference in a session), and `limit` is then applied to its index
variants. If the API cut an Analysis's top hits off at 10,000, you get
an `opengwasr_truncated` warning that clumping saw only the strongest;
clumping on the server (an API Source's LD Store) has no such cap.
`og_provenance(x)$clump` names the reference's path and the `r2` and
`kb`.

The columns are those the vignette section "What the columns mean"
lists: the Analysis `id`, the variant, `beta` (`z * se`), `se`, `z`,
`pval`, `effect_scale` (`"sd"` or `"log_or"` from the Analysis's stored
effect scale, or `"original"` with `original_units = TRUE`), `n`,
`sample_size_kind`, `ncase` and `ncontrol`, `association_status` and the
`store_id` and `release_id` each Association came from. `n` is the
Association's own sample size where the API sends one, otherwise the
Analysis's `sample_size`; the other three describe the Analysis. `pval`
underflows to 0 when `|z|` is above about 38.5; `z` stays exact, so rank
the strongest signals by `abs(z)`.

One Job takes at most 100 Analyses from 10 GWAS Stores, and at most
100,000 rows (`limit` times its Analyses). A larger request is split
into several Jobs, run a few at a time (`og_api(max_in_flight = )`) and
put back together: you get the rows an unsplit request would give, with
every Job in
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md).
The rows are ordered by Analysis, in the order you asked for, and within
each Analysis in the API's order (strongest first), whether one Job or
several answered. `limit` applies to each Analysis, so splitting never
changes it. A request the API refuses as too costly
(`opengwasr_request_too_large`) is never split to get round that.

When `limit` cuts any Analysis's top hits off, you get a warning of
class `opengwasr_truncated`, and
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md)
has `truncated = TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
src <- og_api()
top <- og_tophits(src, "OGS-00011:GCST003116", clump = FALSE, limit = 10)
og_provenance(top)
} # }
```
