# Clump Associations you already have

`og_clump()` clumps a data frame of Associations, such as top hits
fetched unclumped from another Source, and returns the rows of the index
variants (see
[`vignette("ld")`](https://opengwas.github.io/opengwasr/articles/ld.md)).
By default it clumps each Analysis's rows on their own, as
[`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
does, so
`og_clump(src, og_tophits(src, ids, clump = FALSE, limit = 10000))`
gives each Analysis the same index variants as `og_tophits(src, ids)`.
`by_analysis = FALSE` clumps all the rows as one set, such as the
instruments of several exposures together, so that their index variants
compete.

## Usage

``` r
og_clump(src, data, r2 = NULL, kb = NULL, by_analysis = TRUE)
```

## Arguments

- src:

  A Source, from
  [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md)
  or
  [`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md),
  which carries the LD resource.

- data:

  A data frame of Associations, with `alid` and `z`, and `id`: the
  Analysis of each row, by which the rows are clumped and the LD Store
  is matched to the Analysis's Assigned Ancestry.

- r2:

  The r² above which a variant joins an index variant's clump; `NULL`
  for the LD Store's `t_far`. Required with a PLINK Reference.

- kb:

  The window, in kilobases; `NULL` for the LD Store's far window.
  Required with a PLINK Reference.

- by_analysis:

  `TRUE` (the default) clumps the rows of each Analysis (`id`) on their
  own; `FALSE` clumps all the rows as one set. Rows with no `id` column
  are one set either way.

## Value

The rows of `data` whose variant is an index variant of their set, in
their order in `data`, with columns `clump_ld`, `clump_ld_choice`,
`clump_r2`, `clump_kb` and `clump_size` (the variants the index variant
stands for, itself included), and
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md)
attached: `ld` and `ld_choice` (each LD Store used, and how they were
chosen), `absent` and `clump`, one row per Analysis clumped (one, with
`id` `NA`, for one set), whose `clumps` is `"computed"`.

## Details

Variants are ranked by \|z\|; a variant listed several times in a set is
ranked by its largest \|z\|, and every Association of an index variant
in that set is returned. Starting from the strongest, each index variant
absorbs every weaker variant within `kb` kilobases whose r² with it is
above `r2` (the rule `plink --clump` uses). Variants the LD Store
doesn't hold can't be clumped: they are left out, with a warning of
class `opengwasr_ld_absent_variants` (one for each LD Store).

`r2` and `kb` default to the LD Store's Far Threshold (`t_far`) and its
far window (`far_bp`, 10,000 kb), which it can always answer exactly. A
pair it can't answer exactly is refused by the API
(`opengwasr_ld_refusal`, with `reason`, `t_far`, `near_bp` and
`far_bp`).

The LD is the Source's LD resource. On the API Source that is its
`ld_store` (`og_api(ld_store = )`), else, for each Analysis, the LD
Store matching its Assigned Ancestry, one Job per Analysis. An
Unassigned Analysis is refused, with class `opengwasr_ld_unmatched`,
before any Job runs; so is `data` with no `id` (one set, with no
ancestry to match), and, with `by_analysis = FALSE`, Analyses whose
Assigned Ancestries differ. For other LD, pass another Source, such as
`og_api(inherit = src, ld_store = "EUR")`.

On a Local Source, PLINK clumps on your machine with its PLINK Reference
(`og_local(bfile = )`), one run per Analysis. Its LD isn't adjusted for
principal components, so the first call in a session to read each PLINK
Reference warns, with class `opengwasr_plink_unadjusted` (see
[`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)).
`r2` and `kb` are required (there are no defaults to take, so without
them it stops, with class `opengwasr_plink_settings_required`). Each row
is matched by rsID: its own, from `data`'s `rsid` column if it has one,
else its ALID's, from the Source's Store Releases' variant table; rows
with no rsID match are left out, with a message (class
`opengwasr_plink_unmatched`, `count`, `total`: the distinct ALID and
rsID pairs). An LD-only Local Source (`og_local(bfile = )` with no
`gwas`) has nothing to look an ALID up in, so `data` without an `rsid`
column is refused (class `opengwasr_gwas_store_required`, with the ALIDs
in `alids`, and the ways round in `forms`: see
[`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)).
A Local Source with no LD resource is refused
(`opengwasr_ld_resource_required`). Provenance's `clump` names the
reference's path as `ld`, and `clump_ld` is the same path.

## Examples

``` r
if (FALSE) { # \dontrun{
src <- og_api()
ids <- c("OGS-00011:GCST90029007", "OGS-00011:GCST90002412")
top <- og_tophits(src, ids, clump = FALSE, limit = 10000)
each <- og_clump(src, top)                        # each Analysis on its own
joint <- og_clump(src, top, by_analysis = FALSE)  # all as one set
table(joint$id)
} # }
```
