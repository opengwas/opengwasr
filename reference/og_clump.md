# Clump Associations you already have

`og_clump()` clumps a data frame of Associations as one set, such as the
top hits of several Analyses together, and returns the rows of the index
variants (see
[`vignette("ld")`](https://opengwas.github.io/opengwasr/articles/ld.md)).

## Usage

``` r
og_clump(src, data, ld = NULL, r2 = NULL, kb = NULL)
```

## Arguments

- src:

  A Source, from
  [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md).
  `NULL` works with a PLINK Reference as `ld` when nothing is looked up:
  `data` has an `rsid` column, so no ALID needs its rsID from the API. A
  lookup that is needed without a Source stops, with class
  `opengwasr_source_required`.

- data:

  A data frame of Associations, with `alid` and `z`, and `id` for the LD
  Store to be matched by the Analyses' Assigned Ancestry.

- ld:

  An LD Store's name, an ancestry code (its LD Store), a PLINK Reference
  from
  [`og_plink_reference()`](https://opengwas.github.io/opengwasr/reference/og_plink_reference.md)
  (clumped locally by PLINK), or `NULL` to match the Assigned Ancestry
  of the Analyses in `data` (refused, with class
  `opengwasr_ld_unmatched`, when they differ, when one is Unassigned, or
  when `data` has no `id`).

- r2:

  The r² above which a variant joins an index variant's clump; `NULL`
  for the LD Store's `t_far`. Required with a PLINK Reference.

- kb:

  The window, in kilobases; `NULL` for the LD Store's far window.
  Required with a PLINK Reference.

## Value

The rows of `data` whose variant is an index variant, in their order in
`data`, with columns `clump_ld`, `clump_ld_choice`, `clump_r2`,
`clump_kb` and `clump_size` (the variants the index variant stands for,
itself included), and
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md)
attached: `ld`, `ld_choice`, `absent` and `clump`.

## Details

Variants are ranked by \|z\|; a variant listed several times (in several
Analyses) is ranked by its largest \|z\|, and every Association of an
index variant is returned. Starting from the strongest, each index
variant absorbs every weaker variant within `kb` kilobases whose r² with
it is above `r2` (the rule `plink --clump` uses). Variants the LD Store
doesn't hold can't be clumped: they are left out, with a warning of
class `opengwasr_ld_absent_variants`.

`r2` and `kb` default to the LD Store's Far Threshold (`t_far`) and its
far window (`far_bp`, 10,000 kb), which it can always answer exactly. A
pair it can't answer exactly is refused by the API
(`opengwasr_ld_refusal`, with `reason`, `t_far`, `near_bp` and
`far_bp`).

With a PLINK Reference as `ld`, PLINK clumps on your machine (see
[`og_plink_reference()`](https://opengwas.github.io/opengwasr/reference/og_plink_reference.md)):
`r2` and `kb` are required (there are no defaults to take, so without
them it stops, with class `opengwasr_plink_settings_required`). Variants
are matched by rsID, from `data`'s `rsid` column if it has one, else the
GWAS Stores' variant table; those with no rsID match are left out, with
a message (class `opengwasr_plink_unmatched`, `count`). Provenance's
`clump` names the reference's path as `ld`, and `clump_ld` is the same
path.

## Examples

``` r
if (FALSE) { # \dontrun{
src <- og_api()
top <- og_tophits(src, c("OGS-00011:GCST90029007", "OGS-00011:GCST90002412"), clump = FALSE)
joint <- og_clump(src, top)
table(joint$id)
} # }
```
