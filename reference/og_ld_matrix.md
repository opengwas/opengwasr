# LD matrices

`og_ld_matrix()` returns the signed LD correlation r between every pair
of `variants`, read from an LD Store. See
[`vignette("ld")`](https://opengwas.github.io/opengwasr/articles/ld.md).

## Usage

``` r
og_ld_matrix(src, variants, fill_below_t_far = FALSE)
```

## Arguments

- src:

  A Source, from
  [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md)
  or
  [`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md),
  which carries the LD resource.

- variants:

  ALIDs and rsIDs, or a data frame of Associations.

- fill_below_t_far:

  Set pairs below the Far Threshold to 0, as above.

## Value

A numeric matrix (class `og_ld_matrix`, which is also a `matrix`) with
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md)
attached: `ld` (the LD Store that answered), `ld_choice` (`"user"`,
`"ancestry_default"` or `"matched"`) and `absent`.

## Details

The matrix is named by ALID, on both dimensions; the sign of r refers to
the first allele in each name. `variants` are ALIDs or rsIDs (an rsID
stands for every ALID it names), or a data frame of Associations, whose
`alid` column names the variants and whose Analyses' Assigned Ancestry
chooses the LD Store when the Source has no LD resource. A variant the
LD Store doesn't hold is left out of the matrix, with a warning of class
`opengwasr_ld_absent_variants` (`count`, `ids`); they are in
`og_provenance(m)$absent`.

An LD Store doesn't hold every pair: a pair it doesn't hold is `NA`. One
Job takes at most 500 variants; more are split into several Jobs and put
back together, so the matrix covers every pair.

With `fill_below_t_far = TRUE`, a pair of variants on one chromosome,
within the LD Store's `far_bp` of each other, that it doesn't hold (so
its r² is below the LD Store's Far Threshold) is set to 0. Pairs further
apart, or on different chromosomes, stay `NA`.

The LD is the Source's LD resource: on the API Source its `ld_store`
(`og_api(ld_store = )`), else the LD Store matching the Assigned
Ancestry of the Associations' Analyses (refused for Unassigned or mixed
ancestries, and for variants given without Analyses). For other LD, pass
another Source, such as `og_api(inherit = src, ld_store = "EUR")`.

On a Local Source, PLINK computes the matrix on your machine from its
PLINK Reference (`og_local(bfile = )`). Its LD isn't adjusted for
principal components, so the first call in a session to read each PLINK
Reference warns, with class `opengwasr_plink_unadjusted` (see
[`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)).
Variants are matched to the reference by rsID (an ALID by its `rsid`
column, if `variants` is a data frame that has one, else by the Source's
Store Releases' variant table); those with no rsID, or not in the
reference, are left out, with a message (class
`opengwasr_plink_unmatched`, `count`). An LD-only Local Source
(`og_local(bfile = )` with no `gwas`) has nothing to look an ALID up in,
so ALIDs without an `rsid` column are refused (class
`opengwasr_gwas_store_required`, with the ALIDs in `alids`, and the ways
round in `forms`: see
[`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)).
A Local Source with no LD resource is refused
(`opengwasr_ld_resource_required`). The matrix holds every pair, in the
order of `variants`, and is named `<rsid>_<allele>_<allele>` with the
reference's alleles: the sign of r refers to the first one.
`fill_below_t_far` doesn't apply. Provenance's `ld` is the reference's
path.

A request the LD Store can't answer is refused by the API
(`opengwasr_ld_refusal`, with `reason`, `t_far`, `near_bp` and
`far_bp`); a choice of LD Store that can't be made without guessing is
refused by opengwasr (`opengwasr_ld_unmatched`, with the LD Stores in
`ld_stores`).

## Examples

``` r
if (FALSE) { # \dontrun{
src <- og_api(ld_store = "EUR")
m <- og_ld_matrix(src, c("16:53772541:A:G", "16:53767042:C:T"))
og_provenance(m)$ld
} # }
```
