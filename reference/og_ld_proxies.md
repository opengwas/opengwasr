# LD proxies

`og_ld_proxies()` finds the variants in LD with each of `variants`:
every variant within `kb` kilobases whose r² with it is at least `r2`,
strongest first. It is an LD lookup, for questions about LD itself. It
never substitutes one variant's Associations for another's (see
[`vignette("ld")`](https://opengwas.github.io/opengwasr/articles/ld.md)).

## Usage

``` r
og_ld_proxies(src, variants, r2 = 0.8, kb = 10000)
```

## Arguments

- src:

  A Source, from
  [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md).

- variants:

  ALIDs and rsIDs, or a data frame of Associations.

- r2:

  The smallest r² of a proxy.

- kb:

  The window, in kilobases either side of the variant.

## Value

A data frame with
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md)
attached (`ld`, `ld_choice`, `absent`), one row per proxy: `alid`,
`proxy_alid`, `r` (signed, for the first alleles of both), `r2` and
`distance` (base pairs). Ordered by `alid` as asked, then by r²
(strongest first).

## Details

`variants` are as for
[`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md),
and the LD Store is chosen the same way: the Source's `ld_store`
(`og_api(ld_store = )`), else matched to the Assigned Ancestry of the
Associations' Analyses. A Local Source is refused: it reads no LD
Stores. Variants the LD Store doesn't hold have no proxies, and give a
warning of class `opengwasr_ld_absent_variants`. The API refuses an `r2`
and `kb` the LD Store can't answer exactly (`opengwasr_ld_refusal`, with
`reason`, `t_far`, `near_bp` and `far_bp`). One Job takes at most 1,000
variants; more are split.

## Examples

``` r
if (FALSE) { # \dontrun{
src <- og_api(ld_store = "EUR")
px <- og_ld_proxies(src, "16:53767042:C:T")
head(px)
} # }
```
