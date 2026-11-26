# Is a variant in the LD Store?

`og_ld_reflookup()` says whether each of `variants` is held by an LD
Store, so you can check instruments before clumping or building an LD
matrix. See
[`vignette("ld")`](https://opengwas.github.io/opengwasr/articles/ld.md).

## Usage

``` r
og_ld_reflookup(src, variants)
```

## Arguments

- src:

  A Source, from
  [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md).

- variants:

  ALIDs and rsIDs, or a data frame of Associations.

## Value

A data frame with
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md)
attached (`ld`, `ld_choice`, `absent`), one row per variant asked, in
the order asked: `alid`, `rsid` (`NA` where not known: an ALID given
without an `rsid` column) and `present`.

## Details

`variants` and the LD Store are as for
[`og_ld_proxies()`](https://opengwas.github.io/opengwasr/reference/og_ld_proxies.md);
a Local Source is refused. An rsID stands for every ALID it names, one
row each; an rsID no GWAS Store holds has one row with `alid` `NA` and
`present` `FALSE`. A variant the LD Store doesn't hold is a row, not a
warning. One Job takes at most 10,000 variants; more are split.

## Examples

``` r
if (FALSE) { # \dontrun{
src <- og_api(ld_store = "EUR")
og_ld_reflookup(src, c("16:53767042:C:T", "rs1421085"))
} # }
```
