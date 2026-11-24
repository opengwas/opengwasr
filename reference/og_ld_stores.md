# List the LD Stores

`og_ld_stores()` lists the LD Stores the API serves, one row each, with
what each holds: see
[`vignette("ld")`](https://opengwas.github.io/opengwasr/articles/ld.md).

## Usage

``` r
og_ld_stores(src)
```

## Arguments

- src:

  A Source, from
  [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md).

## Value

A data frame: `ld_store` (the LD Store's name), `ancestry`, `n` (people
in the reference panel), `t_far` (the Far Threshold), `near_bp`,
`far_bp`, `n_variants`, `default`, which will mark each ancestry's
default LD Store once the API declares one (`NA` until then), and
`ready`. An LD Store that isn't ready can't answer: choosing it is
refused.

## Examples

``` r
if (FALSE) { # \dontrun{
og_ld_stores(og_api())
} # }
```
