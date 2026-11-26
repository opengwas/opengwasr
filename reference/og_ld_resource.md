# The LD resource a Source carries

`og_ld_resource()` returns the LD resource a Source carries (ADR 0006;
see
[`vignette("ld")`](https://opengwas.github.io/opengwasr/articles/ld.md),
"The Source's LD resource"): the one every verb that needs LD uses.
Packages built on opengwasr, such as TwoSampleMR, read it to honour the
Source's LD resource.

## Usage

``` r
og_ld_resource(src)
```

## Arguments

- src:

  A Source, from
  [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md)
  or
  [`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md).

## Value

For an API Source, its `ld_store` (an LD Store's name or an ancestry
code), as given to `og_api(ld_store = )`. For a Local Source, its PLINK
Reference, from `og_local(bfile = , plink = )`: an `og_plink_reference`,
a list of `path` (absolute, with forward slashes on every platform),
`plink_bin` (the binary's absolute path) and `plink_version` (as PLINK
reports it, such as `"1.90b6.21"` or `"2.0.0-b.1.4"`). `NULL` when the
Source carries none: the API Source then matches each Analysis's
Assigned Ancestry, and a Local Source refuses anything that needs LD.
Anything that isn't a Source is refused as every query function refuses
it (class `opengwasr_source_required` for `NULL`, else
`opengwasr_argument`).

## Examples

``` r
if (FALSE) { # \dontrun{
og_ld_resource(og_api())                    # NULL
og_ld_resource(og_api(ld_store = "EUR"))    # "EUR"
} # }
```
