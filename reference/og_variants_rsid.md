# Look up variants by rsID or position

`og_variants_rsid()` returns the variants the GWAS Stores' own variant
tables hold for `rsids`; `og_variants_chrpos()` those at `positions`.
They are how you find ALIDs (see
[`vignette("opengwasr")`](https://opengwas.github.io/opengwasr/articles/opengwasr.md)).
Each runs as Jobs on the API, split when it is over a Job's limits
(1,000 values, 10 GWAS Stores), and waits for them. A Local Source
([`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md))
answers from the Store Releases it holds, with the same rows.

## Usage

``` r
og_variants_rsid(src, rsids, releases = NULL)

og_variants_chrpos(src, positions, releases = NULL)
```

## Arguments

- src:

  A Source, from
  [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md)
  or
  [`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md).

- rsids:

  rsIDs, such as `"rs1421085"`.

- releases:

  Store Releases to use, as in
  [`og_analyses()`](https://opengwas.github.io/opengwasr/reference/og_analyses.md).

- positions:

  GRCh38 positions as `"chromosome:position"`, such as `"16:53767042"`
  (a `chr` prefix is allowed).

## Value

A data frame with `alid`, `chromosome`, `position`, `effect_allele`,
`other_allele`, `rsid`, `store_id` and `release_id`, with
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md)
attached.

## Details

You get every match: one row per ALID per Store Release that holds it,
so a multiallelic rsID, or a position with several alleles, gives
several rows. A value nothing matches has no row. Rows are in the order
the Jobs returned them. rsIDs are matched in lower case.

## Examples

``` r
if (FALSE) { # \dontrun{
src <- og_api()
og_variants_rsid(src, "rs1421085", releases = "OGS-00011")
og_variants_chrpos(src, "16:53767042", releases = "OGS-00011")
} # }
```
