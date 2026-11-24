# A PLINK Reference: your own genotypes for local LD

`og_plink_reference()` names a PLINK binary fileset (`.bed`, `.bim`,
`.fam`) on your machine, and the PLINK that will read it, to pass as
`ld` to
[`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md),
[`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md)
and
[`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md).
See
[`vignette("ld")`](https://opengwas.github.io/opengwasr/articles/ld.md).

## Usage

``` r
og_plink_reference(path, plink_bin = NULL)
```

## Arguments

- path:

  The fileset's prefix, without `.bed`, `.bim` or `.fam`.

- plink_bin:

  The path of a PLINK 1.9 binary. `NULL` looks for one in the
  genetics.binaRies package if it is installed, then on your `PATH`; if
  there is none, it stops (class `opengwasr_plink_not_found`) and says
  how to install it.

## Value

An object of class `og_plink_reference`: `path` (absolute) and
`plink_bin`.

## Details

Variants are matched to the reference by rsID, because PLINK References
are usually on GRCh37 while ALIDs are on GRCh38: a variant with no rsID,
or whose rsID isn't in the `.bim`, is left out, and a message (class
`opengwasr_plink_unmatched`, with `count`) says how many.
[`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md)
and
[`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
need `r2` and `kb`: a PLINK Reference has no recorded defaults. PLINK
has no option to rank by \|z\|, so the variants are ranked by \|z\| in R
and PLINK is given the ranks as p-values (rank / (n + 1), the strongest
variant the smallest); it clumps with `--clump-p1 1` and `--clump-p2 1`,
so only `r2` and `kb` decide.

## Examples

``` r
if (FALSE) { # \dontrun{
ref <- og_plink_reference("~/ld/1kg/EUR")
og_clump(src, top, ld = ref, r2 = 0.001, kb = 10000)
} # }
```
