# Give your own summary statistics ALIDs

`og_alid()` adds an `alid` column to a data frame of your own summary
statistics, so it can be joined to OpenGWAS data, and orients the
alleles to match. An ALID is `chromosome:position:A1:A2` on GRCh38 (see
the vignette on ALIDs).

## Usage

``` r
og_alid(
  data,
  chromosome = "chromosome",
  position = "position",
  effect_allele = "effect_allele",
  other_allele = "other_allele",
  eaf = "eaf",
  beta = "beta",
  z = "z",
  reference = NULL
)
```

## Arguments

- data:

  A data frame.

- chromosome, position, effect_allele, other_allele:

  Names of the columns holding the GRCh38 chromosome and position and
  the two alleles.

- eaf, beta, z:

  Names of the columns holding the effect allele frequency, effect size
  and z-score. Each is adjusted when swapping alleles if the data has
  it; a name you give that the data lacks is an error. Give `NULL` to
  say the data has no such column. A named column must be numeric.
  Negating an integer `beta` or `z` keeps it integer; `1 - eaf` makes an
  integer `eaf` double.

- reference:

  `NULL` (indels get no ALID), `"other_allele"` or `"effect_allele"`
  (that column is always the reference allele), or the name of a column
  holding each row's reference allele. Used only for indels.

## Value

`data`, with an `alid` column added. In rows that have an ALID, the
chromosome and alleles are normalised and `beta`, `z` and `eaf` adjusted
where alleles were swapped; every other row, and every other column, is
exactly as given. Each message has a class (`opengwasr_alid_swapped`,
`opengwasr_alid_undeclared_reference`, `opengwasr_alid_unnamed`) and a
`count` field.

## Details

- Alleles of one length (SNVs, and substitutions of several bases) are
  always oriented: they are sorted, so `A1` comes first.

- An indel's ALID puts the GRCh38 reference allele first, which the
  alleles alone can't say. An indel gets an ALID only if you declare the
  reference with `reference`; otherwise its `alid` is `NA`, and a
  message gives the count.

- Where the effect allele is not `A1`, the row's alleles are swapped:
  `beta` and `z` are negated and `eaf` is replaced by 1 - `eaf`. Only
  columns the data has are adjusted.

- Chromosome labels are normalised as the GWAS Stores do (`chr` prefix
  removed, `23` to `X`, `M` to `MT`, ...), and alleles are upper-cased.

- `data` must not already have an `alid` column, and no two arguments
  may name one column.

- A row that names no variant (a missing chromosome, an allele that is
  not A, C, G or T, a position that is not a finite positive whole
  number, or identical alleles) gets `alid = NA`, and a message gives
  the count. Its values are left exactly as given.

`og_alid()` assumes two things it can't check without the reference
genome: alleles are on the forward strand, and indels are left-aligned,
as in a normalised VCF.

## Examples

``` r
my_gwas <- data.frame(
  chromosome    = c("16", "chr18", "5", "1"),
  position      = c(53767042, 60161902, 75701728, 827587),
  effect_allele = c("T", "c", "AAAAT", "G"),
  other_allele  = c("C", "T", "A", "GGGGCC"),
  eaf           = c(0.58, 0.24, 0.13, 0.04),
  beta          = c(-0.076, 0.055, -0.025, 0.12)
)
og_alid(my_gwas)
#> ℹ Swapped the alleles of 1 variant to match its ALID: `beta` negated, `eaf`
#>   replaced by 1 - `eaf`.
#> ! 2 indels have no ALID, because no allele is declared the reference. Name it
#>   with `reference`.
#>   chromosome position effect_allele other_allele  eaf   beta            alid
#> 1         16 53767042             C            T 0.42  0.076 16:53767042:C:T
#> 2         18 60161902             C            T 0.24  0.055 18:60161902:C:T
#> 3          5 75701728         AAAAT            A 0.13 -0.025            <NA>
#> 4          1   827587             G       GGGGCC 0.04  0.120            <NA>
og_alid(my_gwas, reference = "other_allele")
#> ℹ Swapped the alleles of 3 variants to match their ALIDs: `beta` negated, `eaf`
#>   replaced by 1 - `eaf`.
#>   chromosome position effect_allele other_allele  eaf   beta               alid
#> 1         16 53767042             C            T 0.42  0.076    16:53767042:C:T
#> 2         18 60161902             C            T 0.24  0.055    18:60161902:C:T
#> 3          5 75701728             A        AAAAT 0.87  0.025 5:75701728:A:AAAAT
#> 4          1   827587        GGGGCC            G 0.96 -0.120  1:827587:GGGGCC:G
```
