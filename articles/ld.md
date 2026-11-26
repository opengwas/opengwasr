# LD operations

Linkage disequilibrium (LD) is how strongly two variants are inherited
together, measured by their correlation r. Every Source carries an **LD
resource**, which answers LD for it: an **LD Store** on the OpenGWAS
API, or your own **PLINK Reference**. This vignette covers each LD
operation: choosing the LD, LD matrices, LD proxies, reflookup and
clumping.

Variants are named by ALID, as everywhere in opengwasr (see
[ALIDs](https://opengwas.github.io/opengwasr/articles/opengwasr.html#alids)).
The sign of r refers to the first allele in each ALID.

> **Status.** Every chunk runs, replayed from responses recorded against
> the development Data API on 23 November 2026 (`ld-resource-api` and
> `ld-resource-override` replay the same requests as `choosing-ld`,
> `unassigned-ld` and `ld-matrices`; `plink-api` and `plink-alid-rsids`
> replay requests recorded for [Getting
> started](https://opengwas.github.io/opengwasr/articles/opengwasr.md),
> copied here), except these:
>
> - `stored-clumps`: no Store Release has stored Clumps yet; reading
>   them comes in a later step (#39). Its output is illustrative.
> - `plink-reference`, `plink-api`, `plink-ld-only` and
>   `plink-alid-rsids`: they read the small synthetic PLINK Reference
>   that comes with opengwasr, and need a PLINK binary. They run
>   wherever PLINK 1.9 or 2 is found (`plink_ready`: genetics.binaRies,
>   or `plink` or `plink2` on the `PATH`), as in CI; without one, as on
>   CRAN, their code is shown unevaluated.
>
> *Developer note:* The API responses in this vignette were recorded
> against the development Data API, directly, with a developer key (see
> `vignettes/recordings.R`). Once the OpenGWAS Gateway is live (#18),
> re-record through it, so the vignettes show the access users will
> have.

``` r

library(opengwasr)
src <- og_api()
```

## LD Stores

An **LD Store** holds the LD of one reference panel of one ancestry, on
GRCh38.
[`og_ld_stores()`](https://opengwas.github.io/opengwasr/reference/og_ld_stores.md)
lists them:

``` r

og_ld_stores(src)
```

    ##              ld_store ancestry   n      t_far near_bp   far_bp n_variants
    ## 1 hgdp1kgp-v2-EUR-k10      EUR 665 0.03186544 3000000 10000000    8586889
    ## 2 hgdp1kgp-v2-AFR-k10      AFR 608 0.03490787 3000000 10000000   14374341
    ## 3 hgdp1kgp-v2-EAS-k10      EAS 724 0.02922861 3000000 10000000    7837854
    ## 4 hgdp1kgp-v2-SAS-k10      SAS 672 0.03152799 3000000 10000000    8879035
    ## 5 hgdp1kgp-v2-AMR-k10      AMR 341 0.06315152 3000000 10000000    7561067
    ##   default ready
    ## 1      NA  TRUE
    ## 2      NA  TRUE
    ## 3      NA  TRUE
    ## 4      NA  TRUE
    ## 5      NA  TRUE

- `n` is the number of people whose genotypes the LD Store was built
  from (HGDP + 1000 Genomes).
- An LD Store doesn’t hold every pair of variants. Within `near_bp` (3
  Mb) of each other, it holds every pair’s r. From `near_bp` to `far_bp`
  (10 Mb), it holds r only for pairs whose r² reaches its **Far
  Threshold**, `t_far`; for the rest it knows only that r² is below
  `t_far`. Beyond `far_bp`, and across chromosomes, it holds nothing.
- A variant the LD Store doesn’t cover is **absent** from it. Every LD
  operation tells you how many variants were absent.
- `default` will mark each ancestry’s default LD Store once the API
  declares one. Until then it is `NA`.

## Choosing the LD

### The Source’s LD resource

A Source says where Associations come from, and which **LD resource**
answers LD for them. The LD is chosen on the Source, and only there: no
verb takes an LD argument. An LD resource is one of three kinds, and
each can be on the API or on your machine:

| LD resource | On the API | On your machine |
|----|----|----|
| **LD Store**: the precomputed LD of one reference panel | `og_api(ld_store = "EUR")`, an LD Store’s name or an ancestry code. Available now | `og_local(gwas = ..., ld_store = path)`. Coming, when opengwasr can read downloaded LD Stores |
| **Genotype Store**: genotypes of one reference panel, from which LD is computed when asked | `og_api(geno_store = ...)`. Coming, when the API serves Genotype Stores | `og_local(gwas = ..., geno_store = path)`. Coming, when opengwasr can read downloaded Genotype Stores |
| **PLINK Reference**: your own PLINK binary fileset (`.bed`, `.bim`, `.fam`) | None: the API doesn’t read your files | `og_local(gwas = ..., bfile = prefix)`, or `og_local(bfile = prefix)` alone for LD only, and `plink =` the PLINK 1.9 or 2 binary if opengwasr can’t find it. Available now |
| None given | [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md): each Analysis’s Assigned Ancestry chooses its LD Store | `og_local(gwas = ...)`: anything that needs LD stops and asks you to give an LD resource |

LD from a PLINK Reference is computed from its genotypes without
adjusting for population structure (principal components), unlike
OpenGWAS LD Stores and Genotype Stores, so it may be less accurate.
opengwasr warns of this once per PLINK Reference in a session (class
`opengwasr_plink_unadjusted`).

A form that is coming is refused with an error that says so (class
`opengwasr_ld_resource_unavailable`), rather than quietly doing
something else. `og_api(ld_store = )` takes an LD Store’s name, such as
`"hgdp1kgp-v2-EUR-k10"`, or an ancestry code, such as `"EUR"`, for that
ancestry’s LD Store (its declared default, if it has several). An exact
LD Store name wins; otherwise it is read as an ancestry code. LD Store
names and ancestry codes (`EUR`, `AFR`, `EAS`, `SAS`, `AMR`) can’t be
confused today, and the provenance always records which LD Store was
used: a name gives `ld_choice = "user"` (`clump_ld_choice` when
clumping), an ancestry code `"ancestry_default"`.

Every verb that needs LD uses the Source’s LD resource:
[`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
when it clumps,
[`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md),
[`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md),
[`og_ld_proxies()`](https://opengwas.github.io/opengwasr/reference/og_ld_proxies.md)
and
[`og_ld_reflookup()`](https://opengwas.github.io/opengwasr/reference/og_ld_reflookup.md).
Here the Source’s LD resource is the EUR LD Store, and the top hits are
clumped there:

``` r

src_eur <- og_api(ld_store = "EUR")
src_eur
```

    ## <og_api_source>
    ## • URL: http://localhost:6433
    ## • Signed in with: OPENGWAS_DEV_KEY (as opengwasr-dev)
    ## • Timeout: 3600 s
    ## • Jobs in flight: at most 4
    ## • LD resource: LD Store "EUR"

``` r

cad_eur <- og_tophits(src_eur, "OGS-00011:GCST003116")
```

    ## Warning in og_tophits(src_eur, "OGS-00011:GCST003116"): 234 of the 2,194 top
    ## hits of OGS-00011:GCST003116 aren't in LD Store hgdp1kgp-v2-EUR-k10, so they
    ## can't be clumped and were left out.

``` r

unique(cad_eur[, c("id", "clump_ld", "clump_ld_choice", "clump_r2", "clump_kb")])
```

    ##                     id            clump_ld  clump_ld_choice   clump_r2 clump_kb
    ## 1 OGS-00011:GCST003116 hgdp1kgp-v2-EUR-k10 ancestry_default 0.03186544    10000

An LD Store named by its name:

``` r

cad_eur <- og_tophits(og_api(ld_store = "hgdp1kgp-v2-EUR-k10"), "OGS-00011:GCST003116")
```

    ## Warning in og_tophits(og_api(ld_store = "hgdp1kgp-v2-EUR-k10"),
    ## "OGS-00011:GCST003116"): 234 of the 2,194 top hits of OGS-00011:GCST003116
    ## aren't in LD Store hgdp1kgp-v2-EUR-k10, so they can't be clumped and were left
    ## out.

``` r

unique(cad_eur$clump_ld_choice)
```

    ## [1] "user"

[`og_ld_resource()`](https://opengwas.github.io/opengwasr/reference/og_ld_resource.md)
returns the LD resource a Source carries (`NULL` when it carries none).
Packages built on opengwasr, such as TwoSampleMR, use it to honour the
Source’s LD resource:

``` r

og_ld_resource(src_eur)
```

    ## [1] "EUR"

``` r

og_ld_resource(og_api())
```

    ## NULL

### A different LD for one call is a different Source

To use other LD for one call, make a Source like the one you have, with
other LD. `inherit =` does it in one line: every argument you give
replaces the inherited one, and everything else (the URL, key, timeout
and `max_in_flight` of an API Source; the Store Releases of a Local
Source) comes from `inherit`. Giving any LD argument replaces the
inherited LD resource; it is never mixed with it. The one exception is
`plink` alone: `og_local(inherit = local, plink = "plink2")` keeps the
PLINK Reference’s fileset and changes the PLINK that reads it.

``` r

src_amr <- og_api(inherit = src_eur, ld_store = "AMR")
basophils <- og_tophits(src_amr, "OGS-00014:GCST90475125")
unique(basophils$clump_ld)
```

    ## [1] "hgdp1kgp-v2-AMR-k10"

``` r

m <- og_ld_matrix(src_eur, c("16:53772541:A:G", "16:53767042:C:T", "18:60161902:C:T", "2:628504:A:G", "5:75701728:A:AAAAT"))
og_provenance(m)$ld
```

    ## [1] "hgdp1kgp-v2-EUR-k10"

`src_eur` is unchanged, so the LD matrix uses the EUR LD Store. On a
Local Source, `og_local(inherit = local, bfile = "<prefix>")` keeps its
Store Releases and gives it a PLINK Reference. `inherit` must be the
same kind of Source: `og_api(inherit = local)` is refused (class
`opengwasr_argument`).

### Associations from one place, LD from another

A Source’s LD resource lives where the Source does:
[`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md)
answers LD from the API’s LD Stores, and
[`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)
from resources on your machine. To take Associations from one and LD
from the other, fetch with one Source and clump with the other, through
[`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md)
on the data you already have:

| Associations | LD | How |
|----|----|----|
| API | API | `og_tophits(og_api(), ids)` (matched by Assigned Ancestry), or `og_tophits(og_api(ld_store = "EUR"), ids)` |
| Local | PLINK | `og_tophits(og_local(gwas = ..., bfile = ...), ids, r2 = , kb = )` |
| Local | API | `og_clump(og_api(), og_tophits(local, ids, clump = FALSE, limit = 10000))` |
| API | PLINK | `og_clump(og_local(bfile = ...), og_tophits(og_api(), ids, clump = FALSE, limit = 10000), r2 = , kb = )` |

[`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md)
clumps each Analysis’s rows on their own, as
[`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
does, so every row gives each Analysis its own index variants ([Clumping
data you already have](#og-clump)). In the last two rows,
`limit = 10000` (the most) fetches every top hit to clump: `limit`
defaults to 1,000, and would cut the top hits off before they are
clumped, keeping only the strongest 1,000 of each Analysis. A warning of
class `opengwasr_truncated` says when it does.

The last row uses a Local Source with only a PLINK Reference: an
**LD-only Local Source**. It answers
[`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md)
and
[`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md)
and nothing else ([below](#plink-reference)). The third is in [LD from
the API’s LD
Stores](https://opengwas.github.io/opengwasr/articles/local-source.html#local-ld-api).

The rest of this vignette uses `src`, made with
[`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md),
which has no LD resource of its own, so each Analysis’s Assigned
Ancestry chooses, and `src_eur` where one LD Store is named.

### Matching by Assigned Ancestry

With no LD resource on the API Source, opengwasr uses the LD Store that
matches each Analysis’s **Assigned Ancestry**, the ancestry opengwasdb
inferred from the Analysis’s allele frequencies. Analyses of different
ancestries in one call each get their own LD Store. Here, two Analyses’
top hits are clumped (see [Clumping](#clumping)):

``` r

both <- og_tophits(src, c("OGS-00011:GCST003116", "OGS-00013:GCST90475090"))
unique(both[, c("id", "clump_ld", "clump_ld_choice", "clump_r2", "clump_kb")])
```

    ##                        id            clump_ld clump_ld_choice   clump_r2
    ## 1    OGS-00011:GCST003116 hgdp1kgp-v2-EUR-k10         matched 0.03186544
    ## 50 OGS-00013:GCST90475090 hgdp1kgp-v2-AFR-k10         matched 0.03490787
    ##    clump_kb
    ## 1     10000
    ## 50    10000

`OGS-00013:GCST90475090` (HbA1c, Assigned Ancestry AFR) is clumped
against African LD, at that LD Store’s own `t_far`. A non-European
Analysis is never given European LD unless you ask for it.
[`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md)
matches each Analysis’s own LD Store the same way, from its `id` column.
An LD matrix or LD proxies of a data frame of Associations need one LD
Store for all its Analyses, matched from its `id` column, and
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md)
records the choice in `ld` and `ld_choice`.

If several LD Stores serve one ancestry, opengwasr uses the one the LD
Store list declares as that ancestry’s default, and `clump_ld_choice`
(or `ld_choice`) says `"ancestry_default"`. Today each ancestry has one
LD Store.
([`og_ld_stores()`](https://opengwas.github.io/opengwasr/reference/og_ld_stores.md)’s
`default` column says which; it is `NA` until the API declares one, and
an ancestry with several LD Stores and no declared default is refused.)

A plain vector of variants has no ancestry to match, so
[`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md)
and
[`og_ld_proxies()`](https://opengwas.github.io/opengwasr/reference/og_ld_proxies.md)
need the Source’s LD resource when given one.

### Unassigned and mixed ancestry

An Analysis whose allele frequencies match no single ancestry
confidently is **Unassigned** (`assigned_ancestry` is `NA` in
[`og_analyses()`](https://opengwas.github.io/opengwasr/reference/og_analyses.md)).
opengwasr won’t guess an LD Store for it, and never falls back to
European LD:

``` r

og_tophits(src, "OGS-00014:GCST90475125")
```

    ## Error in `og_tophits()`:
    ## ! Can't choose an LD Store for OGS-00014:GCST90475125: its ancestry is Unassigned.
    ## ℹ Its allele frequencies match no single ancestry confidently.
    ## ℹ Give the Source an LD Store, such as `og_api(inherit = src, ld_store = "EUR")`, from: "EUR", "AFR", "EAS", "SAS", "AMR" (see `og_ld_stores()`).
    ## ℹ Or pass `clump = FALSE` for unclumped top hits.

The error has class `opengwasr_ld_unmatched`, a kind of
`opengwasr_refusal`, with the LD Stores in `e$ld_stores`. If you decide
AMR LD is right for it (its source describes it as “AMR”), say so, with
a Source whose LD resource is the AMR LD Store:

``` r

basophils <- og_tophits(og_api(inherit = src, ld_store = "AMR"), "OGS-00014:GCST90475125")
unique(basophils$clump_ld)
```

    ## [1] "hgdp1kgp-v2-AMR-k10"

The same refusal applies whenever one LD Store is needed for several
Analyses (an LD matrix, or `og_clump(by_analysis = FALSE)`), and their
ancestries differ or one of them is Unassigned. One LD Store is never
applied to a different ancestry without your say-so.

## LD matrices

[`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md)
returns the signed LD correlation r between every pair of variants, here
from `src_eur`’s EUR LD Store:

``` r

m <- og_ld_matrix(
  src_eur,
  c("16:53772541:A:G", "16:53767042:C:T", "18:60161902:C:T", "2:628504:A:G", "5:75701728:A:AAAAT")
)
round(m, 3)
```

    ##                    16:53772541:A:G 16:53767042:C:T 18:60161902:C:T 2:628504:A:G
    ## 16:53772541:A:G              1.000          -0.998              NA           NA
    ## 16:53767042:C:T             -0.998           1.000              NA           NA
    ## 18:60161902:C:T                 NA              NA               1           NA
    ## 2:628504:A:G                    NA              NA              NA            1
    ## 5:75701728:A:AAAAT              NA              NA              NA           NA
    ##                    5:75701728:A:AAAAT
    ## 16:53772541:A:G                    NA
    ## 16:53767042:C:T                    NA
    ## 18:60161902:C:T                    NA
    ## 2:628504:A:G                       NA
    ## 5:75701728:A:AAAAT                  1

- The dimension names are ALIDs. The sign of r refers to the first
  allele in each name. Square it for r².
- Variants can be rsIDs or ALIDs. rsIDs are translated to ALIDs first,
  and every ALID of an rsID is included.
- Absent variants are left out of the matrix, with a warning giving how
  many; `og_provenance(m)$absent` lists them.
- More than 500 variants are split into several Jobs, and the pieces are
  put back together so that every pair is included.

Given a data frame of Associations,
[`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md)
matches the LD Store by the Analyses’ Assigned Ancestry:

``` r

cad <- og_tophits(src, "OGS-00011:GCST003116")
m <- og_ld_matrix(src, cad)
og_provenance(m)$ld
```

    ## [1] "hgdp1kgp-v2-EUR-k10"

### Missing pairs, and filling them with 0

A pair the LD Store doesn’t hold is `NA`, never 0 by default. `NA` means
one of two things:

- **Below the Far Threshold.** The two variants are on one chromosome,
  between `near_bp` and `far_bp` apart, and their r² is below `t_far`.
  The LD Store knows they are at most weakly linked (for EUR, \|r\|
  below about 0.18), but not r itself.
- **Not measured.** The two variants are more than `far_bp` apart, or on
  different chromosomes. The LD Store holds nothing about them.

Many methods need a number for every pair. `fill_below_t_far = TRUE`
sets the first kind to 0 and leaves the second `NA`:

``` r

v <- c("7:105919152:C:T", "7:105920689:A:G", "7:110998200:C:T", "7:117559403:A:G", "16:53767042:C:T")
round(og_ld_matrix(src_eur, v), 3)
```

    ##                 7:105919152:C:T 7:105920689:A:G 7:110998200:C:T 7:117559403:A:G
    ## 7:105919152:C:T           1.000          -0.182              NA              NA
    ## 7:105920689:A:G          -0.182           1.000              NA              NA
    ## 7:110998200:C:T              NA              NA               1              NA
    ## 7:117559403:A:G              NA              NA              NA               1
    ## 16:53767042:C:T              NA              NA              NA              NA
    ##                 16:53767042:C:T
    ## 7:105919152:C:T              NA
    ## 7:105920689:A:G              NA
    ## 7:110998200:C:T              NA
    ## 7:117559403:A:G              NA
    ## 16:53767042:C:T               1

``` r

round(og_ld_matrix(src_eur, v, fill_below_t_far = TRUE), 3)
```

    ##                 7:105919152:C:T 7:105920689:A:G 7:110998200:C:T 7:117559403:A:G
    ## 7:105919152:C:T           1.000          -0.182               0              NA
    ## 7:105920689:A:G          -0.182           1.000               0              NA
    ## 7:110998200:C:T           0.000           0.000               1               0
    ## 7:117559403:A:G              NA              NA               0               1
    ## 16:53767042:C:T              NA              NA              NA              NA
    ##                 16:53767042:C:T
    ## 7:105919152:C:T              NA
    ## 7:105920689:A:G              NA
    ## 7:110998200:C:T              NA
    ## 7:117559403:A:G              NA
    ## 16:53767042:C:T               1

`7:110998200:C:T` is about 5 Mb from the first two variants and 6.6 Mb
from `7:117559403:A:G`: inside the far window, so those pairs become 0.
The first two variants are 11.6 Mb from `7:117559403:A:G`, beyond
`far_bp`, so those pairs stay `NA`, as do all pairs with the chromosome
16 variant.

The option is off by default because 0 claims more than the LD Store
knows: only that r² is below `t_far`. Within `near_bp` nothing is
missing, so it changes nothing there. A matrix from a PLINK Reference
has every pair, so the option doesn’t apply.

## LD proxies

[`og_ld_proxies()`](https://opengwas.github.io/opengwasr/reference/og_ld_proxies.md)
finds the variants in LD with each variant you give: every variant
within `kb` kilobases whose r² with it is at least `r2` (defaults 0.8
and 10,000 kb). Strongest first:

``` r

px <- og_ld_proxies(src_eur, "16:53767042:C:T")
nrow(px)
```

    ## [1] 98

``` r

head(px, 5)
```

    ##              alid      proxy_alid          r        r2 distance
    ## 1 16:53767042:C:T 16:53768582:C:T -1.0000000 1.0000000     1540
    ## 2 16:53767042:C:T 16:53769311:A:G  1.0000000 1.0000000     2269
    ## 3 16:53767042:C:T 16:53769662:A:T  1.0000000 1.0000000     2620
    ## 4 16:53767042:C:T 16:53775211:C:T -1.0000000 1.0000000     8169
    ## 5 16:53767042:C:T 16:53772541:A:G -0.9980431 0.9960899     5499

- `r` is signed for the first alleles of `alid` and `proxy_alid`;
  `distance` is in base pairs.
- Variants can be rsIDs or ALIDs, or a data frame of Associations, whose
  Analyses’ Assigned Ancestry then chooses the LD Store.
- Absent variants have no LD proxies, and a warning (class
  `opengwasr_ld_absent_variants`) gives how many.
- `r2` and `kb` follow the same rule as clumping: below `t_far`, `kb`
  can be at most 3,000 ([Your own r² and window](#refusals)). Otherwise
  the API refuses (`opengwasr_ld_refusal`, with `reason`, `t_far`,
  `near_bp` and `far_bp`).

This is an LD lookup, for questions about LD itself: which variants tag
a signal, or which variants to look for in data from outside OpenGWAS.
It is not the LD-proxy substitution ieugwasr made when an outcome lacked
a variant. opengwasr never swaps another variant’s Association in for a
missing one; Reference-Completed Store Releases hold Imputed
Associations instead (see [Imputed
Associations](https://opengwas.github.io/opengwasr/articles/opengwasr.html#imputed-associations)).

## Is a variant in the LD Store?

[`og_ld_reflookup()`](https://opengwas.github.io/opengwasr/reference/og_ld_reflookup.md)
says whether the LD Store holds each variant, so you can check
instruments before clumping or building an LD matrix. It returns one row
per variant asked, with `alid`, `rsid` (when known) and `present`:

``` r

og_ld_reflookup(src_eur, c("16:53767042:C:T", "rs1421085", "16:53767042:A:C"))
```

    ##              alid      rsid present
    ## 1 16:53767042:C:T      <NA>    TRUE
    ## 2 16:53767042:C:T rs1421085    TRUE
    ## 3 16:53767042:A:G rs1421085   FALSE
    ## 4 16:53767042:A:C      <NA>   FALSE

- An rsID stands for every ALID it names, one row each: `rs1421085`
  names two ALIDs here, and the LD Store holds one of them. An rsID no
  GWAS Store holds has one row with `alid` `NA` and `present` `FALSE`.
- A variant the LD Store doesn’t hold is a row, not a warning;
  [`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md)
  lists them as `absent`.
- The LD Store is chosen as for the other LD functions, and only an LD
  Store can answer: a Local Source, whose LD is a PLINK Reference, is
  refused.

## Clumping

Clumping keeps one variant per independent signal.

### Top hits are clumped by default

[`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
clumps unless you pass `clump = FALSE`:

``` r

cad <- og_tophits(src, "OGS-00011:GCST003116")
```

    ## Warning in og_tophits(src, "OGS-00011:GCST003116"): 234 of the 2,194 top hits
    ## of OGS-00011:GCST003116 aren't in LD Store hgdp1kgp-v2-EUR-k10, so they can't
    ## be clumped and were left out.

``` r

nrow(cad)
```

    ## [1] 49

``` r

head(cad[, c("rsid", "alid", "beta", "pval", "clump_size")], 5)
```

    ##          rsid            alid        beta         pval clump_size
    ## 1   rs2891168  9:22098620:A:G -0.19335911 2.296241e-98        193
    ## 2  rs55730499 6:160584578:C:T -0.31671619 5.392728e-39         94
    ## 3   rs7528419 1:109274570:A:G  0.11453085 1.968746e-23          9
    ## 4   rs4711863  6:12915185:C:G -0.09856474 1.613590e-21         96
    ## 5 rs115654617 2:203029276:A:C  0.13790739 3.119371e-18        316

The 2,194 top hits at p ≤ 5e-8 reduce to 49 index variants. `clump_size`
counts the top hits each index variant stands for, itself included.

- Variants are ranked by \|z\|. Starting from the strongest, each index
  variant absorbs every weaker top hit within `kb` kilobases whose r²
  with it is above `r2` (the rule `plink --clump` uses).
- Every top hit at the threshold is clumped, and `limit` (default 1,000)
  then caps the index variants returned.
- Absent top hits can’t be clumped. They are left out, with a warning
  giving how many, and the count is kept in the provenance.

### Stored Clumps, and clumping on the fly

`clump = TRUE` is the default, and `clump = FALSE` gives every top hit,
unclumped. A set of **Clumps** is always made with one configuration: an
LD resource, `r2` and `kb`.

- **Stored Clumps.** Store Releases will carry Clumps made when each
  release was built: its 5e-8 top hits, clumped with each ancestry’s
  default LD resource and its `r2` and `kb`. The default call,
  `og_tophits(src, ids)`, will read them, so it needs no LD resource at
  all, on either Source. They are used only when your request matches
  that configuration exactly, never when it is merely close. Today’s
  Store Releases don’t have stored Clumps yet.
- **Clumping on the fly.** Until a Store Release has them,
  `clump = TRUE` clumps on the fly, with the Source’s LD resource. Any
  other `threshold`, `r2`, `kb` or LD resource is always clumped on the
  fly, with the Source’s LD resource.
- **Provenance says which.** `og_provenance(x)$clump` records the LD
  resource, `r2` and `kb` used, and in `clumps` whether the Clumps were
  `"stored"` or `"computed"`.

Once a Store Release has stored Clumps, the default call reads them:

``` r

cad <- og_tophits(src, "OGS-00011:GCST003116")
og_provenance(cad)$clump[, c("id", "ld", "r2", "kb", "leads", "clumps")]
```

``` text
# illustrative
                    id                  ld         r2    kb leads clumps
1 OGS-00011:GCST003116 hgdp1kgp-v2-EUR-k10 0.03186544 10000    49 stored
```

### Default thresholds come from the LD Store

You don’t need to choose `r2` and `kb`. By default, opengwasr clumps
within the LD Store’s far window (10,000 kb), at its Far Threshold
`t_far` (about 0.03). That is always a question the LD Store can answer
exactly.

ieugwasr’s default, r² 0.001 within 10,000 kb, is not. Beyond 3,000 kb
an LD Store keeps only pairs with r² ≥ `t_far`, so it can’t tell an r²
of 0.001 from one of 0.02 there. If you ask for it, opengwasr refuses.

### Your own r² and window, and reading a refusal

Pass `r2` and `kb` to clump with your own thresholds. If the LD Store
can answer them exactly, it does. If not, the API refuses and says why,
and opengwasr passes its message on. It never clumps at something close
to what you asked.

``` r

og_tophits(src, "OGS-00011:GCST003116", r2 = 0.001, kb = 10000)
```

    ## Error in `og_tophits()`:
    ## ! r2=0.001 is below t_far=0.03186544342507645: beyond 3,000,000 bp the store holds only the Bound r² < t_far for unstored pairs, which does not decide r̂² ≥ r2

The message is the API’s. The error has class `opengwasr_ld_refusal`, a
kind of `opengwasr_refusal`. It carries the API’s reason and the LD
Store’s limits as fields, so a script can decide what to do: here, any
`r2` is answered within `near_bp`, 3,000 kb, and an `r2` of at least
`t_far` within `far_bp`, 10,000 kb. Catch `opengwasr_ld_refusal` itself
to read those fields; the parent class `opengwasr_refusal` also covers
refusals that don’t have them:

``` r

cad_strict <- tryCatch(
  og_tophits(src, "OGS-00011:GCST003116", r2 = 0.001, kb = 10000),
  opengwasr_ld_refusal = function(e) {
    message("Refused (", e$reason, "); clumping within ", e$near_bp / 1000, " kb instead.")
    og_tophits(src, "OGS-00011:GCST003116", r2 = 0.001, kb = e$near_bp / 1000)
  }
)
```

    ## Refused (threshold_below_t_far); clumping within 3000 kb instead.

Fields, as the API sends them: `e$reason` (here
`"threshold_below_t_far"`), `e$store` (the LD Store), `e$ancestry`,
`e$t_far`, `e$near_bp` and `e$far_bp`.
[`og_ld_proxies()`](https://opengwas.github.io/opengwasr/reference/og_ld_proxies.md)
refuses in the same way.

### Clumping data you already have

[`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md)
clumps a data frame of Associations you already have, such as top hits
fetched unclumped from another Source. It needs `alid` and `z`, and an
`id` column saying which Analysis each row is from. It takes `r2` and
`kb` exactly as
[`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
does, and uses the Source’s LD resource.

- **Each Analysis is clumped on its own** (`by_analysis = TRUE`, the
  default), as
  [`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
  does, so
  `og_clump(src, og_tophits(src, ids, clump = FALSE, limit = 10000))`
  gives each Analysis the same index variants as `og_tophits(src, ids)`.
- **Each Analysis gets its own LD Store**: with no LD resource on the
  API Source, the one matching its Assigned Ancestry, as in [Matching by
  Assigned Ancestry](#ld-store-matching), so Analyses of different
  ancestries can be clumped in one call.
- **The rows come back in their order in `data`**: the rows of each
  Analysis’s index variants, with the [clumping
  columns](#clumping-provenance).
- **A data frame with no `id` column is one set.** It has no Assigned
  Ancestry to match, so the Source needs an LD resource, such as
  `og_clump(src_eur, data)`.

To clump several Analyses together, as one set whose index variants
compete, such as the instruments of a multivariable MR, pass
`by_analysis = FALSE`: a variant listed several times is then ranked by
its largest \|z\|, every Association of a kept variant is returned, and
with no LD resource on the API Source every Analysis must share one
Assigned Ancestry (else `opengwasr_ld_unmatched`, as
[above](#unassigned-and-mixed-ancestry)). Here the 100 strongest index
variants of BMI and of LDL cholesterol are clumped together:

``` r

bmi <- og_tophits(src, "OGS-00011:GCST90029007", limit = 100)
ldl <- og_tophits(src, "OGS-00011:GCST90002412", limit = 100)
joint <- og_clump(src, rbind(bmi, ldl), by_analysis = FALSE)
table(joint$id)
```

    ## 
    ## OGS-00011:GCST90002412 OGS-00011:GCST90029007 
    ##                    100                     97

### Clumping provenance

Every clumped data frame records how it was clumped, in columns that
survive subsetting, merging and binding:

| Column | Meaning |
|----|----|
| `clump_ld` | The LD Store’s name, or the PLINK Reference’s path |
| `clump_ld_choice` | How it was chosen: `"matched"` (the one LD Store for the Analysis’s Assigned Ancestry), `"ancestry_default"` (an ancestry’s default LD Store: the Source’s `ld_store` is an ancestry code, or the Assigned Ancestry has several LD Stores) or `"user"` (the Source names an LD Store, or carries a PLINK Reference) |
| `clump_r2`, `clump_kb` | The r² and window applied, whether defaults or yours |
| `clump_size` | How many top hits the index variant stands for |

[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md)
(defined in [Getting
started](https://opengwas.github.io/opengwasr/articles/opengwasr.html#provenance))
adds the counts behind them. Unlike the columns, it doesn’t survive
[`rbind()`](https://rdrr.io/r/base/cbind.html):

``` r

og_provenance(cad)
```

    ## ── opengwasr provenance ─────────────────
    ## • Query: og_tophits(threshold = 5e-08, limit = 1000, observed_only = FALSE, clump = TRUE, original_units = FALSE)
    ## • Store Releases: OGS-00011/OGS-00011-completed
    ## • LD Store: ancestry (); 0 variant(s) absent
    ## • Clumped: OGS-00011:GCST003116 in hgdp1kgp-v2-EUR-k10 (matched), r² 0.0319 within 10,000 kb; 2,194 top hits, 234 not in the LD Store, 49 index variants, computed
    ## • Jobs: 86d6e51a
    ## • Rows: 49 returned of 49 matching
    ## • Retrieved: 2026-11-26 10:17:34 UTC

``` r

og_provenance(cad)$clump
```

    ##                     id                  ld ld_choice         r2    kb hits
    ## 1 OGS-00011:GCST003116 hgdp1kgp-v2-EUR-k10   matched 0.03186544 10000 2194
    ##   absent leads   clumps
    ## 1    234    49 computed

`hits` counts the top hits at the threshold, `absent` those the LD Store
doesn’t hold, and `leads` the index variants. `clumps` says whether the
Clumps were `"computed"` on the fly, as they all are today, or read
`"stored"` with the Store Release ([Stored Clumps](#stored-clumps)). Two
clumpings that differ can be explained from these alone.

## Using a PLINK Reference

A **PLINK Reference** is a PLINK binary genotype fileset (`.bed`,
`.bim`, `.fam`) on your own machine, such as the 1000 Genomes files
ieugwasr users already have. It is the LD resource of a Local Source,
given as `bfile`, and PLINK 1.9 or PLINK 2 reads it on your machine, as
ieugwasr’s local functions did:

- `og_local(gwas = ..., bfile = prefix)`: Associations from your Store
  Releases, LD from your PLINK Reference ([Clumping and LD on a Local
  Source](https://opengwas.github.io/opengwasr/articles/local-source.html#local-clumping)).
- `og_local(bfile = prefix)`: an **LD-only Local Source**, with no Store
  Releases. It answers
  [`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md)
  and
  [`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md)
  for data you already have, such as top hits from the API Source: the
  last row of the [table above](#mixing).

For your own, such as `og_local(bfile = "~/ld/1kg/EUR")`, the calls are
`og_clump(ld_src, og_tophits(src, ids, clump = FALSE, limit = 10000), r2 = 0.001, kb = 10000)`
and `og_ld_matrix(ld_src, rsids)`. Here, the small synthetic PLINK
Reference that comes with opengwasr, 300 made-up people, whose rsIDs
include a few of the CAD and BMI index variants from [above](#og-clump),
clumps those of each Analysis on their own, and reads an LD matrix of
rsIDs. It holds only a few of these real variants, so most are left out,
as the message says; your own PLINK Reference holds far more:

``` r

ld_src <- og_local(bfile = file.path(system.file("plink", package = "opengwasr"), "ref"))
ld_src
```

    ## <og_local_source>
    ## • Store Releases: none (LD only)
    ## • LD resource: PLINK Reference /home/runner/work/_temp/Library/opengwasr/plink/ref
    ##   PLINK 1.90b6.21: /home/runner/work/_temp/Library/genetics.binaRies/bin/plink

``` r

og_ld_resource(ld_src)$plink_version   # the PLINK that was found, 1.9 or 2
```

    ## [1] "1.90b6.21"

``` r

clumped_local <- og_clump(ld_src, rbind(cad, bmi), r2 = 0.001, kb = 10000)
```

    ## ℹ 7 of 149 variants matched the PLINK Reference by rsID; 142 not in it
    ## were left out.

    ## Warning in og_clump(ld_src, rbind(cad, bmi), r2 = 0.001, kb = 10000): LD from a PLINK Reference is computed from its genotypes without adjusting for population structure (principal components), unlike OpenGWAS LD Stores and Genotype Stores, so it may be less accurate.
    ## ℹ PLINK Reference: /home/runner/work/_temp/Library/opengwasr/plink/ref.
    ## This warning is displayed once per session.

``` r

clumped_local[, c("id", "rsid", "alid", "z", "clump_r2", "clump_kb", "clump_size")]
```

    ##                       id       rsid            alid          z clump_r2
    ## 1   OGS-00011:GCST003116  rs2891168  9:22098620:A:G -21.049805    0.001
    ## 2   OGS-00011:GCST003116 rs55730499 6:160584578:C:T -13.062500    0.001
    ## 3   OGS-00011:GCST003116  rs7528419 1:109274570:A:G   9.974609    0.001
    ## 4   OGS-00011:GCST003116  rs4711863  6:12915185:C:G  -9.527344    0.001
    ## 5 OGS-00011:GCST90029007 rs56094641 16:53772541:A:G -38.534206    0.001
    ## 6 OGS-00011:GCST90029007  rs6567160 18:60161902:C:T  23.840820    0.001
    ## 7 OGS-00011:GCST90029007   rs539515 1:177919890:A:C -19.618164    0.001
    ##   clump_kb clump_size
    ## 1    10000          1
    ## 2    10000          1
    ## 3    10000          1
    ## 4    10000          1
    ## 5    10000          1
    ## 6    10000          1
    ## 7    10000          1

``` r

m_local <- og_ld_matrix(ld_src, c("rs4977574", "rs2891168", "rs1333049", "rs1421085"))
round(m_local, 3)
```

    ##               rs4977574_G_A rs2891168_G_A rs1333049_C_G rs1421085_C_T
    ## rs4977574_G_A         1.000         0.701         0.085        -0.011
    ## rs2891168_G_A         0.701         1.000         0.024         0.045
    ## rs1333049_C_G         0.085         0.024         1.000         0.033
    ## rs1421085_C_T        -0.011         0.045         0.033         1.000

- **Variants are matched by rsID,** because PLINK References are usually
  on GRCh37 while ALIDs are on GRCh38. Variants without an rsID match
  are left out, and the message says how many. An LD matrix from a PLINK
  Reference is named `<rsid>_<allele>_<allele>` with the PLINK
  Reference’s alleles, as ieugwasr named it, and the sign of r again
  refers to the first allele in each name.
- **Its LD isn’t adjusted for principal components.** LD from a PLINK
  Reference is computed from its genotypes without adjusting for
  population structure (principal components), unlike OpenGWAS LD Stores
  and Genotype Stores, so it may be less accurate. opengwasr warns of
  this once per PLINK Reference in a session (class
  `opengwasr_plink_unadjusted`). The chunk above shows the warning.
- **`r2` and `kb` are required.** A PLINK Reference has no recorded
  defaults, so opengwasr won’t choose for you. Without them,
  [`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
  and
  [`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md)
  stop and ask for both.
- **Each Analysis is clumped on its own,** one PLINK run per Analysis,
  as
  [`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
  does on a Local Source with a PLINK Reference, so top hits from the
  API get the index variants
  [`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
  would give with this PLINK Reference.
- **PLINK itself** is PLINK 1.9, or PLINK 2 from build 2.0.0-a.6.0
  (November 2024) on, the first with every option opengwasr uses.
  `og_local(bfile = , plink = )` takes its path, or a command on your
  `PATH` such as `plink = "plink2"`; as in a shell, a name without a `/`
  is a command. Without `plink`, opengwasr looks in the
  genetics.binaRies package if it is installed, then for `plink`, then
  `plink2`, on your `PATH`, and takes the first that is a supported
  PLINK. Another program called `plink`, such as PuTTY’s, or an older
  PLINK, is skipped. If none is supported, the error (class
  `opengwasr_plink_not_found`) lists what it skipped, in `skipped`, and
  says how to install PLINK.
- **Its version** is read once, when the Source is made, and kept on its
  PLINK Reference (`og_ld_resource(ld_src)$plink_version`) and in
  provenance (`og_provenance(x)$plink_version`, beside `plink_bin`). A
  PLINK you name that isn’t supported, such as 1.07, is refused (class
  `opengwasr_plink_unsupported`, with the `version` found and the
  versions `supported`). If a supported `plink` or `plink2` is on your
  `PATH`, the error suggests it.
- **PLINK 1.9 and PLINK 2 give the same answer.** opengwasr asks each
  for the same thing in its own words: an LD matrix of r with the
  reference’s allele order kept (`--r square --keep-allele-order` in
  1.9, `--r-unphased square ref-based` in 2), and clumping with
  `--clump`, read from `.clumped` or `.clumps`. On the PLINK Reference
  that comes with opengwasr, both give the same clumps, and r equal to
  within 1e-6.
- [`og_ld_proxies()`](https://opengwas.github.io/opengwasr/reference/og_ld_proxies.md)
  and
  [`og_ld_reflookup()`](https://opengwas.github.io/opengwasr/reference/og_ld_reflookup.md)
  read LD Stores only, so a Local Source refuses them.
- Provenance works as before: `clump_ld` is the PLINK Reference’s path,
  and `clump_ld_choice` is `"user"`.

### Top hits from the API, clumped with PLINK

The last row of the [table](#mixing), end to end: BMI’s top hits from
the API Source, unclumped, then clumped with the PLINK Reference, each
Analysis on its own. Here `limit` is left at its default, so the warning
says the API’s top hits were cut off at 1,000 before clumping; with your
own, pass `limit = 10000` to clump them all:

``` r

bmi_hits <- og_tophits(src, "OGS-00011:GCST90029007", clump = FALSE)
```

    ## Warning in og_tophits(src, "OGS-00011:GCST90029007", clump = FALSE): OGS-00011:GCST90029007 has more than 1,000 top hits at p <= 5e-08; only the strongest 1,000 were returned.
    ## ℹ Raise `limit` (at most 10,000) for more.

``` r

bmi_local <- og_clump(ld_src, bmi_hits, r2 = 0.001, kb = 10000)
```

    ## ℹ 6 of 1,000 variants matched the PLINK Reference by rsID; 994 not in
    ## it were left out.

``` r

bmi_local[, c("id", "rsid", "alid", "z", "clump_size")]
```

    ##                       id       rsid            alid         z clump_size
    ## 1 OGS-00011:GCST90029007 rs56094641 16:53772541:A:G -38.53421          1
    ## 2 OGS-00011:GCST90029007  rs1421085 16:53767042:C:T  38.52622          3
    ## 3 OGS-00011:GCST90029007  rs6567160 18:60161902:C:T  23.84082          1
    ## 4 OGS-00011:GCST90029007   rs539515 1:177919890:A:C -19.61816          1

``` r

og_provenance(bmi_local)$clump[, c("ld_choice", "r2", "kb", "hits", "absent", "leads", "clumps")]
```

    ##   ld_choice    r2    kb hits absent leads   clumps
    ## 1      user 0.001 10000 1000    994     4 computed

### An LD-only Local Source

An LD-only Local Source has no Store Releases, so it has nothing to look
an ALID’s rsID up in, and never calls the API. Give it variants it can
match by rsID: a data frame with an `rsid` column, such as any top hits
or Associations, or rsIDs. ALIDs alone are refused (class
`opengwasr_gwas_store_required`, with the ALIDs in `alids`), and the
error names the ways round, in `forms`: give the Source the Store
Releases that hold them, `og_local(gwas = ..., bfile = ...)` (`"gwas"`);
pass a data frame with an `rsid` column (`"rsid_column"`); or look their
rsIDs up on the API first (`"api_rsids"`). Every verb that reads
Associations refuses it with the same class:

``` r

og_ld_matrix(ld_src, c("9:22125504:C:G", "9:22098575:A:G"))
```

    ## Error in `og_ld_matrix()`:
    ## ! This Local Source holds no Store Releases to look up the rsIDs of 2 ALIDs (9:22125504:C:G, 9:22098575:A:G), which its PLINK Reference matches by rsID.
    ## ℹ Give it the Store Releases too: `og_local(gwas = , bfile = )`, or `og_local(inherit = src, gwas = )`.
    ## ℹ Or pass a data frame with an `rsid` column, such as top hits or Associations, or rsIDs.
    ## ℹ Or look the rsIDs up on the API first: `og_variants_chrpos(og_api(), positions)` gives each ALID's `rsid`, for `positions` its "chromosome:position".

``` r

og_tophits(ld_src, "OGS-00011:GCST003116")
```

    ## Error in `og_tophits()`:
    ## ! This Local Source holds no Store Releases: it was made with only a PLINK Reference (`og_local(bfile = )`), for LD.
    ## ℹ Give it the Store Releases too: `og_local(gwas = , bfile = )`, or `og_local(inherit = src, gwas = )`.

To look the rsIDs up on the API, ask
[`og_variants_chrpos()`](https://opengwas.github.io/opengwasr/reference/og_variants_rsid.md)
for the ALIDs’ positions, and take the `rsid` of each ALID; then read
the LD matrix of those rsIDs from the PLINK Reference:

``` r

alids <- c("16:53767042:C:T")
positions <- sub(":[^:]+:[^:]+$", "", alids)   # chromosome:position
found <- og_variants_chrpos(src, positions, releases = "OGS-00011")
rs <- found$rsid[match(alids, found$alid)]
rs
```

    ## [1] "rs1421085"

``` r

og_ld_matrix(ld_src, c(rs, "rs9939609"))
```

    ##               rs1421085_C_T rs9939609_A_T
    ## rs1421085_C_T      1.000000      0.519285
    ## rs9939609_A_T      0.519285      1.000000
