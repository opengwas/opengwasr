# Migrating from ieugwasr

opengwasr replaces ieugwasr as the way R reaches OpenGWAS data. ieugwasr
talks to **Legacy OpenGWAS**, the OpenGWAS service in production today,
which is being retired. opengwasr talks to the new OpenGWAS API.
ieugwasr stays installable, frozen, until Legacy OpenGWAS is retired, so
published analyses can still be rerun.

**If you use TwoSampleMR,** you may not need to change your code.
`extract_instruments()`, `extract_outcome_data()`, `clump_data()`,
`available_outcomes()`, `ld_matrix()` and `add_metadata()` keep working,
and call opengwasr underneath once TwoSampleMR switches over. Your
numbers will still move, for the reasons in [What changed
scientifically](#what-changed). Read that section.

opengwasr is not a drop-in replacement for ieugwasr. Its functions are
named after the new API’s own terms (Analysis, Association, GWAS Store,
LD Store), and they behave differently where the new data differ.
Familiar names that quietly behaved differently would be worse.

> **Status.** Every chunk runs, replayed from responses recorded against
> the development Data API on 23 November 2026. ieugwasr code is shown
> for comparison only, and isn’t run.
>
> *Developer note:* The API responses in this vignette were recorded
> against the development Data API, directly, with a developer key (see
> `vignettes/recordings.R`). Once the OpenGWAS Gateway is live (#18),
> re-record through it, so the vignettes show the access users will
> have.

## A typical workflow, before and after

With ieugwasr:

``` r

library(ieugwasr)
exposure <- tophits("ukb-b-19953")                       # clumped: r2 0.001, 10,000 kb, EUR
outcome <- associations(exposure$rsid, "ebi-a-GCST003116") # LD proxies for missing variants
```

With opengwasr:

``` r

library(opengwasr)
src <- og_api()                                          # reads OPENGWAS_JWT
exposure <- og_tophits(src, "ukb-b-19953", limit = 100)  # clumped in the matching LD Store
```

    ## Warning in og_tophits(src, "ukb-b-19953", limit = 100): OGS-00009:ukb-b-19953 has more than 100 index variants at p <= 5e-08; only the strongest 100 were returned.
    ## ℹ Raise `limit` (at most 10,000) for more.

    ## Warning in og_tophits(src, "ukb-b-19953", limit = 100): 10,655 of the 79,248
    ## top hits of OGS-00009:ukb-b-19953 aren't in LD Store hgdp1kgp-v2-EUR-k10, so
    ## they can't be clumped and were left out.

``` r

outcome <- og_associations(src, exposure$alid, "ebi-a-GCST003116")
table(outcome$association_status)                        # Imputed Associations, labelled
```

    ## 
    ##  imputed observed 
    ##        7       93

`limit = 100` keeps this example small: it returns the 100 strongest of
the index variants.

Every call takes a Source first:
[`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md)
here. Legacy OpenGWAS identifiers still work where they name one
Analysis, but opengwasr returns the new ones (`OGS-00009:ukb-b-19953`,
`OGS-00011:GCST003116`). Use those in new code.

## Function by function

Every ieugwasr function that TwoSampleMR calls:

| ieugwasr | Used by TwoSampleMR in | opengwasr | What changed |
|----|----|----|----|
| `get_opengwas_jwt()` | every data function, as `opengwas_jwt =` | [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md) | The token is read once, from the same `OPENGWAS_JWT`, and travels with the Source. No function takes a token argument. |
| `legacy_ids(ids)` | `extract_instruments()`, `extract_outcome_data()`, `mv_extract_exposures()` | `og_analyses(src, ids)$id` | Analyses are named `<store_id>:<analysis_id>`. Legacy identifiers are accepted everywhere if they name exactly one Analysis. Unknown ones are an error, not a silent drop. Legacy identifiers whose analysis now has a later release with different numbers (`finn-b-*`) give that later release, with a warning naming it. |
| `gwasinfo()` | `available_outcomes()` | `og_analyses(src)` | Reads the whole catalogue, page by page. Columns differ: `analysis_label` rather than `trait`, `id` in the new form, and `assigned_ancestry`, `stored_effect_scale` and `sample_size_kind` are new. |
| `gwasinfo(id)` | `add_metadata()` | `og_analyses(src, ids)` | Metadata keeps the API’s names: `sample_size`, `n_cases` and `n_controls` (Associations carry them as `n`, `ncase` and `ncontrol`). There is no `unit`. `sd` is now `original_sd`, with `original_sd_method` saying how it was obtained. |
| `tophits(id, pval, clump, r2, kb, force_server)` | `extract_instruments()` | `og_tophits(src, analyses, threshold = 5e-8, limit = 1000, clump = TRUE, r2 = NULL, kb = NULL)` | Still clumped by default, but the default thresholds come from the LD Store (r² about 0.03 within 10,000 kb). r² 0.001 within 10,000 kb is refused. The LD Store matches each Analysis’s ancestry, not always EUR, unless the Source names one (`og_api(ld_store = "EUR")`). `threshold` can be at most 5e-4. `limit` caps the top hits per Analysis (1,000 by default), with a warning when it cuts any off. `force_server` is gone. |
| `associations(variants, id, proxies, r2, align_alleles, palindromes, maf_threshold)` | `extract_outcome_data()`, `ldsc_rg()` | `og_associations(src, variants, analyses, observed_only = FALSE)` | No LD proxies: Imputed Associations are included and labelled instead, so `proxies`, `r2`, `align_alleles`, `palindromes` and `maf_threshold` have no equivalent. Variants can be rsIDs or ALIDs. Large queries are split automatically. |
| `ld_clump(dat, clump_kb, clump_r2, clump_p, pop, bfile, plink_bin)` | `clump_data()` | `og_clump(src, data, r2 = NULL, kb = NULL, by_analysis = TRUE)` | Ranks by \|z\|, not p. Clumps each Analysis (`id`) on its own, as `ld_clump()` did; `by_analysis = FALSE` clumps all the rows as one set. `pop` becomes the Source’s LD resource, `og_api(ld_store = "EUR")`, or `og_api(inherit = src, ld_store = "EUR")` from a Source you have; without one, the LD Store is matched from the Analyses’ ancestry. `clump_r2`/`clump_kb` become `r2`/`kb`, defaulting to the LD Store’s own. `bfile` and `plink_bin` become a Local Source’s LD resource, `og_local(bfile = bfile, plink = plink_bin)`, with PLINK 1.9 or 2: `og_clump(og_local(bfile = bfile), data, r2 = , kb = )`. For `clump_p`, filter `data` before clumping. |
| `ld_matrix(variants, with_alleles, pop)` | `ld_matrix()` | `og_ld_matrix(src, variants, fill_below_t_far = FALSE)` | `pop` becomes the Source’s LD resource, as for [`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md). Named by ALID, which carries the alleles, so `with_alleles` is gone. Pairs the LD Store doesn’t hold are `NA`, not 0; `fill_below_t_far = TRUE` sets the weakly linked ones to 0 ([missing pairs](https://opengwas.github.io/opengwasr/articles/ld.html#ld-matrix-fill)). More than 500 variants are split automatically. |
| `ld_reflookup(rsid, pop)` | the help for `clump_data()` and `ld_matrix()` | none | [`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md), [`og_ld_proxies()`](https://opengwas.github.io/opengwasr/reference/og_ld_proxies.md) and clumping report the variants the LD Store doesn’t hold. |
| `fill_n(d)` | `ldsc_h2()`, `ldsc_rg()` | none needed | Every Association already has `n`, `ncase` and `ncontrol`, with `sample_size_kind`. [`og_analyses()`](https://opengwas.github.io/opengwasr/reference/og_analyses.md) has the same counts under the API’s names, `sample_size`, `n_cases` and `n_controls`. |
| `afl2_list()`, `infer_ancestry()` | `ldsc_h2()`, `ldsc_rg()` | none | The new API holds no allele-frequency reference data (it answers `501`). To choose LD, the Analysis’s Assigned Ancestry replaces inferring it. TwoSampleMR’s LDSC functions move over later. |

Other ieugwasr functions:

| ieugwasr | opengwasr | What changed |
|----|----|----|
| `api_status()`, `user()` | `og_status(src)`, `og_user(src)` | [`og_user()`](https://opengwas.github.io/opengwasr/reference/og_status.md) reports your allowance. |
| `phewas(variants, pval = 1e-5, batch)` | `og_phewas(src, variants, threshold = 1e-8, releases)` | `variants` are rsIDs and ALIDs, as in ieugwasr; each rsID runs one PheWAS per ALID it names, combined in the order given. The default threshold is 1e-8; pass `threshold = 1e-5` for ieugwasr’s. `batch` becomes `releases` (GWAS Store ids). |
| `variants_rsid()`, `variants_chrpos(chrpos, radius)` | `og_variants_rsid(src, rsids)`, `og_variants_chrpos(src, positions)` | Return ALIDs from the GWAS Stores’ own variant tables, not dbSNP records. Positions are on GRCh38 and exact: there is no `radius`. |
| `ld_clump_local(dat, clump_kb, clump_r2, clump_p, bfile, plink_bin)`, `ld_matrix_local()` | `og_clump(og_local(bfile = bfile, plink = plink_bin), data, r2 = , kb = )` and `og_ld_matrix(og_local(bfile = bfile), variants)`; or [`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md) on `og_local(gwas = ..., bfile = bfile)` | The same functions as for LD Stores. `og_local(bfile = )` alone is a Local Source with LD only: it clumps and reads LD matrices for data you already have, such as top hits from the API, matched by their `rsid` column ([Using a PLINK Reference](https://opengwas.github.io/opengwasr/articles/ld.html#plink-reference)). With Store Releases too, every call uses the PLINK Reference ([Clumping and LD](https://opengwas.github.io/opengwasr/articles/local-source.html#local-clumping)). `r2` and `kb` are required: a PLINK Reference has no defaults. LD from a PLINK Reference is computed from its genotypes without adjusting for population structure (principal components), unlike OpenGWAS LD Stores and Genotype Stores, so it may be less accurate. opengwasr warns of this once per PLINK Reference in a session (class `opengwasr_plink_unadjusted`). PLINK 2 works as well as PLINK 1.9, and `plink_bin` can be a command on your `PATH`, such as `"plink2"`. |
| `variants_gene()` | none | The new API has no gene annotation. |

## What changed scientifically

These are the reasons the same analysis can give different numbers
through opengwasr. None of them happens silently. Each is either a
documented default or a refusal that tells you what to ask for instead.

### No proxies: Imputed Associations instead

ieugwasr’s `associations()` asked for an LD proxy by default when an
outcome Analysis lacked a variant: a nearby variant in LD, found in
European 1000 Genomes data, with its alleles realigned. opengwasr has no
proxy lookup. The new Store Releases are Reference-Completed: beside the
Associations each source reported, they hold an **Imputed Association**
for most variants the source didn’t report, built once, consistently,
when the GWAS Store was built.

So a missing variant comes back as itself, labelled
`association_status == "imputed"`, rather than as a different variant.
Imputed Associations are included by default; `observed_only = TRUE`
leaves them out. In the example above, all 100 BMI instruments are found
in CAD, 7 of them Imputed; with Observed Associations only, 93. Report
how many of your outcome Associations were Imputed.

To look up the variants in LD with a given one, as a question about LD
rather than a substitute Association, use
[`og_ld_proxies()`](https://opengwas.github.io/opengwasr/reference/og_ld_proxies.md)
(see [LD
proxies](https://opengwas.github.io/opengwasr/articles/ld.html#ld-proxies)).

``` r

observed <- og_associations(src, exposure$alid, "ebi-a-GCST003116", observed_only = TRUE)
nrow(observed)
```

    ## [1] 93

### Continuous traits are per SD

ieugwasr returned effects on whatever scale each source published: per
SD for some continuous traits, in natural units (kg/m², mmol/L) for
others. opengwasr returns continuous traits **per SD** by default, so
effects are comparable across Analyses, and says so in `effect_scale` on
every Association. Binary traits are log odds as before, except the
1,846 binary `ukb-b-*` Analyses that the GWAS Store holds per SD:
`effect_scale` says which.

So MR estimates for a continuous exposure are now per SD of the
exposure. For natural units, pass `original_units = TRUE`. opengwasr
rescales by the Analysis’s recorded phenotype SD, and refuses when there
isn’t one, including for binary Analyses stored per SD.

``` r

og_associations(src, "rs1421085", "OGS-00011:GCST90624700",
                original_units = TRUE)[, c("beta", "se", "effect_scale")]
```

    ##       beta         se effect_scale
    ## 1 0.466714 0.01520618     original

### Clumping defaults come from the LD Store

ieugwasr and TwoSampleMR clumped at r² 0.001 within 10,000 kb. The new
LD Stores hold every r² only within 3,000 kb. Beyond that, they hold
only pairs with r² of at least their `t_far` (about 0.03; 0.063 for
AMR), so they can’t answer r² 0.001 at 10,000 kb exactly. opengwasr’s
default is the LD Store’s own: within 10,000 kb at its `t_far`. If you
ask for 0.001 within 10,000 kb, it refuses, and the error suggests what
the LD Store can answer: r² 0.001 within 3,000 kb, or r² of at least
`t_far` within 10,000 kb.

Expect different instrument counts from before. Each clumped data frame
records the LD Store, r² and window it used (see [clumping
provenance](https://opengwas.github.io/opengwasr/articles/ld.html#clumping-provenance)).
Store Releases will also carry stored Clumps, made at these defaults
when they were built, and the default call will read them; any other
`r2`, `kb` or LD is clumped on the fly, and the provenance says which
([Stored
Clumps](https://opengwas.github.io/opengwasr/articles/ld.html#stored-clumps)).

``` r

unique(exposure[, c("clump_ld", "clump_ld_choice", "clump_r2", "clump_kb")])
```

    ##              clump_ld clump_ld_choice   clump_r2 clump_kb
    ## 1 hgdp1kgp-v2-EUR-k10         matched 0.03186544    10000

### The LD Store matches the Analysis’s ancestry

ieugwasr clumped against European LD unless you set `pop`. opengwasr
clumps each Analysis in the LD Store of its Assigned Ancestry, inferred
from the Analysis’s allele frequencies. An Analysis with no confident
ancestry (Unassigned), or a set of Analyses whose ancestries differ, is
refused unless you choose the LD, with the Source’s LD resource:
`og_api(ld_store = "EUR")`, or `og_api(inherit = src, ld_store = "EUR")`
for a Source like the one you have. There is no fallback to European LD.

``` r

og_ld_stores(src)[, c("ld_store", "ancestry", "t_far")]
```

    ##              ld_store ancestry      t_far
    ## 1 hgdp1kgp-v2-EUR-k10      EUR 0.03186544
    ## 2 hgdp1kgp-v2-AFR-k10      AFR 0.03490787
    ## 3 hgdp1kgp-v2-EAS-k10      EAS 0.02922861
    ## 4 hgdp1kgp-v2-SAS-k10      SAS 0.03152799
    ## 5 hgdp1kgp-v2-AMR-k10      AMR 0.06315152

### Variants are ALIDs on GRCh38

Legacy OpenGWAS identified variants by rsID, with GRCh37 positions. The
new GWAS Stores identify them by **ALID**, `chromosome:position:A1:A2`
on GRCh38, with `A1` the effect allele
([ALIDs](https://opengwas.github.io/opengwasr/articles/opengwasr.html#alids)
explains them in full). Every Association has both `alid` and `rsid`.
opengwasr accepts either, translates rsIDs for you, and returns every
ALID an rsID matches rather than choosing one. `rs1421085`, for example,
matches two ALIDs in OGS-00011. `position` values are GRCh38, so they
differ from the ones in old outputs. PLINK References, usually GRCh37,
are matched by rsID.

``` r

og_variants_rsid(src, "rs1421085", releases = "OGS-00011")[, c("rsid", "alid")]
```

    ##        rsid            alid
    ## 1 rs1421085 16:53767042:A:G
    ## 2 rs1421085 16:53767042:C:T

### `beta` and `pval` are computed for you

The new API stores `z` and `se`, not `beta`. opengwasr adds `beta` =
`z * se` to every Association, and `pval` computed from the p-value the
API sends. When \|z\| is above about 38.5, `pval` underflows to 0, the
limit of R’s numbers, while `z` stays exact. Column names follow the new
API (`effect_allele`, `other_allele`, `chromosome`); TwoSampleMR maps
them to its own.

``` r

head(exposure[, c("rsid", "z", "se", "beta", "pval")], 2)
```

    ##         rsid         z          se        beta          pval
    ## 1 rs56094641 -36.49073 0.002016341 -0.07357775 1.555977e-291
    ## 2  rs6567160  23.12988 0.002337749  0.05407187 2.317582e-118
