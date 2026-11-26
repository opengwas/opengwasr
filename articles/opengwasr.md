# Getting started with opengwasr

opengwasr gives you OpenGWAS data in R: Analysis metadata, top hits,
Associations and LD. Every query function takes a **Source** as its
first argument, which says where the query is answered. This vignette
uses the API Source,
[`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md),
which queries the OpenGWAS API over the network. A Local Source reads
Store Releases downloaded to your own machine instead (see
[Sources](#sources)). Its verbs, arguments and columns are the same, but
it answers LD (clumping and LD matrices) only when you give it an LD
resource.

You never handle the API’s Jobs, polling, paging or variant encodings
yourself. Each call waits for its answer and returns a plain
`data.frame`. When a query can’t be answered exactly as you asked,
opengwasr stops with an error that says why. It never quietly gives you
a different answer.

> **Status.** Every chunk runs, replayed from responses recorded against
> the development Data API on 23 November 2026, except the ones marked
> illustrative, each with its reason: the planned `og_rho()` (it needs a
> Data API endpoint that doesn’t exist yet), the error you get with no
> key, the error for an ambiguous identifier (no identifier is ambiguous
> in today’s catalogue). `sources-local` opens the example Store
> Releases that come with opengwasr, and runs only where Python and
> ogquery are set up (`local_ready`, as in [Querying downloaded Store
> Releases](https://opengwas.github.io/opengwasr/articles/local-source.md));
> `sources-local-plink` also needs a PLINK binary (`plink_ready`).
> Elsewhere, as on CRAN, their code is shown unevaluated.
>
> *Developer note:* The API responses in this vignette were recorded
> against the development Data API, directly, with a developer key (see
> `vignettes/recordings.R`). Once the OpenGWAS Gateway is live (#18),
> re-record through it, so the vignettes show the access users will
> have.

The examples follow one Mendelian randomization question: does body mass
index (BMI) affect coronary artery disease (CAD)?

``` r

library(opengwasr)
```

## Sources

A **Source** says two things: where Associations come from, and which
**LD resource** answers LD for them (clumping, LD matrices and LD
proxies). There are two kinds of Source:

- [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md),
  the **API Source**, reads the OpenGWAS API over the network.
- `og_local(gwas = ...)`, a **Local Source**, reads Store Releases you
  have downloaded (see [Querying downloaded Store
  Releases](https://opengwas.github.io/opengwasr/articles/local-source.md)).
  `og_local(bfile = ...)` alone is a Local Source with LD only, from
  your own PLINK Reference.

An LD resource is one of three kinds, and each can be on the API or on
your machine:

| LD resource | On the API | On your machine |
|----|----|----|
| **LD Store**: the precomputed LD of one reference panel | `og_api(ld_store = "EUR")`, an LD Store’s name or an ancestry code. Available now | `og_local(gwas = ..., ld_store = path)`. Coming, when opengwasr can read downloaded LD Stores |
| **Genotype Store**: genotypes of one reference panel, from which LD is computed when asked | `og_api(geno_store = ...)`. Coming, when the API serves Genotype Stores | `og_local(gwas = ..., geno_store = path)`. Coming, when opengwasr can read downloaded Genotype Stores |
| **PLINK Reference**: your own PLINK binary fileset (`.bed`, `.bim`, `.fam`) | None: the API doesn’t read your files | `og_local(gwas = ..., bfile = prefix)`, or `og_local(bfile = prefix)` alone for LD only, and `plink =` the PLINK 1.9 or 2 binary if opengwasr can’t find it. Available now |
| None given | [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md): each Analysis’s Assigned Ancestry chooses its LD Store | `og_local(gwas = ...)`: anything that needs LD stops and asks you to give an LD resource |

A form that is coming is refused with an error that says so, rather than
quietly doing something else.

``` r

src <- og_api()                         # LD: each Analysis's Assigned Ancestry chooses
src_eur <- og_api(ld_store = "EUR")     # LD: the EUR LD Store
src_eur
```

    ## <og_api_source>
    ## • URL: http://localhost:6433
    ## • Signed in with: OPENGWAS_DEV_KEY (as opengwasr-dev)
    ## • Timeout: 3600 s
    ## • Jobs in flight: at most 4
    ## • LD resource: LD Store "EUR"

With Store Releases you have downloaded, such as
`og_local(gwas = "/data/opengwas/OGS-00011", bfile = "/data/ld/1kg/EUR")`.
Here, the small example Store Releases and PLINK Reference that come
with opengwasr:

``` r

stores <- system.file("stores", package = "opengwasr")
local <- og_local(gwas = file.path(stores, "dense"))   # LD: none, so anything needing LD asks for one
og_ld_resource(local)
```

    ## NULL

``` r

bfile <- file.path(system.file("plink", package = "opengwasr"), "ref")
local <- og_local(gwas = file.path(stores, "dense"), bfile = bfile)   # LD: your PLINK Reference
local
```

    ## <og_local_source>
    ## • Store Releases: 1
    ## • s/r: /home/runner/work/_temp/Library/opengwasr/stores/dense/r
    ## • LD resource: PLINK Reference /home/runner/work/_temp/Library/opengwasr/plink/ref
    ##   PLINK 1.90b6.21: /home/runner/work/_temp/Library/genetics.binaRies/bin/plink

Every verb that needs LD
([`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
when it clumps,
[`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md),
[`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md),
[`og_ld_proxies()`](https://opengwas.github.io/opengwasr/reference/og_ld_proxies.md)
and
[`og_ld_reflookup()`](https://opengwas.github.io/opengwasr/reference/og_ld_reflookup.md))
uses the Source’s LD resource; no verb takes an LD argument. For other
LD in one call, make a Source like yours with other LD: `inherit =`
keeps everything you don’t give, such as
`og_ld_matrix(og_api(inherit = src_eur, ld_store = "AFR"), variants)`,
or `og_local(inherit = local, bfile = ...)`.

A Source’s LD lives where the Source does. To take Associations from one
place and LD from another, fetch with one Source and clump with the
other, such as
`og_clump(og_local(bfile = ...), og_tophits(src, ids, clump = FALSE, limit = 10000), r2 = 0.001, kb = 10000)`
for top hits from the API clumped with your PLINK Reference
(`limit = 10000` fetches them all; the default, 1,000, would cut them
off before clumping) ([Associations from one place, LD from
another](https://opengwas.github.io/opengwasr/articles/ld.html#mixing)).
[LD
operations](https://opengwas.github.io/opengwasr/articles/ld.html#ld-resource)
explains choosing LD in full.

## Credentials

### Your OpenGWAS token

opengwasr signs in to the OpenGWAS Gateway with the same token ieugwasr
uses. Get it from your [OpenGWAS
profile](https://api.opengwas.io/profile/) and add it to your
`.Renviron` file, which you can open with `usethis::edit_r_environ()`:

``` text
OPENGWAS_JWT=<your token>
```

Restart R so the new setting is read. opengwasr never prints your token,
and never writes it into errors or logs.

> **Not yet available.** The Gateway doesn’t serve the new OpenGWAS API
> yet. Until it does, only the developer access below works.

### Developer access (development and tests only)

Before the Gateway is live, opengwasr’s developers reach the Data API
directly with a developer key. This is for developing opengwasr, running
its live tests and recording these vignettes. Don’t use it for analyses.

``` text
OPENGWAS_URL=http://localhost:6433
OPENGWAS_DEV_KEY=<developer key>
OPENGWAS_DEV_USER=opengwasr-dev
```

When `OPENGWAS_DEV_KEY` is set,
[`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md)
sends it to `OPENGWAS_URL`, naming `OPENGWAS_DEV_USER` (default
`opengwasr-dev`) as the user. Keep these settings in a `.Renviron` that
is never committed.

### Creating the API Source

``` r

src <- og_api()
src
```

    ## <og_api_source>
    ## • URL: http://localhost:6433
    ## • Signed in with: OPENGWAS_DEV_KEY (as opengwasr-dev)
    ## • Timeout: 3600 s
    ## • Jobs in flight: at most 4

[`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md)
takes `url`, `timeout` (seconds to wait for any one call before giving
up) and `max_in_flight` (how many Jobs one call may run at once). Each
defaults to an option (`opengwasr.url`, `opengwasr.timeout`,
`opengwasr.max_in_flight`), so you can set it once for a session. A
fourth option, `opengwasr.progress`, turns the progress display off when
`FALSE`; it is on in interactive sessions by default.

``` r

options(opengwasr.timeout = 600)
src <- og_api()
```

Without a key,
[`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md)
stops straight away:

This is illustrative, not recorded: the vignettes always run with a key
set.

``` text
# illustrative
Error in `og_api()`:
! No OpenGWAS key found.
ℹ Set `OPENGWAS_DEV_KEY` in your .Renviron to your developer key, then restart R.
```

That error has class `opengwasr_auth`. A key the API doesn’t accept is
reported on the first query that needs one, such as
[`og_user()`](https://opengwas.github.io/opengwasr/reference/og_status.md)
([`og_status()`](https://opengwas.github.io/opengwasr/reference/og_status.md)
doesn’t, because the API serves it without a key), with the API’s own
message, `Invalid bearer token`, and class `opengwasr_unauthorized`.
Printing the Source shows the setting that supplied the key, never the
key.

### Checking your access

[`og_status()`](https://opengwas.github.io/opengwasr/reference/og_status.md)
reports whether the API is up. The API serves `/api/status` without a
key, so
[`og_status()`](https://opengwas.github.io/opengwasr/reference/og_status.md)
works even with a wrong one (building the Source with
[`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md)
still needs a key to be set).
[`og_user()`](https://opengwas.github.io/opengwasr/reference/og_status.md)
reports who you are signed in as, and your allowance, and is the call
that checks your key.

``` r

og_status(src)
```

    ## ── OpenGWAS API ──
    ## ✔ Ready
    ## • Most Associations one Job returns: 100,000
    ## • Unread Job results kept for: 1 h
    ## • Jobs one user may have queued: 100

``` r

og_user(src)
```

    ## ── Signed in ──
    ## • User: opengwasr-dev
    ## • Tier: gateway
    ## • Allowance: 1,000
    ## • Jobs running at once: 10

Both return a list, so scripts can read the fields. You can have up to
10 Jobs running and up to 100 queued at any time; opengwasr keeps within
both.

## Finding Analyses

An **Analysis** is one statistical analysis of one trait. Each lives in
a **GWAS Store**, a published collection of harmonised GWAS summary
statistics with an `OGS-*` id, and is named `<store_id>:<analysis_id>`,
such as `OGS-00011:GCST90029007`.

### By identifier

``` r

info <- og_analyses(src, c("OGS-00011:GCST90029007", "OGS-00011:GCST003116"))
info[, c("id", "analysis_label", "assigned_ancestry", "sample_size",
         "outcome_type", "stored_effect_scale")]
```

    ##                       id          analysis_label assigned_ancestry sample_size
    ## 1 OGS-00011:GCST90029007         Body mass index               EUR      532396
    ## 2   OGS-00011:GCST003116 Coronary artery disease               EUR      187599
    ##   outcome_type stored_effect_scale
    ## 1   continuous                  sd
    ## 2       binary              log_or

Analyses come back in the order you gave them, one per identifier. The
metadata includes the trait (`analysis_label`, `trait_ontology_id`), the
Assigned Ancestry, sample sizes (`sample_size`, `sample_size_kind`,
`n_cases`, `n_controls`), how effects are scaled (`stored_effect_scale`,
`original_sd`, `original_sd_method`), the publication and the licence.

### Legacy OpenGWAS identifiers

Identifiers from **Legacy OpenGWAS**, the OpenGWAS service ieugwasr
calls, are accepted where they name exactly one Analysis. The
`requested_id` column shows what you asked for:

``` r

og_analyses(src, c("ukb-b-19953", "ebi-a-GCST003116"))[, c("requested_id", "id", "analysis_label")]
```

    ##       requested_id                    id          analysis_label
    ## 1      ukb-b-19953 OGS-00009:ukb-b-19953   Body mass index (BMI)
    ## 2 ebi-a-GCST003116  OGS-00011:GCST003116 Coronary artery disease

An identifier that names no Analysis is an error, of class
`opengwasr_analysis_not_found`. For most identifiers the API says only
that it found nothing, so the message just names the identifier; where
the API gives a reason, as it does for a Legacy OpenGWAS family it
doesn’t hold, the message quotes it and `e$api_reasons` keeps it. Some
Legacy OpenGWAS identifier families aren’t held by any GWAS Store:

``` r

og_analyses(src, "ieu-a-2")
```

    ## Error in `og_analyses()`:
    ## ! Identifiers that name no Analysis:
    ## ✖ ieu-a-2: OpenGWAS batch ieu-a is not held by any Store

Some legacy identifiers now point to a later release of the same
analysis. For example, `finn-b-I9_CHD` points to FinnGen release 13,
`OGS-00017:finngen-r13-I9_CHD`. opengwasr answers with the successor,
and warns you, because its numbers differ from the ones Legacy OpenGWAS
served:

``` r

og_analyses(src, "finn-b-I9_CHD")[, c("requested_id", "id", "analysis_label")]
```

    ## Warning in og_analyses(src, "finn-b-I9_CHD"): `finn-b-I9_CHD` now points to a later release of that analysis, "OGS-00017:finngen-r13-I9_CHD", whose numbers differ from the ones Legacy OpenGWAS served.
    ## ℹ Ask for "OGS-00017:finngen-r13-I9_CHD" by name to silence this warning.

    ##    requested_id                           id                     analysis_label
    ## 1 finn-b-I9_CHD OGS-00017:finngen-r13-I9_CHD Major coronary heart disease event

The warning has class `opengwasr_legacy_successor`, and carries the
identifier you gave in `w$requested_id` and the successor in `w$id`.
Catch it with [`tryCatch()`](https://rdrr.io/r/base/conditions.html) to
stop instead, or muffle it with
[`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html). Every
query function that takes Analyses does the same.

If an identifier could name more than one Analysis, opengwasr stops and
lists them, with class `opengwasr_ambiguous_analysis`. No identifier is
ambiguous in today’s catalogue. This is what the error would look like,
for a hypothetical bare `GCST90000001` held by two GWAS Stores
(illustrative, not recorded, because there is nothing to provoke it
with):

``` text
# illustrative
Error in `og_analyses()`:
! `GCST90000001` names more than one Analysis.
ℹ Use one of: "OGS-00011:GCST90000001", "OGS-00012:GCST90000001".
```

### Pinning a Store Release

Each GWAS Store is published as a series of immutable **Store
Releases**. Without `releases`, opengwasr uses the one Store Release the
API serves for each GWAS Store, and sends that choice to the API rather
than leaving it to the API’s default of every configured Store Release.
You can pin Store Releases with `releases`, which every query function
accepts. Give a GWAS Store id, such as `"OGS-00011"`, to use the one
Store Release it serves (if any GWAS Store ever serves several, the API
doesn’t say which is latest, so opengwasr stops with
`opengwasr_release_selection_required` and lists them, whether you named
that GWAS Store or left `releases` out), or `"<store_id>/<release_id>"`
to pin a particular one:

``` r

og_analyses(src, "OGS-00011:GCST90029007",
            releases = "OGS-00011/OGS-00011-completed")[, c("id", "release_id")]
```

    ##                       id          release_id
    ## 1 OGS-00011:GCST90029007 OGS-00011-completed

To reproduce an analysis later, record the `store_id` and `release_id`
columns that come back with every Association (see
[Provenance](#provenance)).

### Browsing the catalogue

Without identifiers,
[`og_analyses()`](https://opengwas.github.io/opengwasr/reference/og_analyses.md)
reads the whole catalogue, page by page, with a progress bar. The whole
catalogue is tens of thousands of Analyses, so this example reads one
GWAS Store, the UK Biobank analyses of `OGS-00009`, by pinning it with
`releases`. Leave `releases` out for everything:

``` r

catalogue <- og_analyses(src, releases = "OGS-00009")
nrow(catalogue)
```

    ## [1] 2514

``` r

bmi <- subset(catalogue, grepl("body mass index", analysis_label, ignore.case = TRUE) &
                assigned_ancestry %in% "EUR" & outcome_type == "continuous")
head(bmi[, c("id", "analysis_label", "sample_size")], 3)
```

    ##                         id        analysis_label sample_size
    ## 1345 OGS-00009:ukb-b-19953 Body mass index (BMI)      461460
    ## 1479  OGS-00009:ukb-b-2303 Body mass index (BMI)      454884

## Variants are named by ALID

opengwasr names every variant by its **ALID**, as the GWAS Stores and LD
Stores do. An ALID is `chromosome:position:A1:A2`, such as
`16:53767042:C:T`:

- `chromosome` is `1` to `22`, `X`, `Y` or `MT`, with no `chr` prefix.
  Other contig names (such as GRCh38 alt contigs) are kept as given, as
  the GWAS Stores keep them.
- `position` is on GRCh38, one-based, as in a VCF file. An indel is
  left-aligned, and its position is that of the first base of its
  alleles.
- `A1` and `A2` are the two alleles, upper case. When they are the same
  length (an SNV, or a substitution of several bases), they are in
  alphabetical order: `C:T`, never `T:C`. When their lengths differ (an
  indel), the GRCh38 reference allele comes first: the insertion
  `5:75701728:A:AAAAT`, the deletion `1:827587:GGGGCC:G`. An insertion
  and a deletion of the same sequence at one position are therefore two
  different variants.
- `A1` is the effect allele. Every Association’s `effect_allele` is the
  `A1` of its `alid`, so `beta`, `z` and `eaf` always refer to `A1`, and
  the sign of an LD r refers to it too.
- An ALID names exactly two alleles. A site with more alleles appears as
  several ALIDs at the same position, one per pair of alleles a GWAS
  Store holds, and one rsID can name several of them.

ALIDs are the same in every GWAS Store and LD Store on GRCh38, so the
same variant joins across Analyses, Store Releases and LD. Use
[`og_variants_rsid()`](https://opengwas.github.io/opengwasr/reference/og_variants_rsid.md)
and
[`og_variants_chrpos()`](https://opengwas.github.io/opengwasr/reference/og_variants_rsid.md)
([below](#phewas-regions-variants)) to find the ALIDs for rsIDs or
positions.

### ALIDs for your own data

To join your own summary statistics to OpenGWAS data, give them ALIDs
with
[`og_alid()`](https://opengwas.github.io/opengwasr/reference/og_alid.md).
It takes a data frame with a chromosome, a GRCh38 position and two
alleles, and returns it with an `alid` column added and the alleles
oriented to match. Here `my_gwas` is a small made-up data frame standing
in for your own data:

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
```

    ## ℹ Swapped the alleles of 1 variant to match its ALID: `beta` negated, `eaf`
    ##   replaced by 1 - `eaf`.
    ## ! 2 indels have no ALID, because no allele is declared the reference. Name it
    ##   with `reference`.

    ##   chromosome position effect_allele other_allele  eaf   beta            alid
    ## 1         16 53767042             C            T 0.42  0.076 16:53767042:C:T
    ## 2         18 60161902             C            T 0.24  0.055 18:60161902:C:T
    ## 3          5 75701728         AAAAT            A 0.13 -0.025            <NA>
    ## 4          1   827587             G       GGGGCC 0.04  0.120            <NA>

- **SNVs and substitutions are always oriented.** Their alleles are
  sorted, so `A1` comes first. The first variant’s effect allele `T`
  isn’t `A1`, so its alleles are swapped, `beta` is negated (and `z`, if
  there is one), and `eaf` becomes 1 − `eaf`. The second variant’s
  `chr18` and `c` are normalised to `18` and `C`.
- **Indels need their reference allele declared.** An indel’s ALID puts
  the GRCh38 reference allele first, and the alleles alone can’t say
  which one that is: at a repeat, an insertion and a deletion can have
  the same two alleles. Without `reference`, an indel’s `alid` is `NA`,
  and a message gives the count. This is the same rule the GWAS Stores
  were built by, so an ALID
  [`og_alid()`](https://opengwas.github.io/opengwasr/reference/og_alid.md)
  makes always matches theirs.

If your data came from a VCF, or otherwise says which allele is the
reference, pass `reference`: `"other_allele"` or `"effect_allele"` when
one of those columns is always the reference, or the name of a column
holding the reference allele:

``` r

og_alid(my_gwas, reference = "other_allele")
```

    ## ℹ Swapped the alleles of 3 variants to match their ALIDs: `beta` negated, `eaf`
    ##   replaced by 1 - `eaf`.

    ##   chromosome position effect_allele other_allele  eaf   beta               alid
    ## 1         16 53767042             C            T 0.42  0.076    16:53767042:C:T
    ## 2         18 60161902             C            T 0.24  0.055    18:60161902:C:T
    ## 3          5 75701728             A        AAAAT 0.87  0.025 5:75701728:A:AAAAT
    ## 4          1   827587        GGGGCC            G 0.96 -0.120  1:827587:GGGGCC:G

`reference` is used only for indels.
[`og_alid()`](https://opengwas.github.io/opengwasr/reference/og_alid.md)
assumes two things it can’t check without the reference genome: alleles
are on the forward strand, and indels are already left-aligned, as in a
normalised VCF. The column names are arguments (`chromosome`,
`position`, `effect_allele`, `other_allele`, `eaf`, `beta`, `z`), with
the defaults shown. `eaf`, `beta` and `z` are adjusted when the data has
them.

## Top hits

[`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
returns each Analysis’s Associations at or below a p-value `threshold`
(default `5e-8`), strongest first. It **clumps them by default**,
keeping one variant per independent signal; the [LD
operations](https://opengwas.github.io/opengwasr/articles/ld.html#clumping)
vignette explains how. Here, `clump = FALSE` shows every top hit:

``` r

top <- og_tophits(src, "OGS-00011:GCST003116", clump = FALSE, limit = 5)
```

    ## Warning in og_tophits(src, "OGS-00011:GCST003116", clump = FALSE, limit = 5): OGS-00011:GCST003116 has more than 5 top hits at p <= 5e-08; only the strongest 5 were returned.
    ## ℹ Raise `limit` (at most 10,000) for more.

``` r

head(top[, c("rsid", "alid", "beta", "se", "pval", "association_status")], 4)
```

    ##         rsid           alid       beta          se         pval
    ## 1  rs2891168 9:22098620:A:G -0.1933591 0.009185791 2.296241e-98
    ## 2  rs4977574 9:22098575:A:G -0.1929106 0.009185791 6.425124e-98
    ## 3 rs10738610 9:22123767:A:C -0.1936836 0.009292603 1.772656e-96
    ## 4 rs10757275 9:22106226:A:G  0.1930436 0.009269714 2.558808e-96
    ##   association_status
    ## 1           observed
    ## 2           observed
    ## 3           observed
    ## 4           observed

- `limit` (default 1,000, here 5) caps the top hits returned per
  Analysis. When it cuts any off, you get the warning above.
- `threshold` must be one the GWAS Store indexes. Each GWAS Store
  indexes `5e-8`, `5e-6` and `5e-4`, and the API refuses anything
  looser:

``` r

og_tophits(src, "OGS-00011:GCST003116", threshold = 1e-3, clump = FALSE, limit = 5)
```

    ## Error in `og_tophits()`:
    ## ! threshold must be at most 0.0005, the loosest p-value the Top-Hit Index of OGS-00011 holds

That error has class `opengwasr_unsupported_threshold`, a kind of
`opengwasr_refusal`, and carries the thresholds the GWAS Store indexes,
as the API listed them, in `e$indexed_thresholds`.

## Associations

[`og_associations()`](https://opengwas.github.io/opengwasr/reference/og_associations.md)
returns the Associations for a set of variants in a set of Analyses: for
example, the outcome data for your instruments. Variants can be rsIDs or
[ALIDs](#alids), mixed.

``` r

assoc <- og_associations(
  src,
  variants = c("rs1421085", "16:53767042:C:T", "rs6567160", "rs34159589", "rs999999999"),
  analyses = c("OGS-00011:GCST90029007", "OGS-00011:GCST003116")
)
```

    ## Warning in og_associations(src, variants = c("rs1421085", "16:53767042:C:T", :
    ## 1 rsID is in none of these Analyses' GWAS Stores, so it has no Associations:
    ## rs999999999.

``` r

assoc[, c("id", "rsid", "alid", "beta", "se", "pval", "association_status")]
```

    ##                       id       rsid               alid         beta          se
    ## 1 OGS-00011:GCST90029007 rs34159589 5:75701728:A:AAAAT -0.025441924 0.002000809
    ## 2 OGS-00011:GCST90029007  rs1421085    16:53767042:C:T  0.075834384 0.001968384
    ## 3 OGS-00011:GCST90029007  rs6567160    18:60161902:C:T  0.054658253 0.002292633
    ## 4   OGS-00011:GCST003116 rs34159589 5:75701728:A:AAAAT -0.003017932 0.009841919
    ## 5   OGS-00011:GCST003116  rs1421085    16:53767042:C:T  0.029665306 0.009628296
    ## 6   OGS-00011:GCST003116  rs6567160    18:60161902:C:T  0.057734668 0.010559082
    ##            pval association_status
    ## 1  4.830139e-37            imputed
    ## 2  0.000000e+00           observed
    ## 3 1.260915e-125           observed
    ## 4  7.591169e-01            imputed
    ## 5  2.062688e-03           observed
    ## 6  4.557239e-08           observed

- An rsID can name several ALIDs, and opengwasr asks for every one of
  them. It never picks one for you. `rs1421085` names `16:53767042:A:G`
  and `16:53767042:C:T`; these Analyses hold only the second, which you
  also asked for by its ALID, so it appears once per Analysis.
  `rs34159589` names `5:75701728:A:AAAAT` and
  `5:75701728:AAAAT:AAAATAAAT`; again only the first has Associations
  here.
- Every match is returned: if an Analysis held Associations for both
  ALIDs of an rsID, you would get both.
- Every Association has an `alid`, so you can always join by it. It has
  an `rsid` too, except where a GWAS Store holds the variant with no
  rsID: `rsid` is then `NA`.
- An rsID that no GWAS Store holds is reported in a warning (class
  `opengwasr_unresolved_variants`, with the rsIDs in its `ids` field),
  never dropped quietly.
- **Imputed Associations are included**
  (`association_status == "imputed"`). Pass `observed_only = TRUE` to
  leave them out. See [Imputed Associations](#imputed-associations).

There is no LD-proxy option. Reference-Completed Store Releases already
hold an Imputed Association for most variants a source didn’t report.

## PheWAS, regions and variant lookups

[`og_phewas()`](https://opengwas.github.io/opengwasr/reference/og_phewas.md)
returns the Associations of one or more variants (rsIDs and ALIDs)
across Analyses, at p ≤ `threshold` (default `1e-8`). By default it
searches every Analysis of every GWAS Store; `releases` narrows the
search, and `analyses` names the Analyses to look in. An rsID is
resolved to every ALID it names, and opengwasr runs one PheWAS per ALID
and combines them in the order you gave the variants. Here, the
Associations of `rs1421085` at p ≤ 1e-50 in one GWAS Store:

``` r

ph <- og_phewas(src, "rs1421085", threshold = 1e-50, releases = "OGS-00011")
nrow(ph)
```

    ## [1] 27

``` r

head(ph[, c("id", "analysis_label", "beta", "pval")], 4)
```

    ##                       id                              analysis_label
    ## 1   OGS-00011:GCST002047 Educational attainment (college completion)
    ## 2   OGS-00011:GCST004461             Low-grade serous ovarian cancer
    ## 3 OGS-00011:GCST90025994                             Body mass index
    ## 4 OGS-00011:GCST90029007                             Body mass index
    ##          beta          pval
    ## 1 -0.41119424  0.000000e+00
    ## 2 -3.22208393  0.000000e+00
    ## 3  0.08162382 2.766768e-322
    ## 4  0.07583438  0.000000e+00

`pval` is 0 where it underflows; `z` stays exact. `limit` (default
1,000, at most 10,000) applies to each ALID, not to all the variants
together. When it cuts a PheWAS off you get a warning, as for top hits.

[`og_range()`](https://opengwas.github.io/opengwasr/reference/og_range.md)
returns every Association in a stretch of one chromosome. Positions are
on GRCh38, one-based and inclusive. A range over 10 Mb, or over more
than 10 Analyses, is split into several Jobs for you:

``` r

locus <- og_range(src, "OGS-00011:GCST003116", chromosome = "9",
                  start = 22090000, end = 22130000)
table(locus$association_status)
```

    ## 
    ##  imputed observed 
    ##       19       85

[`og_variants_rsid()`](https://opengwas.github.io/opengwasr/reference/og_variants_rsid.md)
and
[`og_variants_chrpos()`](https://opengwas.github.io/opengwasr/reference/og_variants_rsid.md)
translate rsIDs and GRCh38 positions into ALIDs, using the GWAS Stores’
own variant tables. You get each ALID once for every GWAS Store that
holds it:

``` r

og_variants_rsid(src, "rs34159589", releases = "OGS-00011")
```

    ##                         alid chromosome effect_allele other_allele position
    ## 1         5:75701728:A:AAAAT          5             A        AAAAT 75701728
    ## 2 5:75701728:AAAAT:AAAATAAAT          5         AAAAT    AAAATAAAT 75701728
    ##            release_id       rsid  store_id
    ## 1 OGS-00011-completed rs34159589 OGS-00011
    ## 2 OGS-00011-completed rs34159589 OGS-00011

``` r

og_variants_chrpos(src, "16:53767042", releases = "OGS-00011")
```

    ##              alid chromosome effect_allele other_allele position
    ## 1 16:53767042:A:G         16             A            G 53767042
    ## 2 16:53767042:C:T         16             C            T 53767042
    ##            release_id      rsid  store_id
    ## 1 OGS-00011-completed rs1421085 OGS-00011
    ## 2 OGS-00011-completed rs1421085 OGS-00011

Every match is returned: one position can hold several ALIDs, and one
rsID can name several.

## Correlation between Analyses: the Rho Matrix

> **Planned.** `og_rho()` needs a Data API endpoint that doesn’t exist
> yet.

Analyses in one GWAS Store often share participants, and their traits
can be correlated, so their z-scores are correlated even at variants
with no effect on either trait. **Rho** is that correlation under the
null: the traits’ phenotypic correlation times the Analyses’ proportion
of sample overlap. A GWAS Store with a Dense Layout can carry a **Rho
Matrix**: Rho for every pair of its Analyses, estimated by opengwasdb
from their Observed null z-scores. Methods that combine Analyses and
must allow for sample overlap, such as multivariable MR or multi-trait
analyses, can use it rather than estimate it again.

`og_rho()` will return the whole matrix for a GWAS Store, or the part
for the Analyses you choose:

``` r

rho_all <- og_rho(src, store = "OGS-00009")
rho <- og_rho(src, analyses = c("OGS-00009:ukb-b-19953", "OGS-00009:ukb-b-2303", "OGS-00009:ukb-b-11842"))
```

- It returns a symmetric matrix named by Analysis `id`, with 1 on the
  diagonal. A pair with too few shared null variants to estimate Rho is
  `NA`.
- `og_provenance(rho)$n_null` is the matching matrix of how many null
  variants supported each pair, and `og_provenance(rho)` also gives the
  settings that produced it, such as the null z threshold.
- Give `store` for the whole matrix, or `analyses` for part of it. Rho
  is estimated within one GWAS Store, so all the `analyses` must share
  one.
- A GWAS Store without a Rho Matrix is refused, with class
  `opengwasr_rho_unavailable`, a kind of `opengwasr_refusal`.

## What the columns mean

Every function that returns Associations
([`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md),
[`og_associations()`](https://opengwas.github.io/opengwasr/reference/og_associations.md),
[`og_phewas()`](https://opengwas.github.io/opengwasr/reference/og_phewas.md),
[`og_range()`](https://opengwas.github.io/opengwasr/reference/og_range.md))
returns the same columns:

| Column | Meaning |
|----|----|
| `id` | The Analysis, as `<store_id>:<analysis_id>` |
| `store_id`, `analysis_id`, `analysis_label` | Its GWAS Store, its identifier within the GWAS Store, and its trait |
| `alid`, `rsid`, `chromosome`, `position` | The variant, on GRCh38 (see [ALIDs](#alids)). `rsid` is `NA` for a variant with no rsID |
| `effect_allele`, `other_allele`, `eaf` | Its alleles (`A1` and `A2` of the ALID), and the effect allele’s frequency |
| `beta`, `se` | The effect of one copy of the effect allele, and its standard error |
| `z`, `pval` | The z-score and the p-value |
| `effect_scale` | The scale `beta` and `se` are on: `"sd"`, `"log_or"`, or `"original"` when you ask for `original_units = TRUE` |
| `n`, `sample_size_kind`, `ncase`, `ncontrol` | Sample size, and what kind of count it is |
| `association_status` | `"observed"` or `"imputed"` |
| `release_id` | The Store Release the Association came from |

### Effects and p-values

`z` and `se` are the GWAS Store’s own. opengwasr adds `beta` = `z * se`,
and computes `pval` from the p-value the API sends, so it matches the
API exactly. All of them are computed the same way everywhere.

``` r

top <- og_tophits(src, "OGS-00011:GCST90029007", clump = FALSE, limit = 3)
```

    ## Warning in og_tophits(src, "OGS-00011:GCST90029007", clump = FALSE, limit = 3): OGS-00011:GCST90029007 has more than 3 top hits at p <= 5e-08; only the strongest 3 were returned.
    ## ℹ Raise `limit` (at most 10,000) for more.

``` r

top[, c("rsid", "z", "se", "beta", "pval")]
```

    ##         rsid         z          se        beta pval
    ## 1 rs56094641 -38.53421 0.001968384 -0.07585011    0
    ## 2 rs11642015 -38.53265 0.001968384 -0.07584705    0
    ## 3 rs62048402  38.52727 0.001968384  0.07583645    0

When \|z\| is above about 38.5, the p-value is smaller than R can
represent (about 1e-323), so `pval` underflows to 0. `z` stays exact, so
rank or filter the strongest signals by `abs(z)`, not by `pval`.

### Effect scale and original units

The `effect_scale` column says what `beta` and `se` mean:

- `"sd"`: per standard deviation of the trait. Continuous traits are
  stored this way, so effects are comparable across Analyses.
- `"log_or"`: log odds, for binary traits.

Some binary Analyses are stored per SD rather than as log odds.
`effect_scale` says which, so check it rather than assuming from the
trait.

For a continuous trait in its original units, pass
`original_units = TRUE`. opengwasr multiplies `beta` and `se` by the
Analysis’s recorded phenotype SD (`original_sd` in
[`og_analyses()`](https://opengwas.github.io/opengwasr/reference/og_analyses.md)),
and `effect_scale` becomes `"original"`:

``` r

per_sd <- og_tophits(src, "OGS-00011:GCST90624700", clump = FALSE, limit = 5)
original <- og_tophits(src, "OGS-00011:GCST90624700", clump = FALSE, limit = 5,
                       original_units = TRUE)
head(per_sd[, c("rsid", "beta", "se", "effect_scale")], 3)
```

    ##         rsid        beta         se effect_scale
    ## 1  rs1421085  0.09583174 0.00312233           sd
    ## 2 rs55872725 -0.09583174 0.00312233           sd
    ## 3 rs11642015 -0.09581344 0.00312233           sd

``` r

head(original[, c("rsid", "beta", "se", "effect_scale")], 3)
```

    ##         rsid       beta         se effect_scale
    ## 1  rs1421085  0.4667140 0.01520618     original
    ## 2 rs55872725 -0.4667140 0.01520618     original
    ## 3 rs11642015 -0.4666249 0.01520618     original

Here `original_sd` is 4.87 kg/m², estimated from the source’s own effect
sizes and allele frequencies
(`original_sd_method == "estimated_from_source_maf"`). Log-odds effects
are returned unchanged, since log odds are their original units. When an
Analysis stored per SD isn’t a continuous trait with a recorded
phenotype SD, opengwasr refuses rather than guess. That includes the
binary Analyses stored per SD (the 1,846 binary `ukb-b-*` traits), which
have no phenotype SD to rescale by:

``` r

og_tophits(src, "OGS-00009:ukb-b-19953", clump = FALSE, original_units = TRUE)
```

    ## Error in `og_tophits()`:
    ## ! Can't give this Analysis in original units.
    ## ✖ `OGS-00009:ukb-b-19953`: it has no recorded phenotype SD (`original_sd`).
    ## ℹ Use `original_units = FALSE` (the default) for per-SD effects.

That error has class `opengwasr_original_units_unavailable`, a kind of
`opengwasr_refusal`. Its `ids` field lists the Analyses it refused, and
`reason` says why for each: `"binary_per_sd"`, `"no_original_sd"` or
`"unknown_trait_type"` (the trait isn’t recorded as continuous). It is
raised before the query runs. Asking for several Analyses refuses them
all if any can’t be rescaled.

### Sample size

[`og_analyses()`](https://opengwas.github.io/opengwasr/reference/og_analyses.md)
returns the API’s own metadata names (`sample_size`, `n_cases`,
`n_controls`). Associations carry the names MR methods expect: `n`,
`ncase` and `ncontrol`. `n` is the per-Association sample size where the
GWAS Store holds one, and otherwise the Analysis’s `sample_size`. Some
GWAS Stores, such as the molecular-trait one, send an `n` on each
Association; most don’t, and then every Association has the Analysis’s
`sample_size`. `sample_size_kind`, `ncase` and `ncontrol` always
describe the Analysis’s own count, not where a row’s `n` came from.
`sample_size_kind` says what kind of count that is, so an effective N is
never mistaken for a count of participants:

- `"total"`: participants in the Analysis.
- `"case_control"`: cases plus controls. `ncase` and `ncontrol` give
  each.
- `"variant_level"`: N differs from variant to variant, as in many
  molecular-trait Analyses.

``` r

out <- og_tophits(src, c("OGS-00011:GCST003116", "OGS-00027:ENSG00000000419"),
                  clump = FALSE, limit = 5)
out[c(1, 6:8), c("id", "n", "sample_size_kind", "ncase", "ncontrol", "effect_scale")]
```

    ##                          id      n sample_size_kind ncase ncontrol effect_scale
    ## 1      OGS-00011:GCST003116 187599     case_control 61289   126310       log_or
    ## 6 OGS-00027:ENSG00000000419  30895    variant_level    NA       NA           sd
    ## 7 OGS-00027:ENSG00000000419  30694    variant_level    NA       NA           sd
    ## 8 OGS-00027:ENSG00000000419  30508    variant_level    NA       NA           sd

For the case-control Analysis every row has the Analysis’s `n`. For the
molecular one, `n` varies from variant to variant, and
`sample_size_kind` says `"variant_level"`.

### Imputed Associations

A Reference-Completed Store Release holds, beside the Associations its
source reported (Observed), an Imputed Association for most variants the
source didn’t report. opengwasr includes them by default, and labels
each Association in `association_status`. Here are BMI’s 1,000 strongest
top hits looked up in CAD:

``` r

bmi_top <- og_tophits(src, "OGS-00011:GCST90029007", clump = FALSE)
```

    ## Warning in og_tophits(src, "OGS-00011:GCST90029007", clump = FALSE): OGS-00011:GCST90029007 has more than 1,000 top hits at p <= 5e-08; only the strongest 1,000 were returned.
    ## ℹ Raise `limit` (at most 10,000) for more.

``` r

outcome <- og_associations(src, bmi_top$alid, "OGS-00011:GCST003116")
table(outcome$association_status)
```

    ## 
    ##  imputed observed 
    ##       61      938

``` r

observed <- og_associations(src, bmi_top$alid, "OGS-00011:GCST003116",
                            observed_only = TRUE)
nrow(observed)
```

    ## [1] 938

Including the Imputed Associations finds 999 of the 1,000 variants in
the outcome. With only the Observed ones, 938. Report how many of the
Associations you used were Imputed.

### Provenance

Every Association carries the `store_id` and `release_id` it came from,
and clumped data frames carry their clumping in columns too (see
[clumping
provenance](https://opengwas.github.io/opengwasr/articles/ld.html#clumping-provenance)).
These columns are the provenance that travels with each Association.

`og_provenance(x)` gives the details of the call that produced `x`,
which no single Association can hold:

- For a data frame from a query function: the call and its arguments,
  the Store Releases that answered, the Job ids, and the API’s counts,
  including whether `limit` cut anything off (`total`, `truncated`). For
  clumped top hits it adds how many top hits were considered, how many
  the LD Store didn’t hold (`absent`), and how many index variants
  resulted.
- For an LD matrix from
  [`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md):
  the LD used, the Job ids, and the variants the LD Store didn’t hold
  (`absent`).

It returns a list, and prints as below. It is attached to the data frame
a query function returns and describes exactly those rows. After you
combine data frames ([`rbind()`](https://rdrr.io/r/base/cbind.html),
[`merge()`](https://rdrr.io/r/base/merge.html)) or subset rows,
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md)
warns (class `opengwasr_provenance_stale`) and returns `NULL` when the
rows no longer match, and stops with class `opengwasr_no_provenance` if
the operation dropped the attribute altogether; keep it, or its counts,
before you combine data frames.

``` r

og_provenance(top)
```

    ## ── opengwasr provenance ─────────────────
    ## • Query: og_tophits(threshold = 5e-08, limit = 3, observed_only = FALSE, clump = FALSE, original_units = FALSE)
    ## • Store Releases: OGS-00011/OGS-00011-completed
    ## • Jobs: 60057995
    ## • Rows: 3 returned of 3 matching (truncated)
    ## • Retrieved: 2026-11-26 10:17:55 UTC

Record the Store Releases alongside your analysis. They identify exactly
which data answered.

## When a query can’t be answered

opengwasr never quietly returns a different answer from the one you
asked for. An option is either honoured or refused with an error saying
why. Every error is a classed condition, so scripts and packages can
handle particular failures.

When the API refuses a request, the error’s message is the API’s own,
and its class is the API’s error code with `opengwasr_` in front: the
API’s `unsupported_threshold` is `opengwasr_unsupported_threshold`, and
so on for every code it sends. The code itself is in `e$api_error`. The
API’s error guide lists its codes. These are the ones you are most
likely to meet:

| Class | When |
|----|----|
| `opengwasr_unauthorized` | The API didn’t accept your key |
| `opengwasr_release_not_found` | A Store Release you pinned isn’t served |
| `opengwasr_unsupported_threshold`, `opengwasr_request_too_large`, `opengwasr_ld_refusal` | The API can’t answer exactly what you asked. These also have class `opengwasr_refusal` |
| `opengwasr_user_queue_full`, `opengwasr_server_busy`, … | The API stayed busy until the call’s timeout. These also have class `opengwasr_busy` |
| `opengwasr_result_expired` | A Job’s result had already been read, or had expired ([Large queries and Jobs](https://opengwas.github.io/opengwasr/articles/large-queries-and-jobs.md)) |
| `opengwasr_invalid_request`, `opengwasr_invalid_query` | The API couldn’t accept the request as sent, such as a `threshold` above 1 |
| `opengwasr_api_error` | The API answered with an error but no code |

Identifiers are the exception. Every function that takes Analyses looks
them up first, and the API’s lookup lists the identifiers it can’t
resolve rather than refusing them. For those, opengwasr writes the
message, quoting the API’s reason where it gives one, but the class and
`e$api_error` are the API’s codes all the same, so a handler doesn’t
need to know which way the error came:

| Class | When |
|----|----|
| `opengwasr_analysis_not_found` | An identifier names no Analysis. A retired one also has class `opengwasr_analysis_retired` |
| `opengwasr_ambiguous_analysis` | An identifier names more than one Analysis |
| `opengwasr_release_not_found`, `opengwasr_release_selection_required` | A GWAS Store id in `releases` isn’t served, or a GWAS Store serves several Store Releases and you must pin one |

Elsewhere opengwasr writes its own message only where the API can’t
speak:

| Class | When |
|----|----|
| `opengwasr_auth` | No key, or no API URL, is set |
| `opengwasr_argument` | An argument is the wrong kind, such as `ids` that aren’t character |
| `opengwasr_original_units_unavailable` | `original_units = TRUE` can’t be honoured. Also `opengwasr_refusal` |
| `opengwasr_timeout`, `opengwasr_job_failed`, `opengwasr_interrupted` | A Job didn’t finish in time, failed (with the API’s reason as the message), or you interrupted the wait ([Large queries and Jobs](https://opengwas.github.io/opengwasr/articles/large-queries-and-jobs.md)) |
| `opengwasr_unreachable` | The API couldn’t be reached |

All of them also have class `opengwasr_error`. Catch one by its class:

``` r

info <- tryCatch(
  og_analyses(src, c("ukb-b-19953", "ieu-a-2")),
  opengwasr_analysis_not_found = function(e) {
    message("Skipping: ", paste(e$ids, collapse = ", "))
    og_analyses(src, setdiff(c("ukb-b-19953", "ieu-a-2"), e$ids))
  }
)
```

    ## Skipping: ieu-a-2

Each condition carries the details as fields, such as `e$ids` here, with
the API’s reason for each, where it gave one, in `e$api_reasons` (a
retired Analysis’s successor is in `e$superseded_by`). A condition
raised from an API error response carries the API’s error code and
message as `e$api_error` and `e$api_message`, the HTTP status as
`e$status`, the whole body as `e$api_body`, and every other field of the
body under its own name, such as `e$indexed_thresholds` above.

One warning is classed too: `opengwasr_legacy_successor`, when a legacy
identifier is answered by a later release of its analysis (see [Legacy
OpenGWAS identifiers](#legacy-ids)). It is a warning rather than an
error because opengwasr does answer, and says which Analysis answered.
It doesn’t have class `opengwasr_error`.
