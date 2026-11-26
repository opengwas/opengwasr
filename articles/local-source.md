# Querying downloaded Store Releases

A **Local Source** answers your queries from **Store Releases** you have
downloaded to your own machine, without calling the OpenGWAS API. It
suits large queries: thousands of variants in thousands of Analyses,
which the API Source would split into many Jobs. You create it with
`og_local(gwas)`, and then pass it where you passed
[`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md).
The verbs, arguments and columns are the same (see [Getting
started](https://opengwas.github.io/opengwasr/articles/opengwasr.md)).

This vignette follows one journey: set up Python, open your Store
Releases, run your script against them, clump what you fetched, and run
a TwoSampleMR pipeline on them.

> **Status.** The Local Source is being built. Today it sets up and
> checks Python, opens Store Releases, looks up Analyses and variants,
> and answers top hits, Associations, PheWAS and ranges, and clumps and
> reads LD matrices with a PLINK Reference. Those chunks run on the
> small example Store Releases that come with opengwasr, wherever Python
> and ogquery are set up where this vignette was built (on CRAN, which
> has no Python, the code is shown unevaluated). The chunks that don’t
> run, or need more, and why:
>
> - `local-python-hpc-shell`, `local-python-conda-shell` and
>   `local-python-conda`: they set up a Python environment on your
>   machine.
> - `local-translate` and `local-ld-api`: they call the OpenGWAS API,
>   and this vignette never touches the network.
> - `local-clumping-plink`, `local-plink-settings`, `local-og-clump`,
>   `local-ld-matrix` and `local-ld-inherit`: they use a PLINK
>   Reference, the small synthetic fileset that comes with opengwasr,
>   and need a PLINK binary too. They run wherever PLINK 1.9 or 2 is
>   found (`plink_ready`: genetics.binaRies, or `plink` or `plink2` on
>   the `PATH`), as in CI; without one their code is shown unevaluated.
> - `local-stored-clumps`: no Store Release has stored Clumps yet;
>   reading them comes in a later step (#39).
> - `local-twosamplemr`: it needs TwoSampleMR 0.8.0 (#36), which
>   opengwasr doesn’t depend on, and full-size Store Releases.
>
> The grey output blocks marked `# illustrative` are shaped the way
> opengwasr returns results, not recorded from a run.

``` r

library(opengwasr)
```

## What you need

- **Store Releases on disk.** Each GWAS Store you download is a
  directory, such as `/data/opengwas/OGS-00011`, holding one Store
  Release, such as `/data/opengwas/OGS-00011/OGS-00011-v2`. opengwasr
  doesn’t download them for you.
- **Python, with ogquery.** ogquery is the **Store Reader**, the library
  every consumer uses to read a Store Release. It is on PyPI and needs
  only numpy, zarr and numcodecs. It runs on Linux, macOS and Windows,
  with Python 3.12 to 3.14. opengwasr calls it through reticulate. You
  never use ogquery by name from R.
- **Store Release format 0.3.x.** ogquery reads any format 0.3.x. A
  0.2.x Store Release is refused, with its format named. The hosted
  Store Releases are being rebuilt at 0.3.0.

API users never need Python, reticulate or ogquery.

## Setting up Python

opengwasr looks for Python in this order, and your choice always wins
over the automatic setup:

1.  **`RETICULATE_PYTHON`**, if set, names the Python to use.
2.  **An environment you name to reticulate**: a conda environment or
    virtualenv
    ([`reticulate::use_condaenv()`](https://rstudio.github.io/reticulate/reference/use_python.html),
    [`reticulate::use_virtualenv()`](https://rstudio.github.io/reticulate/reference/use_python.html),
    or `RETICULATE_PYTHON_ENV`), an activated virtualenv
    (`VIRTUAL_ENV`), or a Python reticulate has already started in this
    R session.
3.  **Automatic setup.** Otherwise reticulate makes an environment with
    ogquery in it, using uv
    (`reticulate::py_require("ogquery>=0.1.0,<1")`).

opengwasr reads **ogquery 0.1.0 or later, before 1.0**
(`"ogquery>=0.1.0,<1"`, as pip writes it), and every install below asks
for that range. Any other ogquery is refused before anything is read
(`e$need` is `"ogquery_version"`, with the version found in `e$found`),
including the placeholder 0.0.0 on PyPI.

If the Python you chose doesn’t have a supported ogquery, opengwasr
stops and says how to install one there. It never swaps your Python for
the automatic one. (One gap: a `use_condaenv()` or `use_virtualenv()`
call can’t be seen before Python starts. Once Python has started,
reticulate names the call and opengwasr reports it, but if reticulate
couldn’t start that environment and started its automatic one instead,
only the automatic one is named, and opengwasr can’t tell.
`RETICULATE_PYTHON` has no such gap.)

### On a laptop

Nothing is installed with opengwasr: reticulate is a suggested package.
On your first
[`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)
call, opengwasr offers to install reticulate if it is missing.
reticulate then sets up Python and ogquery for you, automatically, using
uv. You have nothing else to do.

### On an HPC cluster

Compute nodes often have no internet, so prepare the environment on a
login node and tell R where it is. ogquery needs Python 3.12 or later,
so load a module that has it first. In a shell on the login node:

``` bash
module load python/3.12
python3 -m venv /home/me/venvs/ogquery
/home/me/venvs/ogquery/bin/pip install "ogquery>=0.1.0,<1"
```

Then, in your `.Renviron` (which you can open with
`usethis::edit_r_environ()`), with an absolute path, because R doesn’t
expand `~` there:

``` text
RETICULATE_PYTHON=/home/me/venvs/ogquery/bin/python
```

Restart R, and every job on the cluster reads the same environment.

If you use conda, make the environment on the login node instead:

``` bash
conda create -n ogquery python=3.13
conda activate ogquery
pip install "ogquery>=0.1.0,<1"
```

Then either set `RETICULATE_PYTHON` to that environment’s `python` in
`.Renviron`, as above, or name the environment in R before the first
[`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)
call:

``` r

reticulate::use_condaenv("ogquery")
```

### Checking the setup

[`og_local_check()`](https://opengwas.github.io/opengwasr/reference/og_local_check.md)
takes the same directories as
[`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)’s
`gwas`, or a Local Source. It reports the Python opengwasr found and how
it was chosen, the reticulate and ogquery versions, and, for each path,
the Store Release it found there and whether ogquery can read it. It
reports problems rather than stopping on them, so it is where to start
when
[`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)
refuses.

For your own Store Releases, give their GWAS Store directories, such as
`og_local_check(c("/data/opengwas/OGS-00011", "/data/opengwas/OGS-00004"))`.
opengwasr comes with four small example Store Releases, from the Store
Reader’s own test set, in three GWAS Store directories, and this
vignette uses them:

``` r

stores <- system.file("stores", package = "opengwasr")
og_local_check(file.path(stores, c("dense", "ragged")))
```

    ## <og_local_check>
    ## Python:     /opt/hostedtoolcache/Python/3.13.18/x64/bin/python (chosen by RETICULATE_PYTHON)
    ## reticulate: 1.47.0
    ## ogquery:    0.4.0, reads format 0.3.x
    ## 
    ## Store Releases:
    ##   /home/runner/work/_temp/Library/opengwasr/stores/dense
    ##     resolved to /home/runner/work/_temp/Library/opengwasr/stores/dense/r
    ##     s/r, format 0.3.0: readable
    ##   /home/runner/work/_temp/Library/opengwasr/stores/ragged
    ##     resolved to /home/runner/work/_temp/Library/opengwasr/stores/ragged/comp
    ##     residual-rag/comp, format 0.3.0: readable

Under each GWAS Store directory is the Store Release it resolved to.

For a Local Source with a PLINK Reference, it also reports the PLINK
Reference: its fileset, and the PLINK binary and its version. An LD-only
Local Source, `og_local(bfile = )`, has no Store Releases, so that is
all it reports, and it needs no Python.

A Store Release ogquery can’t read is listed as `not readable`, with the
format versions it reads. Its value is a list, so a script can test it
too: `og_local_check(path)$releases$readable`.

Without Python, or without ogquery in it, a Local Source stops with an
error of class `opengwasr_local_unavailable` that says what to do. For a
Python you chose:

``` text
# illustrative
Error in `og_local()`:
! The Python in use, /home/me/venvs/ogquery/bin/python (RETICULATE_PYTHON), can't import ogquery.
ℹ Install ogquery into that Python: `/home/me/venvs/ogquery/bin/python -m pip install "ogquery>=0.1.0,<1"`, then restart R.
```

If the automatic setup ever gives you an unsupported ogquery (an old one
kept in uv’s cache), the error gives the command that clears uv’s cached
ogquery, with the uv reticulate uses; run it and restart R, and
reticulate sets up a supported one.

## Opening Store Releases

Give
[`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)
the directory of each GWAS Store you want to query, one element per GWAS
Store, such as
`og_local(c("/data/opengwas/OGS-00009", "/data/opengwas/OGS-00011"))`. A
GWAS Store directory holds one Store Release, and
[`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)
opens it: it looks one level down, for the directory holding a
`manifest.json`, and never deeper. A Store Release’s own directory works
too, such as `/data/opengwas/OGS-00011/OGS-00011-v2`. With the example
Store Releases (`hyb` holds two, so its Store Release `v1` is named by
its own directory):

``` r

local <- og_local(c(file.path(stores, c("dense", "ragged")), file.path(stores, "hyb", "v1")))
local
```

    ## <og_local_source>
    ## • Store Releases: 3
    ## • s/r: /home/runner/work/_temp/Library/opengwasr/stores/dense/r
    ## • residual-rag/comp: /home/runner/work/_temp/Library/opengwasr/stores/ragged/comp
    ## • hyb/v1: /home/runner/work/_temp/Library/opengwasr/stores/hyb/v1
    ## • LD resource: none

Each Store Release names itself by its manifest:
`<store_id>/<release_id>`, then where it is.

[`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)
reads no Associations: it checks that each path is, or holds, a Store
Release that ogquery can read. A path with no Store Release, in it or
one level down, is refused (`opengwasr_not_store_release`), and so is a
Store Release whose format ogquery can’t read
(`opengwasr_format_unsupported`, naming both format versions). A GWAS
Store directory holding several Store Releases is refused too
(`opengwasr_several_releases`), listing each, so you can give the one
you want:

``` r

og_local(file.path(stores, "hyb"))
```

    ## Error in `og_local()`:
    ## ! A GWAS Store directory holds one Store Release, and this one holds several: /home/runner/work/_temp/Library/opengwasr/stores/hyb
    ## ✖ hyb/v1: /home/runner/work/_temp/Library/opengwasr/stores/hyb/v1
    ## ✖ hyb/v1-completed: /home/runner/work/_temp/Library/opengwasr/stores/hyb/v1c
    ## ℹ Give `og_local()` the directory of the Store Release you want instead, one of those listed.

A Local Source holds at most one Store Release of each GWAS Store, so
two Store Releases of the same GWAS Store are refused when you create it
(`opengwasr_duplicate_store`). The directories `hyb/v1` and `hyb/v1c`
hold two Store Releases of GWAS Store `hyb`, whose release ids are `v1`
and `v1-completed`:

``` r

og_local(file.path(stores, "hyb", c("v1", "v1c")))
```

    ## Error in `og_local()`:
    ## ! A Local Source holds one Store Release of each GWAS Store, and these are both of GWAS Store `hyb`:
    ## ✖ hyb/v1: /home/runner/work/_temp/Library/opengwasr/stores/hyb/v1
    ## ✖ hyb/v1-completed: /home/runner/work/_temp/Library/opengwasr/stores/hyb/v1c
    ## ℹ Give `og_local()` the path of only the one you want to query.

## Changing one line

A script written for the API Source runs on the Local Source by changing
the line that makes the Source. Everything after it is the same:

``` r

# src <- og_api()
src <- og_local(c(file.path(stores, c("dense", "ragged")), file.path(stores, "hyb", "v1")))

top <- og_tophits(src, "s:bmi_irn", clump = FALSE, limit = 10)
head(top[, c("rsid", "alid", "beta", "se", "pval", "association_status")], 4)
```

    ##     rsid       alid        beta         se         pval association_status
    ## 1   <NA> 1:5000:C:T -0.09001923 0.01000214 2.257177e-19           observed
    ## 2 rs5000 1:5000:C:G  0.08001709 0.01000214 1.244192e-15           observed
    ## 3 rs1007 X:2500:G:T -0.08001709 0.01000214 1.244192e-15           observed
    ## 4 rs5000 1:5000:A:C  0.07001495 0.01000214 2.559625e-12           observed

Here `src` is the example Store Releases. With your own, give their GWAS
Store directories, such as `og_local("/data/opengwas/OGS-00011")`, and
name your Analyses, such as `"OGS-00011:GCST003116"`. The data frame has
the columns of the API Source’s, in the same order (see [What the
columns
mean](https://opengwas.github.io/opengwasr/articles/opengwasr.html#columns)):

``` r

names(top)
```

    ##  [1] "id"                 "store_id"           "analysis_id"       
    ##  [4] "analysis_label"     "alid"               "rsid"              
    ##  [7] "chromosome"         "position"           "effect_allele"     
    ## [10] "other_allele"       "eaf"                "beta"              
    ## [13] "se"                 "z"                  "pval"              
    ## [16] "effect_scale"       "n"                  "sample_size_kind"  
    ## [19] "ncase"              "ncontrol"           "association_status"
    ## [22] "release_id"

The numbers come from the same Store Reader the API uses, and are shaped
by the same code, so a Store Release gives the same answer from either
Source.
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md)
says which Store Release each answer came from.

When `limit` cuts an Analysis’s top hits off, you get the warning the
API Source gives (`opengwasr_truncated`):

``` r

cols <- c("id", "alid", "rsid", "beta", "se", "pval", "association_status")
top <- og_tophits(local, "s:bmi_irn", clump = FALSE, limit = 5)
```

    ## Warning in og_tophits(local, "s:bmi_irn", clump = FALSE, limit = 5): s:bmi_irn has more than 5 top hits at p <= 5e-08; only the strongest 5 were returned.
    ## ℹ Raise `limit` (at most 10,000) for more.

``` r

top[, cols]
```

    ##          id       alid   rsid        beta         se         pval
    ## 1 s:bmi_irn 1:5000:C:T   <NA> -0.09001923 0.01000214 2.257177e-19
    ## 2 s:bmi_irn 1:5000:C:G rs5000  0.08001709 0.01000214 1.244192e-15
    ## 3 s:bmi_irn X:2500:G:T rs1007 -0.08001709 0.01000214 1.244192e-15
    ## 4 s:bmi_irn 1:5000:A:C rs5000  0.07001495 0.01000214 2.559625e-12
    ## 5 s:bmi_irn  2:300:C:T rs1005  0.07001495 0.01000214 2.559625e-12
    ##   association_status
    ## 1           observed
    ## 2           observed
    ## 3           observed
    ## 4           observed
    ## 5           observed

``` r

og_provenance(top)
```

    ## ── opengwasr provenance ─────────────────
    ## • Query: og_tophits(threshold = 5e-08, limit = 5, observed_only = FALSE, clump = FALSE, original_units = FALSE)
    ## • Store Releases: s/r
    ## • Jobs: none
    ## • Rows: 5 returned of 5 matching (truncated)
    ## • Retrieved: 2026-11-26 13:18:09 UTC

A local result’s provenance names the Store Releases read, and no Jobs:
a Local Source answers at once.

The other verbs work the same way. The Associations of variants in
Analyses: an rsID stands for every variant it names, here two at
`1:5000`. `residual-rag` is a Reference-Completed Store Release, and its
Association at `1:1001500:A:G` is an Imputed Association, which
`observed_only = TRUE` leaves out:

``` r

og_associations(local, c("rs5000", "1:1001500:A:G"), c("s:bmi_irn", "residual-rag:a"))[, cols]
```

    ##               id          alid   rsid       beta         se         pval
    ## 1      s:bmi_irn    1:5000:A:C rs5000 0.07001495 0.01000214 2.559625e-12
    ## 2      s:bmi_irn    1:5000:C:G rs5000 0.08001709 0.01000214 1.244192e-15
    ## 3 residual-rag:a 1:1001500:A:G rs1003 0.23241760 0.17525451 1.847828e-01
    ##   association_status
    ## 1           observed
    ## 2           observed
    ## 3            imputed

``` r

og_associations(local, c("rs5000", "1:1001500:A:G"), c("s:bmi_irn", "residual-rag:a"), observed_only = TRUE)[, cols]
```

    ##          id       alid   rsid       beta         se         pval
    ## 1 s:bmi_irn 1:5000:A:C rs5000 0.07001495 0.01000214 2.559625e-12
    ## 2 s:bmi_irn 1:5000:C:G rs5000 0.08001709 0.01000214 1.244192e-15
    ##   association_status
    ## 1           observed
    ## 2           observed

A PheWAS of one variant across every Analysis the Local Source holds,
and the Associations of an Analysis in a range:

``` r

og_phewas(local, "1:1000:A:G", threshold = 1e-9)[, cols]
```

    ##            id       alid rsid      beta        se         pval
    ## 1 hyb:trait_a 1:1000:A:G <NA> -1.291315 0.1614144 1.244192e-15
    ## 2 hyb:trait_b 1:1000:A:G <NA> -1.578033 0.1972542 1.244192e-15
    ##   association_status
    ## 1           observed
    ## 2           observed

``` r

og_range(local, "s:bmi_irn", chromosome = "1", start = 4000, end = 6000)[, cols]
```

    ##          id       alid   rsid        beta         se         pval
    ## 1 s:bmi_irn 1:5000:A:C rs5000  0.07001495 0.01000214 2.559625e-12
    ## 2 s:bmi_irn 1:5000:C:G rs5000  0.08001709 0.01000214 1.244192e-15
    ## 3 s:bmi_irn 1:5000:C:T   <NA> -0.09001923 0.01000214 2.257177e-19
    ##   association_status
    ## 1           observed
    ## 2           observed
    ## 3           observed

The variant lookups run the same way. `rs5000` names two variants in one
Store Release, and two Store Releases hold a variant at `1:1000`, one of
them without an rsID:

``` r

og_variants_rsid(local, "rs5000")
```

    ##         alid chromosome effect_allele other_allele position release_id   rsid
    ## 1 1:5000:A:C          1             A            C     5000          r rs5000
    ## 2 1:5000:C:G          1             C            G     5000          r rs5000
    ##   store_id
    ## 1        s
    ## 2        s

``` r

og_variants_chrpos(local, "1:1000")
```

    ##         alid chromosome effect_allele other_allele position release_id   rsid
    ## 1 1:1000:A:G          1             A            G     1000          r rs1000
    ## 2 1:1000:A:G          1             A            G     1000         v1   <NA>
    ##   store_id
    ## 1        s
    ## 2      hyb

[`og_status()`](https://opengwas.github.io/opengwasr/reference/og_status.md),
[`og_user()`](https://opengwas.github.io/opengwasr/reference/og_status.md),
[`og_submit()`](https://opengwas.github.io/opengwasr/reference/og_submit.md)
and the `og_job_*` functions mean nothing for a Local Source: there is
no API to ask, and nothing to queue. Each refuses it with an error of
class `opengwasr_local_unsupported`.
[`og_local_check()`](https://opengwas.github.io/opengwasr/reference/og_local_check.md)
is where to look at a Local Source instead:

``` r

og_status(local)
```

    ## Error in `og_status()`:
    ## ! A Local Source has no API whose status to report.
    ## ℹ Check its Python, ogquery and Store Releases with `og_local_check(src)`.

Clumping and LD matrices need an LD resource, which you give the Local
Source when you create it ([Clumping and LD](#local-clumping)).
[`og_ld_stores()`](https://opengwas.github.io/opengwasr/reference/og_ld_stores.md),
[`og_ld_proxies()`](https://opengwas.github.io/opengwasr/reference/og_ld_proxies.md)
and
[`og_ld_reflookup()`](https://opengwas.github.io/opengwasr/reference/og_ld_reflookup.md)
read the API’s LD Stores only, so a Local Source refuses them the same
way (`opengwasr_local_unsupported`).

## Naming Analyses

An Analysis is named `<store_id>:<analysis_id>`, such as
`OGS-00011:GCST003116`, as with the API Source, and a bare `analysis_id`
is accepted where it names one Analysis. An Analysis name that matches
nothing is refused, as with the API Source
(`opengwasr_analysis_not_found`).
[`og_analyses()`](https://opengwas.github.io/opengwasr/reference/og_analyses.md)
lists every Analysis the Local Source holds, or the ones you name:

``` r

og_analyses(local)[, c("id", "analysis_label", "sample_size", "release_id")]
```

    ##               id analysis_label sample_size release_id
    ## 1      s:bmi_irn        BMI_IRN      500000          r
    ## 2 residual-rag:a           <NA>       50000       comp
    ## 3 residual-rag:b           <NA>       50000       comp
    ## 4    hyb:trait_a        Trait A        1000         v1
    ## 5    hyb:trait_b        Trait B        1000         v1

``` r

og_analyses(local, "s:bmi_irn")[, c("requested_id", "id", "stored_effect_scale", "outcome_type")]
```

    ##   requested_id        id stored_effect_scale outcome_type
    ## 1    s:bmi_irn s:bmi_irn                  sd         <NA>

The rows have every column of the API Source’s, in its order, read from
each Store Release’s `analyses.tsv`, and then the columns of
`analyses.tsv` the API Source doesn’t return. `outcome_type` is derived
as the catalogue derives it. The columns the catalogue adds (`status`,
`listed`, `trait_class`, `ancestry_category` and `label_from_source`)
are `NA` locally, as is any column a Store Release has no value for.

The columns of `analyses.tsv` the API Source doesn’t return come with
their types, as opengwasdb defines them: the Top-Hit Counts
(`n_hits_5e8`, `n_hits_5e6`, `n_hits_5e4`), Reference Completion’s
counts (`completion_n_imputed_total`, `completion_n_missing_total`),
`n_missing_se_out_of_range` and `eaf_orientation_n` are whole numbers;
`completion_median_pearson_r`, `eaf_orientation_r` and
`original_sd_dispersion` are numbers, as is each ancestry proportion
(`ancestry_prop_<population>`, such as `ancestry_prop_eur`), from 0 to
1; `eaf_scope`, `completed_against` and `eaf_orientation` are text. A
blank is `NA` of its column’s type. A column opengwasr doesn’t know is
text. A value that isn’t of its column’s type (a count of `1.5`, say) is
refused with `opengwasr_invalid_analyses`, naming the Store Release, the
column and the value, never rounded:

``` r

str(og_analyses(local, "residual-rag:a")[, c("n_hits_5e8", "completion_n_imputed_total",
                                            "completion_median_pearson_r", "eaf_orientation",
                                            "eaf_orientation_r", "eaf_orientation_n")])
```

    ## 'data.frame':    1 obs. of  6 variables:
    ##  $ n_hits_5e8                 : int 60
    ##  $ completion_n_imputed_total : int 3
    ##  $ completion_median_pearson_r: num 0.597
    ##  $ eaf_orientation            : chr "unverified"
    ##  $ eaf_orientation_r          : num NA
    ##  $ eaf_orientation_n          : int 0

### Store Releases

Because a Local Source holds one Store Release of each GWAS Store, a
name never has to choose between Store Releases. `releases` works as on
the API Source, in every query function that takes Analyses: give a GWAS
Store id, such as `"OGS-00011"`, or `"<store_id>/<release_id>"`, naming
a Store Release the Source holds. One it doesn’t hold is refused, as the
API Source refuses an unknown Store Release.

For example
`og_analyses(src, "OGS-00011:GCST003116", releases = "OGS-00011/OGS-00011-v2")`.
With the example Store Releases:

``` r

og_analyses(local, releases = "residual-rag/comp")[, c("id", "release_id")]
```

    ##               id release_id
    ## 1 residual-rag:a       comp
    ## 2 residual-rag:b       comp

``` r

og_analyses(local, releases = "hyb/v1-completed")
```

    ## Error in `og_analyses()`:
    ## ! This Local Source holds no Store Release `hyb/v1-completed`.
    ## ℹ It holds `s/r`, `residual-rag/comp`, `hyb/v1`.

Record the `store_id` and `release_id` columns that come back with every
Association to reproduce an analysis later.

### Legacy OpenGWAS ids

Store Releases don’t carry the ids **Legacy OpenGWAS** used
(`ukb-b-19953`, `ebi-a-GCST003116`), so a Local Source refuses one
rather than guess, and the error tells you what to do. It stays on your
machine and never calls the API for you.

The error has class `opengwasr_legacy_id_local`:

``` r

og_analyses(local, "ukb-b-19953")
```

    ## Error in `og_analyses()`:
    ## ! `ukb-b-19953` names no Analysis here, and a Local Source holds only canonical ids (`<store_id>:<analysis_id>`): it can't translate a Legacy OpenGWAS id.
    ## ℹ Translate with the API Source, `og_analyses(og_api(), "ukb-b-19953")`, then use the `id` it returns.

Translate once, with the API Source, and keep the canonical id from then
on:

``` r

og_analyses(og_api(), "ukb-b-19953")[, c("requested_id", "id")]
```

``` text
# illustrative
  requested_id                    id
1  ukb-b-19953 OGS-00009:ukb-b-19953
```

Then use `OGS-00009:ukb-b-19953` in every Local Source call.

## Clumping and LD

A Source carries an **LD resource**, which answers LD for its
Associations (see
[Sources](https://opengwas.github.io/opengwasr/articles/opengwasr.html#sources)).
A Local Source has none unless you give it one, and it never uses the
API’s LD: a local query makes no network calls. Today the LD resource a
Local Source can carry is a **PLINK Reference**, a PLINK binary genotype
fileset (`.bed`, `.bim`, `.fam`) on your machine:

| LD resource | On your machine |
|----|----|
| **PLINK Reference** | `og_local(gwas = ..., bfile = prefix)`, or `og_local(bfile = prefix)` alone for LD only, and `plink =` the PLINK 1.9 or 2 binary if opengwasr can’t find it. Available now |
| **LD Store** | `og_local(gwas = ..., ld_store = path)`. Coming, when opengwasr can read downloaded LD Stores |
| **Genotype Store** | `og_local(gwas = ..., geno_store = path)`. Coming, when opengwasr can read downloaded Genotype Stores |
| None given | `og_local(gwas = ...)`: anything that needs LD stops and asks you to give one |

A form that is coming is refused (`opengwasr_local_unsupported`), and so
is giving two kinds at once.

LD from a PLINK Reference is computed from its genotypes without
adjusting for population structure (principal components), unlike
OpenGWAS LD Stores and Genotype Stores, so it may be less accurate.
opengwasr warns of this once per PLINK Reference in a session (class
`opengwasr_plink_unadjusted`).

The LD is chosen on the Source, and only there: no verb takes an LD
argument. A Source’s LD lives where the Source does, so a Local Source’s
LD is on your machine. To use LD from elsewhere, fetch with one Source
and clump with the other ([Associations from one place, LD from
another](https://opengwas.github.io/opengwasr/articles/ld.html#mixing)):

| Associations | LD | How |
|----|----|----|
| Local | PLINK | `og_tophits(og_local(gwas = ..., bfile = ...), ids, r2 = , kb = )` ([below](#local-clumping-plink)) |
| Local | API | `og_clump(og_api(), og_tophits(local, ids, clump = FALSE, limit = 10000))` ([below](#local-ld-api)) |
| API | PLINK | `og_clump(og_local(bfile = ...), og_tophits(og_api(), ids, clump = FALSE, limit = 10000), r2 = , kb = )` ([Using a PLINK Reference](https://opengwas.github.io/opengwasr/articles/ld.html#plink-reference)) |

[`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md)
clumps each Analysis’s rows on their own, as
[`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
does, so every row gives each Analysis its own index variants.
`limit = 10000` (the most) fetches every top hit to clump: its default,
1,000, would cut them off before clumping, with an `opengwasr_truncated`
warning.

### Clumping top hits

[`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
clumps by default (`clump = TRUE`), as with the API Source;
`clump = FALSE` gives every top hit, unclumped.

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

So today, a Local Source with no LD resource can’t clump. It stops
(class `opengwasr_ld_resource_required`), names the LD resources you can
give it, and offers `clump = FALSE`:

``` r

og_tophits(local, "s:bmi_irn")
```

    ## Error in `og_tophits()`:
    ## ! A Local Source with no LD resource can't clump: Store Releases have no stored Clumps yet, so they would be computed, which needs LD.
    ## ℹ Give the Source a PLINK Reference: `og_local(inherit = src, bfile = "<prefix>")` keeps its Store Releases, and `plink =` names the PLINK binary if it isn't found.
    ## ℹ Or, for the API's LD Stores, clump in two steps on the API Source: `og_clump(og_api(), og_tophits(src, ids, clump = FALSE))`.
    ## ℹ Downloaded LD Stores (`ld_store =`) and Genotype Stores (`geno_store =`) can't be read yet.
    ## ℹ Or pass `clump = FALSE` for every top hit, unclumped.

Once a Store Release has stored Clumps, the same call will read them,
with no LD resource:

``` r

mine <- og_local(gwas = "/data/opengwas/OGS-00011")
top <- og_tophits(mine, "OGS-00011:GCST003116")
og_provenance(top)$clump[, c("id", "ld", "r2", "kb", "leads", "clumps")]
```

``` text
# illustrative
                    id                  ld         r2    kb leads clumps
1 OGS-00011:GCST003116 hgdp1kgp-v2-EUR-k10 0.03186544 10000    49 stored
```

### With a PLINK Reference

Give
[`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)
the fileset’s prefix as `bfile`, as ieugwasr’s local functions took it.
opengwasr comes with a small synthetic PLINK Reference, 300 made-up
people, whose rsIDs include the example Store Releases’ top hits, and
this vignette uses it:

``` r

bfile <- file.path(system.file("plink", package = "opengwasr"), "ref")
src <- og_local(gwas = c(file.path(stores, c("dense", "ragged")), file.path(stores, "hyb", "v1")),
                bfile = bfile)
src
```

    ## <og_local_source>
    ## • Store Releases: 3
    ## • s/r: /home/runner/work/_temp/Library/opengwasr/stores/dense/r
    ## • residual-rag/comp: /home/runner/work/_temp/Library/opengwasr/stores/ragged/comp
    ## • hyb/v1: /home/runner/work/_temp/Library/opengwasr/stores/hyb/v1
    ## • LD resource: PLINK Reference /home/runner/work/_temp/Library/opengwasr/plink/ref
    ##   PLINK 1.90b6.21: /home/runner/work/_temp/Library/genetics.binaRies/bin/plink

``` r

top <- og_tophits(src, "s:bmi_irn", r2 = 0.001, kb = 10000)
```

    ## Warning in og_tophits(src, "s:bmi_irn", r2 = 0.001, kb = 10000): LD from a PLINK Reference is computed from its genotypes without adjusting for population structure (principal components), unlike OpenGWAS LD Stores and Genotype Stores, so it may be less accurate.
    ## ℹ PLINK Reference: /home/runner/work/_temp/Library/opengwasr/plink/ref.
    ## This warning is displayed once per session.

    ## ℹ 6 of 8 top hits matched the PLINK Reference by rsID; 2 not in it were
    ## left out.

``` r

top[, c("alid", "rsid", "z", "clump_r2", "clump_kb", "clump_size")]
```

    ##         alid   rsid  z clump_r2 clump_kb clump_size
    ## 1 1:5000:C:G rs5000  8    0.001    10000          2
    ## 2 1:5000:A:C rs5000  7    0.001    10000          2
    ## 3  2:300:C:T rs1005  7    0.001    10000          2
    ## 4 1:9000:A:G rs1004 -6    0.001    10000          1

``` r

og_provenance(top)$clump[, c("id", "ld_choice", "r2", "kb", "hits", "absent", "leads", "clumps")]
```

    ##          id ld_choice    r2    kb hits absent leads   clumps
    ## 1 s:bmi_irn      user 0.001 10000    8      2     3 computed

With your own, give its prefix, and the PLINK binary if opengwasr can’t
find one (it looks in the genetics.binaRies package, then for `plink`,
then `plink2`, on your `PATH`):
`og_local(gwas = "/data/opengwas/OGS-00011", plink = "/usr/local/bin/plink", bfile = "/data/ld/1kg/EUR")`.
PLINK 1.9 and PLINK 2 (2.0.0-a.6.0 or later) both work, and give the
same clumps and LD. `plink` is a path, or a command on your `PATH`, such
as `plink = "plink2"`. The Source prints the PLINK it uses and its
version. A PLINK you name that isn’t supported, such as 1.07, is refused
(class `opengwasr_plink_unsupported`); when opengwasr looks for one
itself, it skips those. Variants are matched to the PLINK Reference by
rsID, because PLINK References are usually on GRCh37: a top hit with no
rsID, or one the reference doesn’t hold, is left out, and a message says
how many. The Analysis’s top hits are read from your Store Releases, and
PLINK clumps them on your machine.

A PLINK Reference has no default `r2` and `kb` of its own, so clumping
with one needs both (class `opengwasr_plink_settings_required`):

``` r

og_tophits(src, "s:bmi_irn")
```

    ## Error in `og_tophits()`:
    ## ! A PLINK Reference has no recorded `r2` and `kb`, so clumping with one needs both.
    ## ℹ For example `r2 = 0.001, kb = 10000`, as ieugwasr's defaults.

To clump Associations you already have, use
[`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md).
It uses the Source’s LD resource too, and clumps each Analysis’s rows on
their own, one PLINK run per Analysis, so here it gives each Analysis
the same index variants as
`og_tophits(src, c("s:bmi_irn", "hyb:trait_a"), r2 = 0.001, kb = 10000)`
would. Each row is matched by its own rsID: `hyb:trait_a`’s top hit at
`1:1000:A:G` has none, so it is left out, although `s:bmi_irn`’s row of
the same ALID has one:

``` r

hits <- og_tophits(src, c("s:bmi_irn", "hyb:trait_a"), clump = FALSE)
clumped <- og_clump(src, hits, r2 = 0.001, kb = 10000)
```

    ## ℹ 13 of 16 variants matched the PLINK Reference by rsID; 3 not in it
    ## were left out.

``` r

clumped[, c("id", "alid", "rsid", "z", "clump_size")]
```

    ##            id         alid   rsid  z clump_size
    ## 1   s:bmi_irn   1:5000:C:G rs5000  8          2
    ## 2   s:bmi_irn   1:5000:A:C rs5000  7          2
    ## 3   s:bmi_irn    2:300:C:T rs1005  7          2
    ## 4   s:bmi_irn   1:9000:A:G rs1004 -6          1
    ## 5 hyb:trait_a  1:51000:A:G rs1050 -8          6
    ## 6 hyb:trait_a 1:300000:A:G rs1200 -8          1

``` r

og_provenance(clumped)$clump[, c("id", "hits", "absent", "leads")]
```

    ##            id hits absent leads
    ## 1   s:bmi_irn    8      2     3
    ## 2 hyb:trait_a    8      1     2

### LD matrices

[`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md)
reads the LD matrix from the Source’s PLINK Reference. Variants given as
ALIDs are matched by their rsIDs, which are looked up in your Store
Releases, never on the API:

``` r

m <- og_ld_matrix(src, c("1:5000:C:G", "1:1000:A:G", "2:300:C:T", "2:7000:A:C"))
round(m, 3)
```

    ##            rs5000_C_G rs1000_A_G rs1005_C_T rs1006_A_C
    ## rs5000_C_G      1.000      0.627      0.046      0.110
    ## rs1000_A_G      0.627      1.000      0.041      0.087
    ## rs1005_C_T      0.046      0.041      1.000      0.757
    ## rs1006_A_C      0.110      0.087      0.757      1.000

The matrix is named `<rsid>_<allele>_<allele>`, with the PLINK
Reference’s alleles; the sign of r refers to the first one. rsIDs work
as well as ALIDs, and need no lookup.

### Other LD for the same Store Releases

Other LD is another Source. `og_local(inherit = )` makes one like the
Source you have: it keeps its Store Releases, and the LD you give
replaces its LD resource. Here `local`, which has no LD resource, gets
the PLINK Reference:

``` r

with_ld <- og_local(inherit = local, bfile = bfile)
with_ld
```

    ## <og_local_source>
    ## • Store Releases: 3
    ## • s/r: /home/runner/work/_temp/Library/opengwasr/stores/dense/r
    ## • residual-rag/comp: /home/runner/work/_temp/Library/opengwasr/stores/ragged/comp
    ## • hyb/v1: /home/runner/work/_temp/Library/opengwasr/stores/hyb/v1
    ## • LD resource: PLINK Reference /home/runner/work/_temp/Library/opengwasr/plink/ref
    ##   PLINK 1.90b6.21: /home/runner/work/_temp/Library/genetics.binaRies/bin/plink

``` r

og_ld_matrix(with_ld, c("rs5000", "rs1000"))
```

    ##            rs5000_C_G rs1000_A_G
    ## rs5000_C_G   1.000000   0.626777
    ## rs1000_A_G   0.626777   1.000000

`local` itself is unchanged. Any LD argument you give replaces the
inherited LD resource as a whole, never mixed with it, except `plink`
alone, which keeps the fileset and changes the PLINK that reads it
(`og_local(inherit = with_ld, plink = "plink2")`):
`og_local(inherit = with_ld, ld_store = path)` is refused because
downloaded LD Stores can’t be read yet, not because the Source would
then have two. A Local Source holds none of the API’s LD Stores, and a
local query never calls the API: use the two steps below for them.

### LD from the API’s LD Stores

To clump Associations from your Store Releases with LD from the API’s LD
Stores, do it in two steps, naming the API Source yourself: read the top
hits unclumped from your Store Releases, then clump them with
[`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md)
on the API Source. This needs your OpenGWAS token (see
[Credentials](https://opengwas.github.io/opengwasr/articles/opengwasr.html#credentials)).
Nothing goes to the API unless you write
[`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md).

``` r

hits <- og_tophits(local, c("OGS-00011:GCST003116", "OGS-00011:GCST90029007"), clump = FALSE, limit = 10000)
top <- og_clump(og_api(), hits)                  # each Analysis in the LD Store matching its Assigned Ancestry
top <- og_clump(og_api(ld_store = "EUR"), hits)  # or each in the EUR LD Store
```

[`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md)
clumps each Analysis’s top hits on their own, as
[`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
does on the API Source, so Analyses of different ancestries can go in
one call. The LD Store’s own `r2` and `kb` apply, as on the API Source
(see
[Clumping](https://opengwas.github.io/opengwasr/articles/ld.html#clumping)).

## TwoSampleMR on a Local Source

TwoSampleMR 0.8.0’s data functions take an opengwasr Source as `src`, so
a pipeline runs on your Store Releases once you give them a Local
Source. `extract_instruments()` clumps by default, so give the Source an
LD resource too, here a PLINK Reference:

``` r

library(TwoSampleMR)
src <- og_local(gwas = c("/data/opengwas/OGS-00009", "/data/opengwas/OGS-00011"),
                bfile = "/data/ld/1kg/EUR")

exposure <- extract_instruments("OGS-00009:ukb-b-19953", src = src, r2 = 0.001, kb = 10000)
outcome <- extract_outcome_data(exposure$SNP, "OGS-00011:GCST003116", src = src)
dat <- harmonise_data(exposure, outcome)
mr(dat)
```

TwoSampleMR clumps with the Source’s LD resource. With a PLINK
Reference, `r2` and `kb` are required (it has no thresholds of its own),
or pass `clump = 0` for unclumped instruments.

Your numbers can differ from ieugwasr’s for the reasons in [Migrating
from
ieugwasr](https://opengwas.github.io/opengwasr/articles/migrating-from-ieugwasr.html#what-changed).
