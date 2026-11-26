# Create a Local Source

A Local Source answers queries from Store Releases downloaded to this
machine, through ogquery, the Store Reader. Give it the directory of
each GWAS Store, one element per GWAS Store. A GWAS Store directory
holds one Store Release, one level down: the directory there holding a
`manifest.json`. It is searched one level down and never deeper, so the
Dense Component inside a Hybrid Store Release
(`<release>/dense/manifest.json`) is never taken for a Store Release,
and hidden directories (whose names start with `.`), such as a build in
progress, are skipped. A Store Release's own directory (the one holding
its `manifest.json`) works too. One that has lost its `manifest.json`
(it still holds `data.zarr` or `analyses.tsv`) is refused, never
searched as a GWAS Store directory. Each Store Release names itself by
its manifest (`store_id`, `release_id`, `format_version`), and a Local
Source holds at most one Store Release of each GWAS Store, so no query
ever chooses between Store Releases.

## Usage

``` r
og_local(
  gwas = NULL,
  plink = NULL,
  bfile = NULL,
  ld_store = NULL,
  geno_store = NULL,
  inherit = NULL
)
```

## Arguments

- gwas:

  The directory of each GWAS Store, or of each Store Release: a
  character vector, one element per GWAS Store. `NULL` (the default) for
  none: an LD-only Local Source, which needs `bfile`. A Local Source
  with neither Store Releases nor an LD resource is refused (class
  `opengwasr_argument`).

- plink:

  The PLINK 1.9 or 2 binary, for `bfile`: its path, or a command on your
  `PATH`, such as `"plink2"` (as in a shell, a name without a path
  separator is a command). `NULL` takes the first supported PLINK of the
  genetics.binaRies package, if it is installed, then `plink`, then
  `plink2`, on your `PATH`, skipping any that isn't supported; if there
  is none, it stops (class `opengwasr_plink_not_found`, with `skipped`:
  `plink_bin` and `version` of each one skipped) and says how to install
  it.

- bfile:

  A PLINK binary fileset's prefix (without `.bed`, `.bim` or `.fam`):
  the Source's LD resource, a PLINK Reference. LD from a PLINK Reference
  is computed from its genotypes without adjusting for population
  structure (principal components), unlike OpenGWAS LD Stores and
  Genotype Stores, so it may be less accurate. opengwasr warns of this
  once per PLINK Reference in a session (class
  `opengwasr_plink_unadjusted`).

- ld_store:

  A downloaded LD Store. Not available yet.

- geno_store:

  A downloaded Genotype Store. Not available yet.

- inherit:

  A Local Source to build this one like, as described under
  "Inheriting".

## Value

An `og_local_source`. Its `releases` is a data frame with one row per
Store Release: `store_id`, `release_id`, `format_version` and `path`
(the Store Release's own directory), with no rows for an LD-only Local
Source. Its LD resource is read with
[`og_ld_resource()`](https://opengwas.github.io/opengwasr/reference/og_ld_resource.md):
an `og_plink_reference`, or `NULL`.

## Details

`og_local()` reads no Associations. It checks that each path is, or
holds, a Store Release that the installed ogquery can read, and refuses
the Source otherwise:

- a path that doesn't exist, or holds no Store Release (no manifest in
  it or one level down, or one ogquery can't parse):
  `opengwasr_not_store_release`, with `e$paths` and why each was
  refused, `e$reasons`;

- a GWAS Store directory holding several Store Releases:
  `opengwasr_several_releases`, with one element per Store Release found
  in `e$directory` (the GWAS Store directory given), `e$store_id`,
  `e$release_id`, `e$releases` (`<store_id>/<release_id>`) and
  `e$paths`, so you can give the one you want;

- a Store Release whose format version ogquery can't read:
  `opengwasr_format_unsupported`, with `e$paths`, each one's version
  (`e$format_version`) and the format series ogquery reads
  (`e$readable`);

- two Store Releases of one GWAS Store: `opengwasr_duplicate_store`.

## Python

The first local call finds Python and ogquery. A Python you chose always
wins, in the order reticulate ranks them: `RETICULATE_PYTHON` (a Python,
or its directory), `RETICULATE_PYTHON_ENV`, an environment you selected
with
[`reticulate::use_virtualenv()`](https://rstudio.github.io/reticulate/reference/use_python.html),
[`reticulate::use_condaenv()`](https://rstudio.github.io/reticulate/reference/use_python.html)
or
[`reticulate::use_python()`](https://rstudio.github.io/reticulate/reference/use_python.html),
or an activated virtualenv (`VIRTUAL_ENV`); or a Python reticulate has
already started in this session. If it lacks ogquery, the error says how
to install it there. opengwasr never swaps it for another: if reticulate
can't start the Python you chose and starts a lower choice, its
automatic one or one it found itself instead, `og_local()` refuses.
Otherwise (and with `RETICULATE_PYTHON=managed` or
`RETICULATE_USE_MANAGED_VENV=true`, which ask for it) reticulate sets
one up automatically with uv
(`reticulate::py_require("ogquery>=0.1.0,<1")`), which needs the
internet the first time.

opengwasr reads ogquery `>=0.1.0,<1`. An ogquery outside that range, or
without `ogquery.release` (such as the placeholder 0.0.0 on PyPI), is
refused before anything is read.

One gap: a `use_virtualenv()`, `use_condaenv()` or `use_python()` call
can't be seen before Python starts, so opengwasr declares ogquery with
`py_require()` as for no choice. Once Python has started, reticulate's
`py_config()` names the call (`forced`), and opengwasr reports it
(`chosen_by = "use_python"`). But if reticulate can't start that
environment and starts its automatic one, `py_config()` names only the
automatic one, and opengwasr can't tell. Set `RETICULATE_PYTHON` to
close the gap.

Without reticulate, `og_local()` offers to install it, or, when R isn't
interactive, stops with `opengwasr_local_unavailable`, as it does
whenever no Python with ogquery can be found. That error carries
`e$need` (`"reticulate"`, `"python"`, `"ogquery"`, `"ogquery_version"`
or `"chosen_python"`), `e$found` (for `"ogquery_version"`, the ogquery
version found, or `NA` for none), `e$required` (the ogquery versions
opengwasr supports, `">=0.1.0,<1"`), `e$python`, `e$chosen_by` (how the
Python was chosen: `"RETICULATE_PYTHON"`, `"RETICULATE_PYTHON_ENV"`,
`"use_python"`, `"VIRTUAL_ENV"`, `"automatic"` or `"reticulate"`, one
reticulate found itself), `e$started_by` (for `"chosen_python"`, how the
Python reticulate started instead was chosen), `e$install` (the command
that installs what is missing, when there is one; for an unsupported
ogquery in reticulate's automatic Python, the command that clears uv's
cached ogquery, after which restarting R sets up a supported one) and
`e$setting` (the setting that names a Python).
[`og_local_check()`](https://opengwas.github.io/opengwasr/reference/og_local_check.md)
reports what was found.

## LD resource

A Local Source carries the LD resource that answers LD for it (ADR 0006;
see
[`vignette("local-source")`](https://opengwas.github.io/opengwasr/articles/local-source.md),
"Clumping and LD"), and the LD is chosen there and nowhere else. Today
that is a **PLINK Reference**: `bfile` names a PLINK binary fileset
(`.bed`, `.bim`, `.fam`) on your machine, and `plink` the PLINK binary
that reads it. `og_local()` checks the fileset (class
`opengwasr_plink_reference_missing`, with `e$files`) and finds PLINK.
[`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
then clumps on the fly with it (`r2` and `kb` are required: a PLINK
Reference has no recorded defaults, class
`opengwasr_plink_settings_required`), and
[`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md)
and
[`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md)
read it. Without an LD resource, anything that needs LD is refused
(class `opengwasr_ld_resource_required`), naming the LD resources in
`e$forms` (`"bfile"` and `"api_two_step"`: the API's LD Stores, by
clumping on an API Source you name,
`og_clump(og_api(), og_tophits(src, ids, clump = FALSE))`) and, when
clumping, `clump = FALSE` (`e$clump_false`): a Local Source never calls
the API itself.

Variants are matched to a PLINK Reference by rsID, because PLINK
References are usually on GRCh37 while ALIDs are on GRCh38: from a data
frame's `rsid` column, else looked up in the Source's Store Releases. A
variant with no rsID, or whose rsID isn't in the `.bim`, is left out,
and a message (class `opengwasr_plink_unmatched`, with `count`) says how
many. PLINK 1.9 and PLINK 2 (2.0.0-a.6.0 or later) both work, and give
the same answer: each is asked for the same thing in its own words (an
LD matrix of r with the reference's allele order kept:
`--r square --keep-allele-order` in 1.9, `--r-unphased square ref-based`
in 2; clumping with `--clump`, read from `.clumped` or `.clumps`). Its
version is read once, here (`--version`), and kept on the PLINK
Reference
([`og_ld_resource()`](https://opengwas.github.io/opengwasr/reference/og_ld_resource.md))
and in the provenance of what it computes (`plink_version`, beside
`plink_bin`). A `plink` that is any other PLINK, such as 1.07 or an
earlier PLINK 2, or a program that isn't PLINK, is refused (class
`opengwasr_plink_unsupported`, with `version`, the version found or
`NA`, `supported`, and `suggest`, a supported `"plink"` or `"plink2"` on
your `PATH`, or `NULL`).

**An LD-only Local Source.** `og_local(bfile = )` with no `gwas` holds
no Store Releases and needs no Python. It answers
[`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md)
and
[`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md)
for data you already have, such as top hits from the API Source, when
PLINK can match the variants by rsID: a data frame with an `rsid`
column, or rsIDs. ALIDs would need a GWAS Store to look up their rsIDs,
so they are refused (class `opengwasr_gwas_store_required`, with the
ALIDs in `e$alids`), naming the ways round in `e$forms`: `"gwas"`
(`og_local(gwas = , bfile = )`), `"rsid_column"` (a data frame with an
`rsid` column) and `"api_rsids"` (their rsIDs looked up first with
`og_variants_chrpos(og_api(), positions)`). Every verb that reads
Associations or Analyses refuses it with the same class, and `e$forms`
`"gwas"`.

`ld_store` (a downloaded LD Store) and `geno_store` (a Genotype Store)
are refused until opengwasr can read them (classes
`opengwasr_ld_resource_unavailable` and `opengwasr_local_unsupported`,
with `e$resource`). Giving more than one kind of LD resource is refused
(class `opengwasr_ld_resource_mixed`, with `e$given`), and so is `plink`
without `bfile`. An error about `bfile` or `plink` names it in `e$arg`.

## Inheriting

`og_local(inherit = src, ...)` builds a Local Source like `src`: every
argument given replaces the inherited one, and the rest come from `src`.
Without `gwas`, its Store Releases are `src`'s, not opened again. Giving
any of `plink`, `bfile`, `ld_store` and `geno_store` replaces `src`'s LD
resource whole, never mixed with it:
`og_local(inherit = src, bfile = ref)` gives `src`'s Store Releases a
PLINK Reference, and `og_local(inherit = src, bfile = NULL)` drops it.
The one exception is `plink` alone:
`og_local(inherit = src, plink = "plink2")` keeps `src`'s PLINK
Reference's fileset and changes the PLINK that reads it (`plink = NULL`
finds one again). `og_local(inherit = src, gwas = NULL)` drops the Store
Releases and keeps the LD resource: an LD-only Local Source. What is
given is checked as for any other Local Source. Anything but a Local
Source as `inherit` is refused (class `opengwasr_argument`).

## See also

[`og_local_check()`](https://opengwas.github.io/opengwasr/reference/og_local_check.md),
[`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md).

## Examples

``` r
stores <- system.file("stores", package = "opengwasr")
# GWAS Store directories, and, for hyb, which holds two, a Store Release directory
src <- og_local(c(file.path(stores, c("dense", "ragged")), file.path(stores, "hyb", "v1")))
src
#> <og_local_source>
#> • Store Releases: 3
#> • s/r: /home/runner/work/_temp/Library/opengwasr/stores/dense/r
#> • residual-rag/comp: /home/runner/work/_temp/Library/opengwasr/stores/ragged/comp
#> • hyb/v1: /home/runner/work/_temp/Library/opengwasr/stores/hyb/v1
#> • LD resource: none
# With a PLINK Reference as its LD resource, where PLINK is installed:
bfile <- file.path(system.file("plink", package = "opengwasr"), "ref")
ld_only <- tryCatch(og_local(bfile = bfile), error = function(e) NULL)
if (!is.null(ld_only)) {
  og_ld_matrix(ld_only, c("rs5000", "rs1000"))
  src <- og_local(inherit = src, bfile = bfile)
  og_tophits(src, "s:bmi_irn", r2 = 0.001, kb = 10000)
}
#> Warning: LD from a PLINK Reference is computed from its genotypes without adjusting for population structure (principal components), unlike OpenGWAS LD Stores and Genotype Stores, so it may be less accurate.
#> ℹ PLINK Reference: /home/runner/work/_temp/Library/opengwasr/plink/ref.
#> This warning is displayed once per session.
#> ℹ 6 of 8 top hits matched the PLINK Reference by rsID; 2 not in it were left out.
#>          id store_id analysis_id analysis_label       alid   rsid chromosome
#> 1 s:bmi_irn        s     bmi_irn        BMI_IRN 1:5000:C:G rs5000          1
#> 2 s:bmi_irn        s     bmi_irn        BMI_IRN 1:5000:A:C rs5000          1
#> 3 s:bmi_irn        s     bmi_irn        BMI_IRN  2:300:C:T rs1005          2
#> 4 s:bmi_irn        s     bmi_irn        BMI_IRN 1:9000:A:G rs1004          1
#>   position effect_allele other_allele  eaf        beta         se  z
#> 1     5000             C            G 0.75  0.08001709 0.01000214  8
#> 2     5000             A            C 0.30  0.07001495 0.01000214  7
#> 3      300             C            T 0.20  0.07001495 0.01000214  7
#> 4     9000             A            G 0.40 -0.06001282 0.01000214 -6
#>           pval effect_scale     n sample_size_kind ncase ncontrol
#> 1 1.244192e-15           sd 5e+05             <NA>    NA       NA
#> 2 2.559625e-12           sd 5e+05             <NA>    NA       NA
#> 3 2.559625e-12           sd 5e+05             <NA>    NA       NA
#> 4 1.973175e-09           sd 5e+05             <NA>    NA       NA
#>   association_status release_id
#> 1           observed          r
#> 2           observed          r
#> 3           observed          r
#> 4           observed          r
#>                                              clump_ld clump_ld_choice clump_r2
#> 1 /home/runner/work/_temp/Library/opengwasr/plink/ref            user    0.001
#> 2 /home/runner/work/_temp/Library/opengwasr/plink/ref            user    0.001
#> 3 /home/runner/work/_temp/Library/opengwasr/plink/ref            user    0.001
#> 4 /home/runner/work/_temp/Library/opengwasr/plink/ref            user    0.001
#>   clump_kb clump_size
#> 1    10000          2
#> 2    10000          2
#> 3    10000          2
#> 4    10000          1
```
