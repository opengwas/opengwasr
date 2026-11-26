# Check the setup of a Local Source

Reports the Python a Local Source uses and how it was chosen, the
reticulate and ogquery versions, the format series ogquery reads, and,
for each path, the Store Release found there (for a GWAS Store
directory, the one it resolved to), its `store_id`, `release_id` and
`format_version`, and whether ogquery can read it. It reports problems
instead of raising them, so it is the place to start when
[`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)
refuses: only a `path` that isn't a character vector of paths, or a
Local Source, is an error.

## Usage

``` r
og_local_check(path)
```

## Arguments

- path:

  The directory of each GWAS Store or Store Release, as for
  [`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md),
  or a Local Source.

## Value

An `og_local_check`: a list with

- `python` (the Python's path) and `chosen_by` (how it was chosen, as
  for
  [`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)'s
  errors: `"RETICULATE_PYTHON"`, `"use_python"`, `"automatic"`, ...);

- `reticulate` and `ogquery`, their versions (`ogquery` is the one
  found, even when opengwasr doesn't support it);

- `ogquery_required`, the ogquery versions opengwasr supports
  (`">=0.1.0,<1"`), and `ogquery_in_range`, whether the ogquery found is
  one of them (`NA` when none is found);

- `reads`, the format series ogquery reads, such as `"0.3"` (any 0.3.x);

- `releases`, a data frame with one row per path: `path` (as given),
  `release_path` (the Store Release directory it resolved to: itself for
  a Store Release directory, the one a GWAS Store directory holds, or
  `NA` when it holds none or several, or the path doesn't exist or isn't
  a directory), `store_id`, `release_id`, `format_version`, `readable`,
  `problem` (`NA`, `"not_release"`, `"several"`, `"format"` or
  `"unavailable"`) and `reason`;

- `duplicates`, the GWAS Stores given more than one Store Release;

- `unavailable`, the `opengwasr_local_unavailable` condition
  [`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)
  would raise, or `NULL`;

- `problems`, all of that as sentences, for printing;

- `ld`, the Local Source's PLINK Reference (as
  [`og_ld_resource()`](https://opengwas.github.io/opengwasr/reference/og_ld_resource.md)
  gives it), or `NULL` (paths given, or none).

Missing values are `NA`. An LD-only Local Source (`og_local(bfile = )`)
has no Store Releases, so no Python is looked for: `releases` has no
rows, `python`, `ogquery` and `reads` are `NA`, and it reports its PLINK
Reference (its fileset, and the PLINK binary and version).

## Details

Like
[`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md),
it finds Python on first use, so it sets Python up automatically when
you haven't chosen one.

## Examples

``` r
stores <- system.file("stores", package = "opengwasr")
og_local_check(file.path(stores, c("dense", "hyb")))
#> <og_local_check>
#> Python:     /opt/hostedtoolcache/Python/3.13.18/x64/bin/python (chosen by RETICULATE_PYTHON)
#> reticulate: 1.47.0
#> ogquery:    0.4.0, reads format 0.3.x
#> 
#> Store Releases:
#>   /home/runner/work/_temp/Library/opengwasr/stores/dense
#>     resolved to /home/runner/work/_temp/Library/opengwasr/stores/dense/r
#>     s/r, format 0.3.0: readable
#>   /home/runner/work/_temp/Library/opengwasr/stores/hyb
#>     not readable: 2 Store Releases one level down (hyb/v1 in v1, hyb/v1-completed in v1c); a GWAS Store directory holds one
```
