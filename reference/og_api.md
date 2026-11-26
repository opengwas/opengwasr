# Create the API Source

The API Source answers queries through the OpenGWAS API. Every query
function takes a Source as its first argument.

## Usage

``` r
og_api(
  url = getOption("opengwasr.url"),
  timeout = default_timeout(),
  max_in_flight = default_max_in_flight(),
  caller = NULL,
  ld_store = NULL,
  geno_store = NULL,
  inherit = NULL
)
```

## Arguments

- url:

  The API's base URL. Defaults to the `opengwasr.url` option, then
  `OPENGWAS_URL`.

- timeout:

  Seconds to wait for any one call before giving up. Defaults to the
  `opengwasr.timeout` option, then 3600.

- max_in_flight:

  How many Jobs one call may run at once. Defaults to the
  `opengwasr.max_in_flight` option, then 4; at most 100, the Jobs one
  user may have queued.

- caller:

  A package or tool calling opengwasr on its users' behalf, to name
  itself in the user agent of every request this Source makes, so the
  API's logs can tell which tool made it:
  `og_api(caller = "TwoSampleMR/0.8.0")` sends
  `opengwasr/<version> TwoSampleMR/0.8.0`. One short string of printable
  characters (at most 100), `name/version` by convention; `NULL` (the
  default) adds nothing.

- ld_store:

  The Source's LD resource: an LD Store's name, such as
  `"hgdp1kgp-v2-EUR-k10"`, or an ancestry code, such as `"EUR"`, for
  that ancestry's LD Store (an exact name wins). Every verb that needs
  LD
  ([`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
  when it clumps,
  [`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md),
  [`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md),
  [`og_ld_proxies()`](https://opengwas.github.io/opengwasr/reference/og_ld_proxies.md),
  [`og_ld_reflookup()`](https://opengwas.github.io/opengwasr/reference/og_ld_reflookup.md))
  uses it: the LD is chosen on the Source, and nowhere else. `NULL` (the
  default): each Analysis's Assigned Ancestry chooses its LD Store. It
  is checked against
  [`og_ld_stores()`](https://opengwas.github.io/opengwasr/reference/og_ld_stores.md)
  when a call first needs it. See
  [`vignette("ld")`](https://opengwas.github.io/opengwasr/articles/ld.md).

- geno_store:

  A Genotype Store, from which LD would be computed when asked. Not
  available yet: anything but `NULL` is refused (class
  `opengwasr_ld_resource_unavailable`) until the API serves Genotype
  Stores.

- inherit:

  An API Source to build this one like: every argument given replaces
  the inherited one, and the rest (`url` and the key it was made with,
  `timeout`, `max_in_flight`, `caller`, and the LD resource) come from
  `inherit`. Giving `ld_store` or `geno_store` replaces the inherited LD
  resource whole. So other LD for one call is
  `og_api(inherit = src, ld_store = "AFR")`. The arguments are checked
  as for any other Source. Anything but an API Source is refused (class
  `opengwasr_argument`).

## Value

An `og_api_source`.

## Details

Credentials come from the environment: `OPENGWAS_URL` and
`OPENGWAS_DEV_KEY`, with `OPENGWAS_DEV_USER` naming the user (default
`opengwasr-dev`). This developer access reaches the Data API directly
and is for developing opengwasr, not for analyses. The key is never
printed.

## Examples

``` r
if (FALSE) { # \dontrun{
src <- og_api()
src
og_api(ld_store = "EUR")
og_api(inherit = src, ld_store = "AFR")  # src's URL, key, timeout; AFR LD
} # }
```
