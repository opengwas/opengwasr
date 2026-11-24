# Create the API Source

The API Source answers queries through the OpenGWAS API. Every query
function takes a Source as its first argument.

## Usage

``` r
og_api(
  url = getOption("opengwasr.url"),
  timeout = default_timeout(),
  max_in_flight = default_max_in_flight(),
  caller = NULL
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
} # }
```
