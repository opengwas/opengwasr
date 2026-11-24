# Check the API and your access

`og_status()` reports whether the API is up and the limits it applies.
`og_user()` reports who you are signed in as, and your allowance.

## Usage

``` r
og_status(src)

og_user(src)
```

## Arguments

- src:

  A Source, from
  [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md).

## Value

A list (class `og_status` or `og_user`, with a print method). Read its
fields in scripts: `status$ready`, `status$execution$limits`;
`user$user`, `user$tier`, `user$allowance`, `user$concurrency_budget`.

## Examples

``` r
if (FALSE) { # \dontrun{
src <- og_api()
og_status(src)
og_user(src)
} # }
```
