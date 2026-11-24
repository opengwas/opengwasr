# Run queries as Jobs without waiting

`og_submit()` starts a query and returns as soon as the API has accepted
its Jobs, so you can start many and collect them yourself. It takes a
query function (such as
[`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md))
and that function's arguments after the Source. `og_job_status()` looks
at Jobs without waiting; `og_job_result()` waits for them and returns
what the query function would have returned, with the same columns and
[`og_provenance()`](https://opengwas.github.io/opengwasr/reference/og_provenance.md);
`og_job_cancel()` cancels them.

## Usage

``` r
og_submit(src, fn, ...)

og_job_status(src, job)

og_job_result(src, job, raw = FALSE)

og_job_cancel(src, job)
```

## Arguments

- src:

  A Source, from
  [`og_api()`](https://opengwas.github.io/opengwasr/reference/og_api.md).

- fn:

  A query function, such as
  [`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md).

- ...:

  Its arguments after `src`.

- job:

  An `og_jobs` object from `og_submit()`; for `og_job_status()` and
  `og_job_cancel()` also Job ids.

- raw:

  For `og_job_result()` with a bare Job id: return the API's unshaped
  rows.

## Value

`og_submit()`: an `og_jobs` object. `og_job_status()`,
`og_job_cancel()`: a data frame with `job_id`, `status` and
`queue_position`. `og_job_result()`: the query function's result.

## Details

The query function runs as usual up to submitting its Jobs: its argument
checks and lookups of Analyses (and any Job it needs to prepare, such as
resolving rsIDs) happen in `og_submit()`, and finish before it returns.
What the result is shaped from is fixed then, so `og_job_result()`
returns the same answer however much later you collect it. A query that
needs several Jobs gives one `og_jobs` object holding all of them.

`og_job_status()` and `og_job_cancel()` take the `og_jobs` object or Job
ids (text). `og_job_result()` needs the `og_jobs` object: a bare Job id
doesn't say how its rows are to be shaped, and it refuses
(`opengwasr_argument`). Rerun the same call instead (it reattaches to
the Job), or pass `raw = TRUE` for the API's rows exactly as they are,
of class `og_job_raw`, without opengwasr's columns or provenance.

Pressing Esc or Ctrl-C while a query function waits doesn't cancel its
Jobs. The error (`opengwasr_interrupted`) carries their ids
(`e$job_ids`) and the `og_jobs` object (`e$jobs`) to collect with
`og_job_result()`. A result the API has deleted (already collected, or
expired) raises `opengwasr_result_expired`.

## Examples

``` r
if (FALSE) { # \dontrun{
src <- og_api()
job <- og_submit(src, og_tophits, "OGS-00011:GCST003116", clump = FALSE)
og_job_status(src, job)
top <- og_job_result(src, job)
} # }
```
