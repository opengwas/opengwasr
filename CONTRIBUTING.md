# Contributing to opengwasr

## Before you start

- **Sign the Contributor License Agreement.** Every contribution from
  outside the University of Bristol needs one; see
  [CLA.md](https://opengwas.github.io/opengwasr/CLA.md). The CLA bot
  asks you to sign on your first pull request.
- **Find or open an issue.** For anything larger than a small fix,
  discuss it in an issue first.
- **Read**
  [GLOSSARY.md](https://opengwas.github.io/opengwasr/GLOSSARY.md) and
  [docs/adr/](https://opengwas.github.io/opengwasr/docs/adr/). Use the
  glossary’s terms in code, documentation and issues. One principle
  matters above all: opengwasr never silently returns a different
  scientific answer from the one asked for. An option is either honoured
  or refused with a clear error.

## How work proceeds

opengwasr is built outside-in (ADR 0005):

1.  Each epic starts by writing its vignettes, one per user journey,
    with chunks set to `eval = FALSE`. These are reviewed before
    implementation begins.
2.  Each implementation ticket names the vignette section it satisfies.
    It turns those chunks on and adds testthat tests at the Source seam
    that make the same calls and assert on the results.
3.  Vignettes run offline from httptest2 recordings of the real API.
    Unit tests run against the fake Data API in `tests/testthat`.
    Neither touches the network on CRAN or in CI.
4.  Errors are classed conditions, and tests assert on the class, not on
    the message text. Comparing a condition’s message with the API’s own
    message (from `fixtures/errors.json` or the raw API) is allowed: it
    tests that the message was passed on, not how it is worded. An API
    error keeps the API’s message and takes its class from the API’s
    code; opengwasr checks only what the API can’t ([ADR 0002,
    clarification of 22 November
    2026](https://opengwas.github.io/opengwasr/docs/adr/0002-one-set-of-verbs-dispatching-on-an-explicit-source.md)).

## Testing against the fake Data API

Unit tests talk to a fake Data API, `fake_api()` in
`tests/testthat/fake-server.R`: its routes, scenarios, limits, recorded
request bodies and error bodies are defined there once. It has two
modes, and the same app serves both.

- **In-process (the default).** `fake <- local_fake_api()` once at the
  top of a test file. Requests that opengwasr’s transport makes to
  `fake$url()` are answered by the fake’s own handlers in the test’s R
  process (`fake_dispatch()` in `helper-fake-api.R`, through
  [`httr2::local_mocked_responses()`](https://httr2.r-lib.org/reference/with_mocked_responses.html)):
  no socket, no second process, about 1 ms a request. Write tests as you
  would for any other: `test_api("...", { ... src ... })` binds `src` to
  a Source for the fake (and for the dev Data API too when `.Renviron`
  provides it); `fake_set(fake, "paging", "offset", "cursor")` switches
  a scenario on for one test;
  `raw_api(fake$url(), fake_key, "_fake/bodies")` reads what the fake
  recorded. To change what the fake does, add a route or a scenario in
  `fake-server.R`, and both modes get it. The in-process response object
  implements only the `res` methods the handlers use (`set_status()`,
  `set_header()`, `set_type()`, `send()`, `send_json()`,
  `send_status()`); a handler that needs another fails loudly, and you
  add it to `fake_dispatch()`.
- **Socket (`local_fake_api(socket = TRUE)`).** The same app served by
  webfakes in a second R process, over a real socket. Use it only for a
  test that is about the transport: what curl does with a per-request
  timeout, a connection that can’t be made or breaks, an interrupt in
  the middle of a request. These live in `test-transport.R` (or beside
  their subject, with a comment saying why). A test that merely needs a
  slow or failing answer doesn’t: the fake’s `delay_ms` and error
  scenarios work in-process, and the deadline logic runs on the virtual
  clock (`local_virtual_clock()`).

Not covered by any test yet: a connection that drops mid-response, and
an interrupt in the middle of a request (the interrupt is tested only
with mocked Jobs).

Test files run in parallel (`Config/testthat/parallel: true`), so a file
must not depend on state another file leaves behind; each starts its own
fake. testthat uses 2 workers by default; set `TESTTHAT_CPUS` for more
locally. Live tests share the dev Data API, so they only read, or submit
and clean up their own Jobs.

If an assertion can’t pass in-process, it is a transport test: move it
to socket mode, don’t weaken it. Live tests and the vignettes’ httptest2
replays are unaffected.

## Adding a query

A query that returns Associations is a thin spec over one engine,
`association_query()` in `R/association-query.R`. The engine owns the
pipeline: the call’s deadline, Store Releases and Analyses, the
original-units check, rsID resolution, splitting into Jobs and running
them (or only submitting them, under
[`og_submit()`](https://opengwas.github.io/opengwasr/reference/og_submit.md)),
combining the pieces, the Jobs’ row counts and truncation, shaping the
columns, ordering and provenance. A query method checks the arguments
only it has, then calls the engine with its endpoint and what is
specific to it. It never re-implements a step of that pipeline.
[`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
is the model:

``` r

og_tophits.og_api_source <- function(src, analyses, threshold = 5e-8, limit = 1000,
                                     observed_only = FALSE, clump = TRUE, ld = NULL, r2 = NULL,
                                     kb = NULL, original_units = FALSE, releases = NULL) {
  check_tophits_arguments(threshold, limit, clump, ld, r2, kb)
  association_query(src, "api/tophits", analyses, observed_only, original_units, releases,
                    body = list(threshold = threshold, limit = as.integer(limit)),
                    analyses_per_job = tophits_analyses_per_job(limit), order = "api",
                    truncated = function(ids) truncated_message(analyses_who(ids), limit, "top hits", threshold),
                    spec = list(clump = if (clump) list(ld = ld, r2 = r2, kb = kb, limit = limit, threshold = threshold)))
}
```

What the method can say:

- `body`: the endpoint’s own fields, the same in every Job. The engine
  adds `analyses`, `observed_only` and `releases` (and `variants`)
  itself, always explicitly, so the API’s defaults never apply. A Job’s
  body is `variants`/`variant`, `analyses`, `body`’s fields, any other
  contributed field, then `observed_only` and `releases`; the order is
  part of what a vignette recording matches.
- `variants = variants`: the query takes rsIDs and ALIDs. The engine
  resolves the rsIDs first and splits the ALIDs across Jobs
  ([`og_associations()`](https://opengwas.github.io/opengwasr/reference/og_associations.md)).
- `analyses_per_job`: fewer Analyses per Job than the API’s limit, for
  example when each Analysis may return many rows.
- `order`: `"position"` (the default: Analyses in the order asked, then
  chromosome, position, ALID), `"api"` (the API’s own order within each
  Analysis: top hits, strongest first) or `"jobs"` (rows as the Jobs
  returned them, for a query that names no Analyses).
- `truncated`: the endpoint cuts rows off at a `limit`. Each Job must
  then report `total` and `truncated`, and the engine checks both
  against the rows it read; without it, a Job’s `total` is its rows.
  When a Job was cut off, the engine warns (`opengwasr_truncated`) with
  the message `truncated(ids)` gives; build it with
  `truncated_message()`.
- `spec`: one list for what a few queries add, never more named
  arguments. `all_analyses = TRUE` lets `analyses` be `NULL` (every
  Analysis of the Store Releases; metadata is looked up once, for the
  Analyses in the result, in `finish`: the one thing a `finish` may look
  up, against the pinned Store Releases; an error there is the API’s).
  `one_variant = TRUE` sends one ALID per Job as `variant`.
  `interval = list(chromosome, start, end, limit)` cuts a range into
  windows. `clump = list(ld, r2, kb, limit, threshold)` clumps each
  Analysis’s top hits, on the server in its LD Store or, with a PLINK
  Reference, on this machine.
  [`og_phewas()`](https://opengwas.github.io/opengwasr/reference/og_phewas.md)
  (`all_analyses`, `one_variant`),
  [`og_range()`](https://opengwas.github.io/opengwasr/reference/og_range.md)
  (`interval`) and
  [`og_tophits()`](https://opengwas.github.io/opengwasr/reference/og_tophits.md)
  (`clump`) are each a few lines over it.

Inside the engine every part of a request is a **contribution**, a list
the engine reads the same way whatever made it, so the next spec field
adds a contribution, not a branch:

- `dim`, a split dimension (`split_dim()`, `split_interval()`), or
  `along = "<name>"` to follow another contribution’s (clumping follows
  the Analyses, `"scope"`);
- `body(chunk)`, the fields one piece adds to a Job’s body (a field of
  the endpoint’s own `body` is replaced in its place: PLINK clumping
  fetches the API’s most top hits whatever `limit` is);
- optionally `analyses_per_job` (fewer Analyses a Job: clumping on the
  server takes one), `truncated(ids)` (its own truncation message),
  `group(chunk)` (Jobs submitted grouped by its value: clumping, by LD
  Store; the answer keeps piece order) and
  `finish(rows, results, pieces, call)`, which returns
  `list(value, provenance, truncated)` from the shaped rows: further
  columns, provenance fields, and which Jobs it cut rows off (one flag
  per Job, in piece order).

`clump_contribution()` in `R/clump.R` and `plink_clump_contribution()`
in `R/plink.R` are the worked examples.

Provenance records the method’s own arguments as the user gave them,
read from its frame. So call the engine directly from the method, and
don’t reassign an argument before the call. An argument the method gains
is recorded automatically.

An LD operation
([`og_ld_matrix()`](https://opengwas.github.io/opengwasr/reference/og_ld_matrix.md),
[`og_ld_proxies()`](https://opengwas.github.io/opengwasr/reference/og_ld_proxies.md),
[`og_ld_reflookup()`](https://opengwas.github.io/opengwasr/reference/og_ld_reflookup.md),
[`og_clump()`](https://opengwas.github.io/opengwasr/reference/og_clump.md))
is a thin spec over the LD engine,
`ld_query(src, path, variants, ld, spec)` in `R/ld-query.R`. Its `spec`:

- `pieces(n)`: how `n` ALIDs split into Jobs, a list of index vectors,
  each one Job;
- `body`: its fields, the same in every Job, or `function(alids, store)`
  of a piece’s ALIDs and the LD Store’s row of
  [`og_ld_stores()`](https://opengwas.github.io/opengwasr/reference/og_ld_stores.md)
  (clumping’s scores and default `r2`);
- `assemble(results, ctx)`: its value from the Jobs’ `list(rows, job)`,
  in piece order;
- optionally `provenance(results, ctx)` (further provenance fields:
  clumping’s `clump`), `absence_is_answer = TRUE` (the answer is which
  variants the LD Store holds, so absent variants are rows, not a
  warning) and `local(ctx)`, how it reads a PLINK Reference, returning
  `list(value, provenance)`; a query without `local` refuses a PLINK
  Reference.

`ctx` has `alids`, `absent`, `store`, `choice`, `variants` (what the
user passed), `rsid_map` (`rsid` and `alid` of each rsID resolved) and
`unresolved`; `local` gets `matched` (`alid` and `rsid` of each variant
the reference holds) and `plink` instead of the Jobs’ parts. The LD
engine chooses the LD Store (`choose_ld_store()`), resolves rsIDs and
warns about absent variants.

Both engines, and the variant lookups (`lookup_query()`), sit on one
scaffold, `engine_query()` in `R/query-plan.R`: the method’s arguments,
the call’s deadline, the record of the call, a plan whose finish
attaches provenance, and `run_query()`, which keeps
[`og_submit()`](https://opengwas.github.io/opengwasr/reference/og_submit.md)
working. A query of a new kind builds on `engine_query()` the same way,
following the query-plan contract at the top of `R/query-plan.R`.

## Pull requests

- One purpose per pull request. Name the issue (`Closes #n`).
- Every check must pass: `R CMD check --as-cran` on Linux, macOS and
  Windows with no errors, warnings or notes, and the CLA.
- Update the documentation when behaviour changes.

## Licence

By contributing, you agree that your contribution is licensed as
described in the CLA you signed. opengwasr is released under
GPL-3.0-only
([LICENSE.md](https://opengwas.github.io/opengwasr/LICENSE.md)).
