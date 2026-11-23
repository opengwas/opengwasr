# Contributing to opengwasr

## Before you start

- **Sign the Contributor License Agreement.** Every contribution from outside the University of Bristol needs one; see [CLA.md](CLA.md). The CLA bot asks you to sign on your first pull request.
- **Find or open an issue.** For anything larger than a small fix, discuss it in an issue first.
- **Read** [GLOSSARY.md](GLOSSARY.md) and [docs/adr/](docs/adr/). Use the glossary's terms in code, documentation and issues. One principle matters above all: opengwasr never silently returns a different scientific answer from the one asked for. An option is either honoured or refused with a clear error.

## How work proceeds

opengwasr is built outside-in (ADR 0005):

1. Each epic starts by writing its vignettes, one per user journey, with chunks set to `eval = FALSE`. These are reviewed before implementation begins.
2. Each implementation ticket names the vignette section it satisfies. It turns those chunks on and adds testthat tests at the Source seam that make the same calls and assert on the results.
3. Vignettes run offline from httptest2 recordings of the real API. Unit tests run against the fake Data API in `tests/testthat`. Neither touches the network on CRAN or in CI.
4. Errors are classed conditions, and tests assert on the class, not on the message text. Comparing a condition's message with the API's own message (from `fixtures/errors.json` or the raw API) is allowed: it tests that the message was passed on, not how it is worded. An API error keeps the API's message and takes its class from the API's code; opengwasr checks only what the API can't ([ADR 0002, clarification of 22 November 2026](docs/adr/0002-one-set-of-verbs-dispatching-on-an-explicit-source.md)).

## Adding a query

A query that returns Associations is a thin spec over one engine, `association_query()` in `R/association-query.R`. The engine owns the pipeline: the call's deadline, Store Releases and Analyses, the original-units check, rsID resolution, splitting into Jobs and running them (or only submitting them, under `og_submit()`), combining the pieces, the Jobs' row counts and truncation, shaping the columns, ordering and provenance. A query method checks the arguments only it has, then calls the engine with its endpoint and what is specific to it. It never re-implements a step of that pipeline. `og_tophits()` is the model:

```r
og_tophits.og_api_source <- function(src, analyses, threshold = 5e-8, limit = 1000, observed_only = FALSE,
                                     clump = TRUE, ld = NULL, r2 = NULL, kb = NULL,
                                     original_units = FALSE, releases = NULL) {
  check_tophits_arguments(threshold, limit, clump, ld, r2, kb)
  association_query(src, "api/tophits", analyses, observed_only, original_units, releases,
                    body = list(threshold = threshold, limit = as.integer(limit)),
                    analyses_per_job = if (clump) 1L else tophits_analyses_per_job(limit), order = "api",
                    truncated = function(ids) truncated_message(ids, threshold, limit, clump),
                    spec = list(clump = if (clump) list(ld = ld, r2 = r2, kb = kb)))
}
```

What the spec can say:

- `body`: the endpoint's own fields, the same in every Job. The engine adds `analyses`, `observed_only` and `releases` (and `variants`) itself, always explicitly, so the API's defaults never apply.
- `variants = variants`: the query takes rsIDs and ALIDs. The engine resolves the rsIDs first and splits the ALIDs across Jobs (`og_associations()`).
- `analyses_per_job`: fewer Analyses per Job than the API's limit, for example when each Analysis may return many rows.
- `order`: `"position"` (the default) or `"api"`, to keep the API's own order within each Analysis (top hits, strongest first).
- `order = "jobs"`: rows as the Jobs returned them, for a query that names no Analyses.
- `spec`: one list for what a few queries add, never more named arguments. `all_analyses = TRUE` lets `analyses` be `NULL` (every Analysis of the Store Releases; metadata is looked up once, for the Analyses in the result, in `finish`: the one thing a `finish` may look up, against the pinned Store Releases; an error there is the API's). `one_variant = TRUE` sends one ALID per Job as `variant`. `interval = list(chromosome, start, end, limit)` cuts a range into windows. `clump = list(ld, r2, kb)` clumps each Analysis on the server in its own LD Store (one Analysis per Job). `og_phewas()` (`all_analyses`, `one_variant`), `og_range()` (`interval`) and `og_tophits()` (`clump`) are each a few lines over it. Inside the engine each of these is a contribution: a split dimension (or `along` another contribution's, as clumping follows the Analyses) and the body fields of a piece, and optionally a `finish` that adds columns and provenance to the shaped rows. So the next one adds a contribution, not a branch.
- `truncated`: the endpoint cuts rows off at a `limit`. Each Job must then report `total` and `truncated`, and the engine checks both against the rows it read. When a Job was cut off, the engine warns with the message this function gives.

Provenance records the method's own arguments as the user gave them, read from its frame. So call the engine directly from the method, and don't reassign an argument before the call. An argument the method gains is recorded automatically.

An LD operation (`og_ld_matrix()`, `og_ld_proxies()`, `og_clump()`) is a thin spec over the LD engine, `ld_query()` in `R/ld-query.R`: its endpoint, how its variants split into Jobs, its body fields (a list, or a function of a piece's ALIDs and the LD Store), how its Jobs' rows assemble into its value, and optionally further provenance. The LD engine chooses the LD Store (`choose_ld_store()`), resolves rsIDs and warns about absent variants. Both engines, and the variant lookups, sit on one scaffold, `engine_query()` in `R/query-plan.R`: the method's arguments, the call's deadline, the record of the call, a plan whose finish attaches provenance, and `run_query()`, which keeps `og_submit()` working. A query of a new kind builds on `engine_query()` the same way, following the query-plan contract at the top of `R/query-plan.R`.

## Pull requests

- One purpose per pull request. Name the issue (`Closes #n`).
- Every check must pass: `R CMD check --as-cran` on Linux, macOS and Windows with no errors, warnings or notes, and the CLA.
- Update the documentation when behaviour changes.

## Licence

By contributing, you agree that your contribution is licensed as described in the CLA you signed. opengwasr is released under GPL-3.0-only ([LICENSE.md](LICENSE.md)).
