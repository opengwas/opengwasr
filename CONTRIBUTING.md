# Contributing to opengwasr

## Before you start

- **Sign the Contributor License Agreement.** Every contribution from outside the University of Bristol needs one; see [CLA.md](CLA.md). The CLA bot asks you to sign on your first pull request.
- **Find or open an issue.** For anything larger than a small fix, discuss it in an issue first.
- **Read** [GLOSSARY.md](GLOSSARY.md) and [docs/adr/](docs/adr/). Use the glossary's terms in code, documentation and issues. One principle matters above all: opengwasr never silently returns a different scientific answer from the one asked for. An option is either honoured or refused with a clear error.

## How work proceeds: tutorial-driven development

opengwasr is built tutorial-first (ADR 0005). The vignettes say how the package is used. The code is built to make them true, never the other way round.

1. **The tutorial comes first.** Each epic starts by writing its vignettes, one per user journey, with every chunk `eval = FALSE`. They fix the interface users will rely on: function names, arguments, columns, messages and refusals. They are reviewed before any implementation begins.
2. **Each ticket names the chunks it makes true**, by chunk label. It builds what those chunks need, turns them on (`eval = FALSE` → evaluated), and adds testthat tests at the Source seam that make the same calls and assert on the answers. A chunk that runs proves the code runs as the tutorial says. A test proves the answer is right. A ticket is done when its chunks evaluate in CI and its tests pass.
3. **When the code and the tutorial disagree, change the tutorial first.** If building a chunk shows that the interface it describes is wrong, change the vignette in its own commit, say why in the pull request, and have it reviewed. Then build to the new chunk. Never change the code's interface and leave the vignette describing the old one.
4. **Real output, not illustration.** A chunk that evaluates shows the output it produced. Remove any hand-written output for it. `eval = FALSE` stays only on a chunk that can't run in CI, and the vignette's Status block or the chunk's text says why: shell setup, a resource CI lacks (a PLINK binary, a genotype fileset), or behaviour a named later ticket builds. Hand-written output under such a chunk is marked `# illustrative`, and is shaped exactly like what opengwasr returns.
5. **Offline everywhere.** API vignettes replay httptest2 recordings of the real API. Local Source vignettes read the example Store Releases bundled in the package, and evaluate wherever Python and ogquery are available (in CI, with `RETICULATE_PYTHON`). On CRAN they show the code unevaluated. Unit tests run against the fake Data API in `tests/testthat`. None of these touches the network on CRAN or in CI.
6. Errors are classed conditions, and tests assert on the class, not on the message text. Comparing a condition's message with the API's own message (from `fixtures/errors.json` or the raw API) is allowed: it tests that the message was passed on, not how it is worded. An API error keeps the API's message and takes its class from the API's code; opengwasr checks only what the API can't ([ADR 0002, clarification of 22 November 2026](docs/adr/0002-one-set-of-verbs-dispatching-on-an-explicit-source.md)).

## Testing against the fake Data API

Unit tests talk to a fake Data API, `fake_api()` in `tests/testthat/fake-server.R`: its routes, scenarios, limits, recorded request bodies and error bodies are defined there once. It has two modes, and the same app serves both.

- **In-process (the default).** `fake <- local_fake_api()` once at the top of a test file. Requests that opengwasr's transport makes to `fake$url()` are answered by the fake's own handlers in the test's R process (`fake_dispatch()` in `helper-fake-api.R`, through `httr2::local_mocked_responses()`): no socket, no second process, about 1 ms a request. Write tests as you would for any other: `test_api("...", { ... src ... })` binds `src` to a Source for the fake (and for the dev Data API too when `.Renviron` provides it); `fake_set(fake, "paging", "offset", "cursor")` switches a scenario on for one test; `raw_api(fake$url(), fake_key, "_fake/bodies")` reads what the fake recorded. To change what the fake does, add a route or a scenario in `fake-server.R`, and both modes get it. The in-process response object implements only the `res` methods the handlers use (`set_status()`, `set_header()`, `set_type()`, `send()`, `send_json()`, `send_status()`); a handler that needs another fails loudly, and you add it to `fake_dispatch()`.
- **Socket (`local_fake_api(socket = TRUE)`).** The same app served by webfakes in a second R process, over a real socket. Use it only for a test that is about the transport: what curl does with a per-request timeout, a connection that can't be made or breaks, an interrupt in the middle of a request. These live in `test-transport.R` (or beside their subject, with a comment saying why). A test that merely needs a slow or failing answer doesn't: the fake's `delay_ms` and error scenarios work in-process, and the deadline logic runs on the virtual clock (`local_virtual_clock()`).

Not covered by any test yet: a connection that drops mid-response, and an interrupt in the middle of a request (the interrupt is tested only with mocked Jobs).

Test files run in parallel (`Config/testthat/parallel: true`), so a file must not depend on state another file leaves behind; each starts its own fake. testthat uses 2 workers by default; set `TESTTHAT_CPUS` for more locally. Live tests share the dev Data API, so they only read, or submit and clean up their own Jobs.

If an assertion can't pass in-process, it is a transport test: move it to socket mode, don't weaken it. Live tests and the vignettes' httptest2 replays are unaffected.

### Parity with the real Data API

`test-parity.R` checks that the API Source and a Local Source return the same data frame for each query of a golden set, and that each returns what the real Data API would return. Three files in `tests/testthat/fixtures/parity/` hold it:

- **`queries.json`, the golden set.** Each query is an opengwasr call (`verb`, `args`) over a source set of the example Store Releases in `inst/stores` (`source`). It also lists the Data API requests the API Source makes for that call (`requests`): their endpoints and bodies, recorded once from opengwasr's own requests and checked by hand.
- **`generate.py`, the generator.** It answers every request as the real Data API would. It follows ogdb-api's own code, citing its lines, and reads the releases through ogquery's public API. It never calls opengwasr.
- **`answers.json`, its output.** The header names the ogquery version, the ogdb-api commit and the bundle the answers came from.

`local_fake_api(parity = "<source set>")` serves those answers. A request without a generated answer gets a 500 that names it.

Regenerate the answers from the package root, with a Python that has ogquery:

```sh
python -I tests/testthat/fixtures/parity/generate.py
```

Regenerate them, and review the diff, whenever any of these change:

- the golden set;
- the example Store Releases;
- the ogquery version;
- the ogdb-api commit the generator follows. Update `OGDB_API_COMMIT` and the cited lines first.

When opengwasr's requests for a golden query change, the parity tests fail with the fake's 500. Record the new request in `queries.json`, check that its body is what the call means, and regenerate.

A parity failure is a defect in one of the two Sources, or a change in the real API. It is never something to adjust in the fake. When something is known to differ, `test-parity.R` lists it under "Known differences", with the issue that resolves it.

## Adding a query

A query that returns Associations is a thin spec over one engine, `association_query()` in `R/association-query.R`. The engine owns the pipeline: the call's deadline, Store Releases and Analyses, the original-units check, rsID resolution, splitting into Jobs and running them (or only submitting them, under `og_submit()`), combining the pieces, the Jobs' row counts and truncation, shaping the columns, ordering and provenance. A query method checks the arguments only it has, then calls the engine with its endpoint and what is specific to it. It never re-implements a step of that pipeline. `og_tophits()` is the model:

```r
og_tophits.og_api_source <- function(src, analyses, threshold = 5e-8, limit = 1000,
                                     observed_only = FALSE, clump = TRUE, r2 = NULL, kb = NULL,
                                     original_units = FALSE, releases = NULL) {
  check_tophits_arguments(src, threshold, limit, clump, r2, kb)
  association_query(src, "api/tophits", analyses, observed_only, original_units, releases,
                    body = list(threshold = threshold, limit = as.integer(limit)),
                    analyses_per_job = tophits_analyses_per_job(limit), order = "api",
                    truncated = function(ids) truncated_message(analyses_who(ids), limit, "top hits", threshold),
                    spec = list(clump = if (clump) list(r2 = r2, kb = kb, limit = limit, threshold = threshold)))
}
```

What the method can say:

- `body`: the endpoint's own fields, the same in every Job. The engine adds `analyses`, `observed_only` and `releases` (and `variants`) itself, always explicitly, so the API's defaults never apply. A Job's body is `variants`/`variant`, `analyses`, `body`'s fields, any other contributed field, then `observed_only` and `releases`; the order is part of what a vignette recording matches.
- `variants = variants`: the query takes rsIDs and ALIDs. The engine resolves the rsIDs first and splits the ALIDs across Jobs (`og_associations()`).
- `analyses_per_job`: fewer Analyses per Job than the API's limit, for example when each Analysis may return many rows.
- `order`: `"position"` (the default: Analyses in the order asked, then chromosome, position, ALID), `"api"` (the API's own order within each Analysis: top hits, strongest first) or `"jobs"` (rows as the Jobs returned them, for a query that names no Analyses).
- `truncated`: the endpoint cuts rows off at a `limit`. Each Job must then report `total` and `truncated`, and the engine checks both against the rows it read; without it, a Job's `total` is its rows. When a Job was cut off, the engine warns (`opengwasr_truncated`) with the message `truncated(ids)` gives; build it with `truncated_message()`.
- `spec`: one list for what a few queries add, never more named arguments. `all_analyses = TRUE` lets `analyses` be `NULL` (every Analysis of the Store Releases; metadata is looked up once, for the Analyses in the result, in `finish`: the one thing a `finish` may look up, against the pinned Store Releases; an error there is the API's). `one_variant = TRUE` sends one ALID per Job as `variant`. `interval = list(chromosome, start, end, limit)` cuts a range into windows. `clump = list(r2, kb, limit, threshold)` clumps each Analysis's top hits with the Source's LD resource (no verb takes an LD argument: ADR 0006, clarification of 25 November 2026), on the server in its LD Store or, with a Local Source's PLINK Reference, on this machine. `og_phewas()` (`all_analyses`, `one_variant`), `og_range()` (`interval`) and `og_tophits()` (`clump`) are each a few lines over it.

Inside the engine every part of a request is a **contribution**, a list the engine reads the same way whatever made it, so the next spec field adds a contribution, not a branch:

- `dim`, a split dimension (`split_dim()`, `split_interval()`), or `along = "<name>"` to follow another contribution's (clumping follows the Analyses, `"scope"`);
- `body(chunk)`, the fields one piece adds to a Job's body (a field of the endpoint's own `body` is replaced in its place: PLINK clumping fetches the API's most top hits whatever `limit` is);
- optionally `analyses_per_job` (fewer Analyses a Job: clumping on the server takes one), `truncated(ids)` (its own truncation message), `group(chunk)` (Jobs submitted grouped by its value: clumping, by LD Store; the answer keeps piece order) and `finish(rows, results, pieces, call)`, which returns `list(value, provenance, truncated)` from the shaped rows: further columns, provenance fields, and which Jobs it cut rows off (one flag per Job, in piece order).

`clump_contribution()` in `R/clump.R` and `plink_clump_contribution()` in `R/plink.R` are the worked examples.

Provenance records the method's own arguments as the user gave them, read from its frame. So call the engine directly from the method, and don't reassign an argument before the call. An argument the method gains is recorded automatically.

An LD operation (`og_ld_matrix()`, `og_ld_proxies()`, `og_ld_reflookup()`, `og_clump()`) is a thin spec over the LD engine, `ld_query(src, path, variants, spec)` in `R/ld-query.R`. The LD is the Source's (`ld_resource()`). Its `spec`:

- `pieces(n)`: how `n` ALIDs split into Jobs, a list of index vectors, each one Job;
- `body`: its fields, the same in every Job, or `function(alids, store, rows)` of a piece's ALIDs, its LD Store's row of `og_ld_stores()` and the rows of `variants` in its set (clumping's scores and default `r2`);
- optionally `sets`, a vector over the rows of a data frame `variants`: the rows of each value are answered on their own, each set in the LD Store of its own Assigned Ancestry (`og_clump()`'s `id`, unless `by_analysis = FALSE`). Without it, every variant is one set, in one LD Store;
- `assemble(results, ctx)`: its value from the Jobs' `list(rows, job)`, in piece order;
- optionally `provenance(results, ctx)` (further provenance fields: clumping's `clump`), `absence_is_answer = TRUE` (the answer is which variants the LD Store holds, so absent variants are rows, not a warning) and `local(ctx)`, how it reads a Local Source's PLINK Reference, returning `list(value, provenance)`; a query without `local` has a Local Source method that refuses.

`ctx` has `alids`, `absent`, `store`, `choice`, `variants` (what the user passed), `rsid_map` (`rsid` and `alid` of each rsID resolved), `unresolved` and `sets` (each `id`, `rows`, `alids`, `store`, `choice`, and its Jobs' `results` and `absent`); `local` gets `matched` (`alid` and `rsid` of each variant the reference holds), `plink` and `call` (the method's, for its conditions) instead of the Jobs' parts. The LD engine chooses the LD Store (`choose_ld_store()`), resolves rsIDs and warns about absent variants.

Both engines, and the variant lookups (`lookup_query()`), sit on one scaffold, `engine_query()` in `R/query-plan.R`: the method's arguments, the call's deadline, the record of the call, a plan whose finish attaches provenance, and `run_query()`, which keeps `og_submit()` working. A query of a new kind builds on `engine_query()` the same way, following the query-plan contract at the top of `R/query-plan.R`.

## Pull requests

- One purpose per pull request. Name the issue (`Closes #n`).
- Every check must pass: `R CMD check --as-cran` on Linux, macOS and Windows with no errors, warnings or notes, and the CLA.
- Update the documentation when behaviour changes.

## Licence

By contributing, you agree that your contribution is licensed as described in the CLA you signed. opengwasr is released under GPL-3.0-only ([LICENSE.md](LICENSE.md)).
