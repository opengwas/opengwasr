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

## Pull requests

- One purpose per pull request. Name the issue (`Closes #n`).
- Every check must pass: `R CMD check --as-cran` on Linux, macOS and Windows with no errors, warnings or notes, and the CLA.
- Update the documentation when behaviour changes.

## Licence

By contributing, you agree that your contribution is licensed as described in the CLA you signed. opengwasr is released under GPL-3.0-only ([LICENSE.md](LICENSE.md)).
