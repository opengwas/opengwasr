# One set of verbs, dispatching on an explicit Source

Every query function takes a Source (the API Source or a Local Source) and returns the same schema whichever it is given, so TwoSampleMR and other callers never branch on where data comes from. Backend parity is a tested contract. The user picks the Source explicitly: opengwasr never routes a query to a different Source on its own, and when the API refuses a query as too large (`413 request_too_large`) the error says how to download the Store instead. The API Source is pure R with no Python dependency, so installing opengwasr is enough to use it.

Behind the verbs, an API query is a Job: opengwasr submits it, polls at the server's `poll_after_seconds`/`Retry-After` with a capped backoff and overall timeout, and returns the rows, so the default experience is a plain blocking call. Submit, status, result and cancel are also exported for callers that fan out many Jobs. If the user interrupts the wait, the Job is left running and its id printed; resubmitting the same query reattaches to it, because the API deduplicates identical queries.

A Local Source is a directory holding one or more Store Releases. Analyses are named exactly as with the API Source, the latest Release of a Store is used unless one is pinned, and ambiguity is an error. opengwasr does not download Stores until an upstream distribution channel exists.

**Considered:** separate `api_*()`/`local_*()` families (rejected: every caller would duplicate its logic per backend); automatic routing by query size (rejected: hides cost and makes results depend on the environment).
