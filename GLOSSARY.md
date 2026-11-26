# opengwasr

The R interface to OpenGWAS data: it queries GWAS Stores and LD Stores
through the OpenGWAS API or from Store Releases you have downloaded, and
hands the results to analytical tools such as TwoSampleMR. Data terms
are taken unchanged from opengwasdb and ogld, and terms that cross
repositories from og-dev-hub’s `CONTEXT.md`; this glossary defines them
only as far as opengwasr users meet them.

## Data (from opengwasdb and ogld)

**Analysis**: One statistical analysis of one Trait, producing
Associations between that Trait and Variants. *Avoid*: GWAS, study,
dataset, GWAS id

**Association**: The statistics linking one Variant to one Analysis.
*Avoid*: hit, result, row

**GWAS Store**: Short for OpenGWAS GWAS Store: a published, versioned
collection of harmonised GWAS summary statistics holding one or more
Analyses, with an `OGS-*` id (opengwasdb’s “Store”). *Avoid*: bare
“Store”, batch, database, dataset

**Store Release**: An immutable, published version of a GWAS Store;
every Association a user receives comes from exactly one. *Avoid*:
version, snapshot

**Stored Effect Scale**: The scale an Analysis’s effects are expressed
on: SD units for continuous traits, log odds for binary ones. *Avoid*:
units, original scale

**ALID**: Short for Alphabetic Variant ID: a variant’s identity,
`chromosome:position:A1:A2` on GRCh38: alleles of one length in
alphabetical order, an indel’s reference allele first. `A1` is the
effect allele. *Avoid*: SNP id, variant id, chrpos

**Rho Matrix**: The null correlation (sample overlap times phenotypic
correlation) between every pair of Analyses in a GWAS Store with a Dense
Layout. *Avoid*: genetic correlation, overlap matrix

**Indexed Variant Subset**: A named, standard set of variants, such as
HapMap3, at which a GWAS Store holds every Analysis’s Associations for
fast extraction. *Avoid*: SNP list, projection

**Reference-Completed**: Describes a Store Release that, beyond the
Associations its sources reported, holds Imputed Associations for the
variants they did not report. *Avoid*: imputed store, filled

**Association Status**: Whether an Association is Observed (reported by
the source), Imputed (added by Reference Completion) or Missing.
*Avoid*: proxy, source flag

**LD Store**: Short for OpenGWAS LD Store: the published LD of one
reference panel of one ancestry on one Reference Assembly, used for
clumping, LD Proxies and LD matrices. Not a kind of GWAS Store. *Avoid*:
bare “Store”, LD panel, LD reference, bfile

**Far Threshold** (`t_far`): The r² an LD Store’s pairs 3 to 10 Mb apart
must reach for it to hold their r; below it, it knows only that r² is
below `t_far`. *Avoid*: r² threshold, clumping threshold (both name a
request’s `r2`)

**LD Proxy**: A variant in LD with a given variant, found by looking it
up in an LD Store. opengwasr never substitutes one’s Association for a
missing Association. *Avoid*: proxy Association

**Assigned Ancestry**: The single super-population label of an Analysis,
inferred from its allele frequencies; an Analysis that matches no single
ancestry confidently is Unassigned. *Avoid*: population, reported
ancestry

**PLINK Reference**: A user-supplied PLINK binary genotype fileset used
for LD operations on the user’s own machine. Its LD is computed from its
genotypes without adjusting for population structure (principal
components), unlike an LD Store’s or a Genotype Store’s, so it may be
less accurate. *Avoid*: bfile, LD Store, LD panel

**Genotype Store**: Short for OpenGWAS Genotype Store: the published
genotypes of one reference panel of one ancestry, from which LD is
computed when it is asked for, rather than read precomputed as from an
LD Store. Neither the API nor opengwasr serves one yet. *Avoid*:
genotype panel, bfile, LD Store

**LD resource**: Where a Source’s LD comes from: an LD Store, a Genotype
Store or a PLINK Reference, on the API or on the user’s machine. A
Source carries at most one (`og_api(ld_store = )`,
`og_local(bfile = )`); without one, the API Source lets each Analysis’s
Assigned Ancestry choose its LD Store, and a Local Source has none. It
is chosen only on the Source: other LD for one call is another Source
(`og_api(inherit = src, ld_store = )`). *Avoid*: LD reference, LD panel,
LD source

**Clumps**: The index variants of an Analysis’s top hits, each standing
for the weaker top hits in LD with it, together with the configuration
that made them: the LD resource, `r2` and `kb`. Stored Clumps are made
when a Store Release is built: its 5e-8 top hits, clumped with each
ancestry’s default LD resource and its `r2` and `kb`, and used only when
a request matches that configuration exactly. Any other Clumps are
computed on the fly. Provenance says which. *Avoid*: clumped hits,
independent hits, pruned variants

**LD Reference Panel**: opengwasdb’s internal LD data used to complete
GWAS Stores at build time; never queried by opengwasr users. *Avoid*: LD
Store

## Access

**Source**: Where a query is answered: either the API Source or a Local
Source. Every query names one. A Source also carries the LD resource
that answers LD for its Associations. *Avoid*: backend, connection,
server

**API Source**: The OpenGWAS API, reached over the network, suited to
small queries.

**Legacy OpenGWAS**: The OpenGWAS service in production today, which
ieugwasr calls, until OpenGWAS replaces it. *Avoid*: the old API, bare
“OpenGWAS” for that service

**Local Source**: Store Releases downloaded to the user’s own machine,
suited to large queries. One made with only a PLINK Reference
(`og_local(bfile = )`) answers LD only, for data the user already has.
*Avoid*: offline mode, local files

**GWAS Store directory**: The directory holding one GWAS Store’s Store
Release, one level down, as
[`og_local()`](https://opengwas.github.io/opengwasr/reference/og_local.md)
reads it: the directory there holding a `manifest.json` (hidden
directories skipped). Several Store Releases there are refused, never
chosen between. *Avoid*: store folder, release root

**Job**: A query submitted to the API Source that is queued and later
collected, rather than answered immediately. *Avoid*: request, task

**Store Reader**: The single library through which every consumer reads
a Store Release. That library is ogquery. *Avoid*: query engine,
opengwasdb (for the reading part)
