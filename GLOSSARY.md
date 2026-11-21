# opengwasr

The R interface to OpenGWAS data: it queries Stores and LD Stores either through the OpenGWAS API or from local copies, and hands the results to analytical tools such as TwoSampleMR. Data terms are taken unchanged from opengwasdb and ogld; this glossary defines them only as far as opengwasr users meet them.

## Data (from opengwasdb and ogld)

**Analysis**:
One statistical analysis of one Trait, producing Associations between that Trait and Variants.
_Avoid_: GWAS, study, dataset, GWAS id

**Association**:
The statistics linking one Variant to one Analysis.
_Avoid_: hit, result, row

**Store**:
A self-contained distribution unit holding one or more Analyses.
_Avoid_: batch, database, dataset

**Store Release**:
An immutable, published version of a Store; every Association a user receives comes from exactly one.
_Avoid_: version, snapshot

**Stored Effect Scale**:
The scale an Analysis's effects are expressed on: SD units for continuous traits, log odds for binary ones.
_Avoid_: units, original scale

**Reference-Completed**:
Describes a Store Release that, beyond the Associations its sources reported, holds Imputed Associations for the variants they did not report.
_Avoid_: imputed store, filled

**Association Status**:
Whether an Association is Observed (reported by the source), Imputed (added by Reference Completion) or Missing.
_Avoid_: proxy, source flag

**LD Store**:
A distribution unit of linkage-disequilibrium reference data for one ancestry, used for clumping, proxies and LD matrices.
_Avoid_: LD panel, LD reference, bfile

**Assigned Ancestry**:
The single super-population label of an Analysis, inferred from its allele frequencies; an Analysis that matches no single ancestry confidently is Unassigned.
_Avoid_: population, reported ancestry

**PLINK Reference**:
A user-supplied PLINK binary genotype fileset used for LD operations on the user's own machine.
_Avoid_: bfile, LD Store, LD panel

**LD Reference Panel**:
opengwasdb's internal LD data used to complete Stores at build time; never queried by opengwasr users.
_Avoid_: LD Store

## Access

**Source**:
Where a query is answered: either the API Source or a Local Source. Every query names one.
_Avoid_: backend, connection, server

**API Source**:
The OpenGWAS API, reached over the network, suited to small queries.

**Local Source**:
Store Releases and LD Stores downloaded to the user's own machine, suited to large queries.
_Avoid_: offline mode, local files

**Job**:
A query submitted to the API Source that is queued and later collected, rather than answered immediately.
_Avoid_: request, task

**Store Reader**:
The single library through which every consumer reads a Store Release.
_Avoid_: query engine, opengwasdb (for the reading part)
