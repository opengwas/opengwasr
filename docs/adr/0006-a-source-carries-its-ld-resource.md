# A Source carries its LD resource; stored clumps by default, clumping on the fly otherwise

A Source says where Associations come from, and also which LD resource answers LD for them. The LD resource is one of three kinds, LD Store, Genotype Store and PLINK Reference, or none is given. Each kind can be on the API or on the user's machine:

| LD resource | On the API | Local |
|---|---|---|
| LD Store (precomputed r) | `og_api(ld_store = "OGL-...")` | `og_local(gwas = ..., ld_store = path)` |
| Genotype Store (LD computed from genotypes) | `og_api(geno_store = "OGL-...")` | `og_local(gwas = ..., geno_store = path)` |
| PLINK Reference (a PLINK binary fileset) | — | `og_local(gwas = ..., plink = path, bfile = prefix)` |
| None given | `og_api()`: each Analysis's Assigned Ancestry chooses its default LD resource | `og_local(gwas = ...)`: none; anything needing LD refuses and names the forms above |

The API's LD resources come from a registry. Each has an `OGL-` id and is specific to one ancestry. The registry also names each ancestry's default. Until it exists, `og_api(ld_store =)` takes today's LD Store names and ancestry codes, as `ld` does. A form whose resource isn't available yet is refused with a classed error saying so, rather than being left out of the interface:
- Genotype Stores on the API;
- reading local LD Stores (ogquery#2);
- local Genotype Stores (no reader yet).

**One place, one override.** The Source's LD resource is the default for every verb that needs LD: `og_tophits()` when clumping, `og_clump()`, `og_ld_matrix()`, `og_ld_proxies()` and `og_ld_reflookup()`. The verb's existing `ld` argument overrides it for one call. No verb gains an argument. `og_plink_reference(path, plink_bin)` and the string forms of `ld` (an LD Store name or an ancestry code) keep working as values of `ld`, so scripts written for the API epic, and TwoSampleMR 0.8.0, are unchanged.

**Stored clumps by default, clumping on the fly otherwise.** GWAS Store Releases gain stored Clumps for their 5e-8 Top-Hit tier (opengwasdb#62, ADR 0106 there). Each set of Clumps was built with one configuration: (LD resource, r², kb), from the ancestry's default LD resource.
- `og_tophits(src, ids)`, with `clump = TRUE` and its default `threshold`, `r2` and `kb`, reads the stored Clumps. No LD is computed, so it works on a Local Source with no LD resource.
- Any other `threshold`, `r2`, `kb` or LD resource clumps on the fly, with the Source's LD resource or `ld`.
- Stored Clumps are used only when the request matches their configuration exactly, never approximately.
- Provenance records whether the clumps were stored or computed, and with which configuration.
- `clump = FALSE` returns the unclumped top hits.

**Until a Store Release has Clumps** (every release today, and every release ogquery 0.1.0 reads), `clump = TRUE` clumps on the fly with the Source's LD resource. On the API that is the ancestry's default LD Store, as now. On a Local Source with no LD resource it is refused, saying the release has no Clumps and naming the LD forms and `clump = FALSE`. A PLINK Reference still requires `r2` and `kb` (`opengwasr_plink_settings_required`): PLINK has no default configuration of its own.

**The ieugwasr local functions.** `ld_clump_local()` and `ld_matrix_local()` (PLINK with a user's bfile) map onto `og_local(gwas = ..., plink = , bfile = )`, or `ld = og_plink_reference()` on either Source.

**Considered:**
- **A separate LD-resource argument on every verb** (`ld_ref =`). It duplicates `ld` and spreads one choice across every call.
- **Letting `og_local()` hold LD Stores beside GWAS Stores**, detected by manifest. It mixes what a Source answers with where its LD comes from.
- **A Local Source silently using the API's LD.** A local query would make network calls and need a token.
- **Choosing stored Clumps whenever they are "close enough".** That is a silently different answer.

**Clarification, 25 November 2026 (LD only on the Source; opengwas/opengwasr#43).** The owner amended "one place, one override": LD is chosen on the Source, and only there. The verbs lose `ld` (`og_tophits()`, `og_clump()`, `og_ld_matrix()`, `og_ld_proxies()`, `og_ld_reflookup()`); they keep `r2`, `kb`, `threshold` and `fill_below_t_far`. The owner waived epic #2's "no verb changes signature" rule for it. A Source's LD resource lives where the Source does: `og_api()` answers LD from the API's LD Stores, and `og_local()` from resources on the user's machine. To mix locations, the user fetches with one Source and clumps with the other, through `og_clump()` on data already fetched:

| Associations | LD | How |
|---|---|---|
| API | API | `og_tophits(og_api(), ids)` (ancestry-matched), or `og_api(ld_store = "EUR")` |
| Local | PLINK | `og_tophits(og_local(gwas = , bfile = ), ids, r2 = , kb = )` |
| Local | API | `og_clump(og_api(), og_tophits(local, ids, clump = FALSE))` |
| API | PLINK | `og_clump(og_local(bfile = ), og_tophits(og_api(), ids, clump = FALSE), r2 = , kb = )` |

- **`og_local(bfile = )` alone is an LD-only Local Source.** `gwas` becomes optional. Such a Source answers `og_clump()` and `og_ld_matrix()` for variants PLINK can match by rsID (a `rsid` column, or rsIDs). ALIDs need a GWAS Store to look up their rsIDs, so they are refused, naming `og_local(gwas = , bfile = )`. Every verb that reads Associations refuses the Source too. It is the one PLINK path with no API, replacing `src = NULL` with `ld = og_plink_reference()`.
- **Other LD for one call is another Source.** `inherit =` on both constructors builds a Source like an existing one: every argument given replaces the inherited one, and the rest (URL, key, timeout, `max_in_flight`, GWAS Store Releases) are kept. Any LD argument given replaces the inherited LD resource whole, never mixed with it, except `plink` alone, which keeps the inherited PLINK Reference's `bfile` and changes the binary. `inherit` must be the same kind of Source. The constructors' own validation applies, so there is no separate copy-and-modify function.
- **`og_plink_reference()` is no longer exported.** The Local Source builds its PLINK Reference from `bfile` and `plink`, and `og_ld_resource()` still returns it. Provenance's `ld_from` is `"source"` or `"ancestry"`; `"call"` is gone.

The paragraph on the ieugwasr local functions now reads: `ld_clump_local()` and `ld_matrix_local()` map onto `og_clump(og_local(bfile = , plink = ), data, r2 = , kb = )` and `og_ld_matrix(og_local(bfile = ), variants)`, or onto `og_local(gwas = , bfile = )` for the user's own Store Releases.
