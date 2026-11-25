# A Source carries its LD resource; stored clumps by default, clumping on the fly otherwise

A Source says where Associations come from, and also which LD resource answers LD for them. The LD resource is one of four kinds. Each kind can be on the API or on the user's machine:

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
