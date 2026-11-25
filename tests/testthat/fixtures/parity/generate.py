"""The parity fixtures' expected answers: what the real Data API would return for each golden Job.

Run from the package root with a Python that has ogquery 0.1.0:

    python tests/testthat/fixtures/parity/generate.py

It reads the golden set (queries.json, beside this file) and the example Store Releases bundled in
inst/stores, and writes answers.json beside this file. test-parity.R serves those answers from the
fake Data API (`fake_world(parity = ...)` in fake-server.R) and compares them with what a Local
Source returns for the same calls.

This is a second implementation of the Data API's semantics, written from ogdb-api's own code at
OGDB_API_COMMIT (ogdb_api/query_operations.py, planner.py, ordering.py, top_hits.py, releases.py,
resolve.py, schemas.py, jobs.py, main.py), with each step citing the lines it follows. It reads each
Store Release through ogquery's public API: the reads ogdb-api moves to under
opengwas/ogdb-api#192. It never calls opengwasr, so agreement between the two means something.

Where the bundled releases and the API's code part, this follows the API's code as if the API
served the bundle's own releases:
  * The bundled copies lack variants.tsv.gz, which ogdb-api's `_capabilities` (releases.py:137-171)
    requires for variant lookups and PheWAS. The bundle's releases have it, so every capability is
    taken as present.
  * Rows carry the fields ogquery's `resolve(..., include_variant_info=True)` gives, less `beta`,
    `pval` and `source_alid`: the API's pinned reader (opengwasdb 42850d8) sends `log10_p` and none
    of those three (opengwasr #33's report, "Rows are ogdb-api's rows").
  * A number that isn't finite (a missing Association's z, an infinite SE) is written as jsonlite
    writes it, "NaN", "Inf" or "-Inf". The API writes NaN and Infinity, which aren't JSON
    (opengwas/ogdb-api#193); test-parity.R lists this under its known differences.
"""

from __future__ import annotations

import csv
import json
import math
import sys
from pathlib import Path

from ogquery.identity import VariantNormalisationError, normalise_chromosome
from ogquery.release import open_release
from ogquery.stats import z_critical
from ogquery.top_hits import TopHitTiers
from ogquery.version import __version__ as OGQUERY_VERSION

OGDB_API_COMMIT = "9d36e76"
# opengwasdb-stores, whose analysis_catalog builds the rows /api/gwasinfo serves.
CATALOG_COMMIT = "6dcf0841"

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
STORES = ROOT / "inst" / "stores"

# The Store Release behind each name queries.json uses (helper-local.R's `example_layout`).
LAYOUT = {"dense": "dense/r", "ragged": "ragged/comp", "hyb": "hyb/v1", "hyb-c": "hyb/v1c"}

# schemas.py: CHROMOSOMES, the chromosomes a range may name (check_chromosome, schemas.py:35-44).
CHROMOSOMES = {str(n) for n in range(1, 23)} | {"X", "Y", "MT"}
# schemas.py:192-194 (PheWAS) and 225-227 (top hits).
MAX_LIMIT = 10_000
# query_operations.py:71: the widest range.
MAX_SPAN = 10_000_000


class Refusal(Exception):
    """An answer the API gives as an error: `status`, and the body main.py builds."""

    def __init__(self, status: int, code: str, message: str, **extra):
        super().__init__(message)
        self.status = status
        self.body = {"error": code, "message": message, **extra}


def invalid_query(message: str) -> Refusal:
    # main.py:809-811: a ValueError from validate_operation is 400 invalid_query.
    return Refusal(400, "invalid_query", message)


# Opened releases -------------------------------------------------------------------------------

class Release:
    """One Store Release as the API's registry holds it: identity, query facade, Analyses, tiers."""

    def __init__(self, name: str):
        self.path = STORES / LAYOUT[name]
        self.handle = open_release(self.path)
        self.store_id = str(self.handle.manifest.store_id)
        self.release_id = str(self.handle.manifest.release_id)
        self.query = self.handle.query()
        self.analyses = self.query.analyses_table()
        # releases.py:141-142: the tiers whose directory exists, which TopHitTiers finds.
        self.tiers = tuple(float(t) for t in TopHitTiers(self.handle.arrays()).stored_thresholds())

    def analysis_ids(self) -> list[str]:
        return [str(row["analysis_id"]) for _, row in sorted(self.analyses.items())]

    def analysis_index(self) -> dict[str, int]:
        return {str(row["analysis_id"]): int(index) for index, row in self.analyses.items()}


RELEASES: dict[tuple[str, str], Release] = {}


def open_all() -> dict[str, Release]:
    named = {name: Release(name) for name in LAYOUT}
    for release in named.values():
        RELEASES[(release.store_id, release.release_id)] = release
    return named


def select(body) -> list[Release]:
    """The releases a Job reads, in the order asked: registry.py:92-100 (`select`), which
    planner.py:203-216 calls for explicit `releases`, as opengwasr always sends them."""
    selected = []
    for item in body["releases"]:
        identity = (item["store_id"], item["release_id"])
        if identity not in RELEASES:
            raise Refusal(404, "release_not_found", f"unknown release: {identity[0]}/{identity[1]}")
        selected.append(RELEASES[identity])
    return selected


def local_analysis_ids(release: Release, requested) -> list[str]:
    """releases.py:323-342: the release's own analysis ids among `requested`, in request order. A
    `<store_id>:<analysis_id>` counts only for its own Store; a bare id for any that holds it."""
    known = set(release.analysis_ids())
    prefix = f"{release.store_id}:"
    local: list[str] = []
    for value in requested or ():
        value = str(value)
        if value.startswith(prefix) and value[len(prefix):] in known:
            value = value[len(prefix):]
        elif value not in known:
            continue
        if value not in local:
            local.append(value)
    return local


def validate_analysis_membership(selection, analyses) -> None:
    """query_operations.py:286-292: every Analysis named is in a selected release."""
    for analysis in analyses or ():
        if not any(local_analysis_ids(release, [analysis]) for release in selection):
            raise Refusal(404, "analysis_not_found", f"unknown analysis ID: {analysis}")


# Order (ordering.py) -----------------------------------------------------------------------------

def chromosome_rank(chromosome) -> tuple[int, str]:
    """ordering.py:22-29 with opengwasdb's chromosome_sort_key: 1-22 by number, then X, Y and MT,
    then any other name; a missing or unreadable one last."""
    if chromosome is None or str(chromosome) == "":
        return (10**9, "")
    text = str(chromosome)
    if text.isdigit() and 1 <= int(text) <= 22:
        return (int(text), "")
    special = {"X": 23, "Y": 24, "MT": 25}
    if text in special:
        return (special[text], "")
    return (100, text)


def genome_key(row) -> tuple:
    """ordering.py:39-46: (chromosome rank, position, ALID)."""
    return (chromosome_rank(row.get("chromosome")), int(row["position"]), str(row.get("alid", "")))


def source_key(row) -> tuple:
    """ordering.py:49-50."""
    return (str(row.get("store_id", "")), str(row.get("release_id", "")), str(row.get("analysis_id", "")))


def row_key(row) -> tuple:
    """ordering.py:53-55."""
    return (*source_key(row), *genome_key(row))


def strength_of_row(row) -> float:
    """top_hits.py:40-46: |z| of a resolved row; a missing z sorts last."""
    value = abs(float(row["z"]))
    return -1.0 if math.isnan(value) else value


# Rows ------------------------------------------------------------------------------------------

def column(record, name):
    return [record[name][i] for i in range(len(record["z"]))]


def take(record, keep):
    return record.take(list(keep))


def resolve(release: Release, record) -> list[dict]:
    """resolve.py:70-81: the reader's rows with their rsID. ogquery's own
    `resolve(..., include_variant_info=True)` spells a missing rsID "." as resolve.py's MISSING
    does. The API's pinned reader sends no `beta`, `pval` or `source_alid`."""
    rows = []
    for row in release.query.resolve(record, include_variant_info=True):
        row = dict(row)
        for name in ("beta", "pval", "source_alid"):
            row.pop(name, None)
        rows.append(row)
    return rows


def provenance(release: Release, rows) -> list[dict]:
    """query_operations.py:115-117 and planner.py:255: each row stamped with its Store Release."""
    return [{**row, "store_id": release.store_id, "release_id": release.release_id} for row in rows]


def federated(selection, operation) -> list[dict]:
    """planner.py:241-266 (`run`): each release's rows, sorted by row_key, then each Association
    once; two rows with the same key that differ are refused (409 conflicting_rows)."""
    rows = []
    for release in selection:
        rows.extend(provenance(release, operation(release)))
    rows.sort(key=row_key)
    distinct: dict[tuple, dict] = {}
    for row in rows:
        key = (row.get("store_id"), row.get("release_id"), row.get("analysis_id"), row.get("alid"))
        previous = distinct.get(key)
        if previous is not None and previous != row:
            raise Refusal(409, "conflicting_rows", f"conflicting association rows for {key}")
        distinct[key] = row
    return list(distinct.values())


# Endpoints (query_operations.py) ---------------------------------------------------------------

def associations(body):
    """validate_operation (query_operations.py:59-63), then execute_operation (245-252) through the
    planner."""
    analyses = body.get("analyses") or body.get("ids")
    if not analyses:
        raise invalid_query("analyses must be non-empty")
    if not body.get("variants"):
        raise invalid_query("variants must be non-empty")
    selection = select(body)
    validate_analysis_membership(selection, analyses)

    def read(release):
        local = local_analysis_ids(release, analyses)
        if not local:
            return []
        return resolve(release, release.query.lookup(list(body["variants"]), local,
                                                     observed_only=body["observed_only"]))

    return federated(selection, read), {}


def associations_range(body):
    """validate_operation (query_operations.py:57-72, check_chromosome at schemas.py:35-44), then
    execute_operation (245-264) and keep_analyses (267-283) through the planner."""
    chromosome = str(body["chromosome"])
    text = chromosome.strip()
    try:
        canonical = normalise_chromosome(text) if text else ""
    except VariantNormalisationError:
        canonical = ""
    if canonical not in CHROMOSOMES:
        shown = text if len(text) <= 32 else text[:29] + "..."
        raise invalid_query(f"chromosome {shown!r} is not one of 1-22, X, Y or MT")
    analyses = body.get("analyses") or body.get("ids")
    if analyses == ["all"]:
        analyses = None
    if not analyses and not body.get("releases"):
        raise invalid_query("Range queries across all analyses require releases")
    if body["end"] < body["start"]:
        raise invalid_query("end must not precede start")
    if body["end"] - body["start"] + 1 > MAX_SPAN:
        raise Refusal(413, "request_too_large", "range exceeds maximum span")
    selection = select(body)
    validate_analysis_membership(selection, analyses)

    def read(release):
        local = local_analysis_ids(release, analyses) if analyses else None
        if analyses and not local:
            return []
        raw = release.query.range_phewas(chromosome, int(body["start"]), int(body["end"]),
                                         observed_only=body["observed_only"])
        if local is None:
            return resolve(release, raw)
        index = release.analysis_index()
        wanted = {index[a] for a in local}
        keep = [i for i, a in enumerate(column(raw, "analysis_index")) if int(a) in wanted]
        return resolve(release, take(raw, keep))

    return federated(selection, read), {}


def tophits(body):
    """schemas.py:223-227 (threshold in (0, 1), limit 1-10,000), select_operation's tiers
    (query_operations.py:94-112, index_tier at top_hits.py:49-60), then `_top_hits` (175-204):
    each Analysis's tier read whole, cut at the threshold (`at_threshold`, top_hits.py:75-84),
    ranked and cut to `limit` (`rank_per_analysis`, top_hits.py:18-37), then all sorted by
    Analysis, |z| descending and genome order."""
    threshold = body["threshold"]
    limit = body["limit"]
    if not 0 < threshold < 1:
        raise invalid_query("threshold must be greater than 0 and less than 1")
    if not 1 <= limit <= MAX_LIMIT:
        raise invalid_query(f"limit must be from 1 to {MAX_LIMIT}")
    analyses = body.get("analyses")
    selection = select(body)
    validate_analysis_membership(selection, analyses)
    tiers = {}
    for release in selection:
        looser = [tier for tier in release.tiers if tier >= threshold]
        if not looser:
            raise Refusal(400, "unsupported_threshold",
                          f"threshold must be at most {max(release.tiers, default=0):g}, the loosest p-value "
                          f"the Top-Hit Index of {release.store_id} holds",
                          indexed_thresholds=sorted(release.tiers))
        tiers[release.store_id, release.release_id] = min(looser)
    capped = False
    rows = []
    for release in selection:
        tier = tiers[release.store_id, release.release_id]
        local = []
        for analysis_id in (local_analysis_ids(release, analyses) if analyses else [None]):
            raw = release.query.top_hits(analysis_id=analysis_id, threshold=tier,
                                         observed_only=body["observed_only"])
            z = [float(v) for v in column(raw, "z")]
            keep = list(range(len(z)))
            if threshold != tier:
                cut = z_critical(threshold)
                keep = [i for i in keep if not math.isnan(z[i]) and abs(z[i]) >= cut]
            strength = [-1.0 if math.isnan(z[i]) else abs(z[i]) for i in range(len(z))]
            analysis = [int(v) for v in column(raw, "analysis_index")]
            variant = [int(v) for v in column(raw, "variant_index")]
            keep.sort(key=lambda i: (analysis[i], -strength[i], variant[i]))
            seen: dict[int, int] = {}
            kept = []
            for i in keep:
                seen[analysis[i]] = seen.get(analysis[i], 0) + 1
                if seen[analysis[i]] <= limit:
                    kept.append(i)
                else:
                    capped = True
            local.extend(resolve(release, take(raw, kept)))
        rows.extend(provenance(release, local))
    rows.sort(key=lambda row: (*source_key(row), -strength_of_row(row), *genome_key(row)))
    return rows, {"threshold": threshold, "observed_only": body["observed_only"], "total": len(rows),
                  "truncated": capped}


def phewas(body):
    """schemas.py:187-194 (threshold in (0, 1], limit 1-10,000), then `_phewas`
    (query_operations.py:155-172): each release's Associations of the variant cut at the threshold
    (`significant`, top_hits.py:63-72: threshold 1 keeps everything), to the Analyses named, its
    count added to `total` and its first `limit` kept; all of them sorted by row_key and cut to
    `limit`."""
    threshold = body["threshold"]
    limit = body["limit"]
    if not 0 < threshold <= 1:
        raise invalid_query("threshold must be greater than 0 and at most 1")
    if not 1 <= limit <= MAX_LIMIT:
        raise invalid_query(f"limit must be from 1 to {MAX_LIMIT}")
    selection = select(body)
    total = 0
    rows = []
    for release in selection:
        raw = release.query.phewas(body["variant"], observed_only=body["observed_only"])
        if threshold < 1:
            cut = z_critical(threshold)
            z = [float(v) for v in column(raw, "z")]
            raw = take(raw, [i for i in range(len(z)) if not math.isnan(z[i]) and abs(z[i]) >= cut])
        local = resolve(release, raw)
        if body.get("analyses"):
            wanted = set(local_analysis_ids(release, body["analyses"]))
            local = [row for row in local if row.get("analysis_id") in wanted]
        total += len(local)
        rows.extend(provenance(release, local[:limit]))
    rows.sort(key=row_key)
    rows = rows[:limit]
    return rows, {"variant": body["variant"], "threshold": threshold, "observed_only": body["observed_only"],
                  "total": total, "returned": len(rows), "truncated": total > len(rows)}


VARIANT_FIELDS = ("alid", "chromosome", "position", "effect_allele", "other_allele", "rsid")


def variant_rows(release: Release, found) -> list[dict]:
    """query_operations.py:313-319 (`_variant_rows`): the variants in index order, VARIANT_FIELDS
    each; ogquery's `variants()` and `variants_in_range()` give the same records, keyed by index."""
    return [{field: found[index][field] for field in VARIANT_FIELDS} for index in sorted(found)]


def variants_rsid(body):
    """query_operations.py:131-145 and `variants_by_rsid` (322-325): an rsID matched whole."""
    wanted = {str(value) for value in body["rsids"]}
    rows = []
    for release in select(body):
        found = release.query.variants(sorted(wanted))
        rows.extend(provenance(release, [r for r in variant_rows(release, found) if r["rsid"] in wanted]))
    return rows, {}


def variants_chrpos(body):
    """query_operations.py:131-145 and `variants_by_position` (328-337): `chr` dropped, values not
    `chromosome:position` skipped, and a variant kept only where its own `chromosome:position` is
    one asked for."""
    wanted = {str(value).removeprefix("chr") for value in body["positions"]}
    rows = []
    for release in select(body):
        found = {}
        for value in sorted(wanted):
            chromosome, _, position = value.rpartition(":")
            if chromosome and position.isdigit():
                found.update(release.query.variants_in_range(chromosome, int(position), int(position)))
        kept = [r for r in variant_rows(release, found) if f"{r['chromosome']}:{r['position']}" in wanted]
        rows.extend(provenance(release, kept))
    return rows, {}


ENDPOINTS = {
    "/api/associations": associations,
    "/api/associations/range": associations_range,
    "/api/tophits": tophits,
    "/api/phewas": phewas,
    "/api/variants/rsid": variants_rsid,
    "/api/variants/chrpos": variants_chrpos,
}


# The catalogue (/api/gwasinfo) -----------------------------------------------------------------

# opengwasdb-stores src/analysis_catalog/columns.py (MANIFEST_COLUMNS): the types of the columns the
# catalogue reads from a release's analyses.tsv. The others are TEXT.
INTEGER = {"trait_bp", "sample_size", "n_cases", "n_controls"}
REAL = {"original_sd", "maf_threshold", "info_score_threshold"}
NAN_IS_ABSENT = {"maf_threshold", "info_score_threshold"}
MANIFEST = ["analysis_label", "source_label", "source_analysis_id", "trait_ontology_id", "trait_ontology_label",
            "trait_ontology_mapping_method", "tissue", "context", "trait_chr", "trait_bp", "assigned_ancestry",
            "ancestry_assignment_method", "source_ancestry_label", "sample_size_kind", "sample_size_scope",
            "sample_size", "n_cases", "n_controls", "stored_effect_scale", "original_effect_scale", "original_sd",
            "original_sd_method", "maf_threshold", "info_score_threshold", "license", "publication_doi",
            "publication_pmid", "consortium", "first_author"]


def typed(column_name: str, cell):
    """policy.py `_typed`: empty or absent is null, never 0 or ""."""
    if cell is None or not str(cell).strip():
        return None
    text = str(cell).strip()
    if column_name in INTEGER:
        return int(text)
    if column_name in REAL:
        if text == "NaN" and column_name in NAN_IS_ABSENT:
            return None
        return float(text)
    return text


def catalogue_rows(release: Release, keys: list[str]) -> list[dict]:
    """policy.py `_analysis_row` (and `outcome_type`, `ancestry_category`, `trait_class`): one row
    per analyses.tsv row, in the file's order, its keys in the order the API sends them (shapes.json
    `analysis_row`). The release is taken as a validated, listed Store's."""
    with open(release.path / "analyses.tsv", newline="", encoding="utf-8") as handle:
        table = list(csv.DictReader(handle, delimiter="\t", quoting=csv.QUOTE_NONE))
    rows = []
    for cells in table:
        analysis_id = cells["analysis_id"].strip()
        values = {name: typed(name, cells.get(name)) for name in MANIFEST}
        from_source = values["analysis_label"] is None and values["source_label"] is not None
        if from_source:
            values["analysis_label"] = values["source_label"]
        scale, kind = values["stored_effect_scale"], values["sample_size_kind"]
        if kind == "case_control" or scale == "log_or":
            outcome = "binary"
        elif scale == "sd" and kind in {"total", "effective", "variant_level"}:
            outcome = "continuous"
        else:
            outcome = None
        ancestry = values["assigned_ancestry"]
        if ancestry is None and values["source_ancestry_label"] == "META":
            ancestry = "META"
        values.update(
            id=f"{release.store_id}:{analysis_id}", store_id=release.store_id, analysis_id=analysis_id,
            status="validated", listed=1, label_from_source=int(from_source), outcome_type=outcome,
            ancestry_category=ancestry,
            trait_class="molecular" if any(values[c] is not None for c in ("context", "tissue", "trait_chr"))
            else "phenotype",
            release_id=release.release_id)
        rows.append({key: values[key] for key in keys})
    return rows


# Writing -----------------------------------------------------------------------------------------

def finite(value):
    """A number as JSON can carry it: a non-finite one as jsonlite spells it (see the docstring)."""
    if isinstance(value, float) and not math.isfinite(value):
        return "NaN" if math.isnan(value) else ("Inf" if value > 0 else "-Inf")
    if isinstance(value, dict):
        return {key: finite(item) for key, item in value.items()}
    if isinstance(value, list):
        return [finite(item) for item in value]
    return value


def plain(value):
    """numpy scalars as Python ones."""
    if hasattr(value, "item") and not isinstance(value, (list, dict, str)):
        return value.item()
    if isinstance(value, dict):
        return {key: plain(item) for key, item in value.items()}
    if isinstance(value, list):
        return [plain(item) for item in value]
    return value


def answer(endpoint: str, body) -> dict:
    out = {"endpoint": endpoint, "body": body}
    try:
        rows, metadata = ENDPOINTS[endpoint](body)
    except Refusal as refusal:
        out["error"] = {"status": refusal.status, "body": refusal.body}
        return finite(plain(out))
    # jobs.py:355: each NDJSON row is written with sorted keys.
    out["rows"] = [dict(sorted(plain(row).items())) for row in rows]
    out["metadata"] = metadata
    return finite(plain(out))


def compact(value) -> str:
    return json.dumps(value, ensure_ascii=True, allow_nan=False)


def dump(out) -> str:
    """answers.json: the header and sources indented, then one line per catalogue row, and per answer
    its endpoint, body, metadata (or error) and one line per row, so a changed value is a one-line diff."""
    lines = ["{", f' "header": {json.dumps(out["header"], indent=1, ensure_ascii=True, allow_nan=False)},',
             f' "sources": {compact(out["sources"])},', ' "catalogue": [']
    lines.append(",\n".join(f"  {compact(row)}" for row in out["catalogue"]))
    lines.append(" ],")
    lines.append(' "answers": [')
    blocks = []
    for item in out["answers"]:
        head = [f'  {{"endpoint": {compact(item["endpoint"])},', f'   "body": {compact(item["body"])},']
        if "error" in item:
            head.append(f'   "error": {compact(item["error"])}}}')
        else:
            head.append(f'   "metadata": {compact(item["metadata"])},')
            if item["rows"]:
                head.append('   "rows": [')
                head.append(",\n".join(f"    {compact(row)}" for row in item["rows"]))
                head.append("   ]}")
            else:
                head.append('   "rows": []}')
        blocks.append("\n".join(head))
    lines.append(",\n".join(blocks))
    lines.append(" ]")
    lines.append("}")
    return "\n".join(lines) + "\n"


def main() -> int:
    golden = json.loads((HERE / "queries.json").read_text(encoding="utf-8"))
    shapes = json.loads((HERE.parent / "shapes.json").read_text(encoding="utf-8"))
    named = open_all()
    sources = {name: [{"store_id": named[r].store_id, "release_id": named[r].release_id} for r in held]
               for name, held in golden["sources"].items()}
    catalogue = [row for release in named.values() for row in catalogue_rows(release, shapes["analysis_row"])]
    answers = []
    seen = set()
    for query in golden["queries"]:
        for request in query.get("requests", []):
            key = (request["endpoint"], json.dumps(request["body"], sort_keys=True))
            if key in seen:
                continue
            seen.add(key)
            answers.append(answer(request["endpoint"], request["body"]))
    bundle = (STORES / "SOURCE").read_text(encoding="utf-8")
    header = {
        "generated_by": "tests/testthat/fixtures/parity/generate.py",
        "ogquery": OGQUERY_VERSION,
        "ogdb_api_commit": OGDB_API_COMMIT,
        "catalog_commit": CATALOG_COMMIT,
        "bundle": {line.split(" ", 1)[0]: line.split(" ", 1)[1] for line in bundle.splitlines()
                   if line and not line.startswith("#") and " " in line},
        "z_critical": {format(t, "g"): z_critical(t) for t in (5e-8, 1e-5, 5e-6, 1e-4, 5e-4)},
    }
    out = {"header": header, "sources": sources, "catalogue": catalogue, "answers": answers}
    (HERE / "answers.json").write_text(dump(out), encoding="utf-8")
    print(f"wrote {len(answers)} answers and {len(catalogue)} catalogue rows to {HERE / 'answers.json'}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
