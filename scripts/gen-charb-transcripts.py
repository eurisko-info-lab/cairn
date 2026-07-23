#!/usr/bin/env python3
"""Regenerate transcripts/charb/*.cairn from a Marble/Charb YAML suite.

Usage:
  python3 scripts/gen-charb-transcripts.py --source /path/to/charb/transcripts
  python3 scripts/gen-charb-transcripts.py --source ... --update-docs
  python3 scripts/gen-charb-transcripts.py --pin-only   # refresh dispositions; preserve hashes
  python3 scripts/gen-charb-transcripts.py --update-docs  # sync SOURCES.md / porcelain.md counts

Does not hard-code a developer home path. Records source Git revision and a
SHA-256 of each input YAML into dispositions.tsv / SOURCE.rev / transcript
headers for reproducible regeneration.
"""
from __future__ import annotations

import argparse
import hashlib
import pathlib
import re
import subprocess
import sys

ROOT = pathlib.Path(__file__).resolve().parents[1]
OUT = ROOT / "transcripts" / "charb"
SOURCES_MD = ROOT / "transcripts" / "SOURCES.md"
PORCELAIN_MD = ROOT / "docs" / "porcelain.md"

RICH = {
    "chain-sync",
    "chain-divergence",
    "e2e-path",
    "patch-conflict-merge",
    "language-composition",
    "multi-language-matrix",
}

# Themes satisfied by surface.Plumbing / Porcelain (promoted off deferred).
PORCELAIN = {
    "authorization",
    "strict-assumptions-governance",
    "branch-variant-registry",
    "catalog-export",
    "key-sbom-capability-registry",
    "chain-json-export",
    "governance-kernel-introspection",
    "chain-recovery-negative",
    "consistency-batch-mixed-failures",
    "chain-repair",
    "recovery-suggestions",
    "strict-governance-recovery-negative",
    "compose-registry",
    "integration-cross-namespace",
    "interop-lightclient-phase4",
    "replay-history-diff",
    "replay-history-switch-and-diff",
    "replay-root-switching",
    "audit-registry",
    "audit-mismatch",
    "runner-registry",
    "runner-selection",
    "workflow-registry",
    "distributed",
    "strict-governance-publish-integration",
    "governance-real-features",
    "tx-state-phase2",
    "incentive-positive",
    # Promoted in 31870be — cheap wrappers over existing engines
    "chain-quarantine",
    "federation-registry",
    "governance-supplychain",
    "mirror-registry",
    "object-run-commit-registry",
}


def esc(s: str) -> str:
    return s.replace("\\", "\\\\").replace('"', '\\"')


def porcelain_body(name: str) -> str:
    return f"""  porcelain {name} ;
"""


def thin_publish(name=None):
    return """  load-language "languages/stlc.cairn" ;
  lang stlc ;
  node a ;
  node b ;
  delta "{ add tip = fun x : Bool . x ; }" ;
  publish main on a ;
  fetch main from a to b ;
"""


def thin_expectfail(name=None):
    return """  load-language "languages/stlc.cairn" ;
  lang stlc ;
  delta "{ add id = fun x : Bool . x ; add both = (id true) ; }" ;
  expectfail "footprint mismatch" delta "{ rename id to ident footprint [] ; }" ;
"""


def thin_pki(name=None):
    return """  load-language "languages/pki.cairn" ;
  lang pki ;
  roundtrip "revoked alice reason \\"compromise\\" at \\"1000\\"" ;
  delta "{ add soft = revoked alice reason \\"compromise\\" at \\"1000\\" ; }" ;
  publish trust ;
  fetch trust ;
"""


def thin_roundtrip(name=None):
    return """  load-language "languages/stlc.cairn" ;
  lang stlc ;
  roundtrip "fun x : Bool . x" ;
  eval "true" expect "true" ;
  publish tip ;
  fetch tip ;
"""


def thin_gossip(name=None):
    return """  load-language "languages/stlc.cairn" ;
  lang stlc ;
  node a ;
  node b ;
  node c ;
  delta "{ add tip = fun x : Bool . x ; }" ;
  publish main on a ;
  fetch main from a to b ;
  fetch main from a to c ;
  gossip a, b, c ;
"""


THIN = {
    "chain-maintenance": thin_gossip,
    "chain-recovery": thin_gossip,
    "chain-sync-check-dry-run": thin_publish,
    "chain-sync-planning": thin_publish,
    "chain-tamper-detection": thin_expectfail,
    "chain-singleton-enforcement": thin_gossip,
    "consistency-check": thin_publish,
    "enhanced-chain-status": thin_gossip,
    "event-filtering": thin_gossip,
    "publish-registry": thin_publish,
    "language-registry": thin_roundtrip,
    "manifest-registry": thin_publish,
    "patch-registry": thin_expectfail,
    "provenance-registry": thin_publish,
    "hash-artifacts": thin_roundtrip,
    "hash-status": thin_roundtrip,
    "kernel-golden": thin_roundtrip,
    "kernel-grammar-diagnostics": thin_roundtrip,
    "kernel-introspection": thin_roundtrip,
    "verify-diagnostics": thin_roundtrip,
    "inspect-artifacts": thin_roundtrip,
    "inspect-extended": thin_roundtrip,
    "pi-core-descriptor": thin_roundtrip,
    "e2e-registry": thin_publish,
    "tamper-regression": thin_expectfail,
    "malformed-generated-commands": thin_expectfail,
    "malformed-namespace-commands": thin_expectfail,
    "cert-payload": thin_pki,
    "crypto-registry": thin_pki,
    "revocation-recovery": thin_pki,
    "audit-replay": thin_gossip,
    "verbose-audit-replay": thin_gossip,
    "replay-history": thin_gossip,
    "replay-registry": thin_gossip,
    "branch-variant": thin_publish,
    "hardening-phase5": thin_publish,
    "repl": thin_roundtrip,
    "repl-status-export": thin_roundtrip,
}


def sha256_file(p: pathlib.Path) -> str:
    h = hashlib.sha256()
    h.update(p.read_bytes())
    return h.hexdigest()


def git_rev(path: pathlib.Path) -> str:
    try:
        # Prefer the enclosing git repo (charb/), not necessarily the transcripts/ dir.
        out = subprocess.check_output(
            ["git", "-C", str(path), "rev-parse", "HEAD"],
            stderr=subprocess.DEVNULL,
            text=True,
        ).strip()
        return out
    except (subprocess.CalledProcessError, FileNotFoundError):
        return "unknown"


def disposition_of(name: str) -> str:
    if name in PORCELAIN:
        return "porcelain"
    if name in RICH or name in THIN:
        return "runnable"
    return "deferred"


def count_dispositions(rows: list[tuple[str, str, str]]) -> dict[str, int]:
    c = {"runnable": 0, "porcelain": 0, "deferred": 0}
    for _, disp, _ in rows:
        c[disp] = c.get(disp, 0) + 1
    return c


def write_dispositions(rows: list[tuple[str, str, str]], revision: str = "") -> None:
    """rows: (name, disposition, source_sha256)"""
    lines = [
        "# name\tdisposition\tsource_sha256",
        "# Pinned exact runnable/porcelain/deferred disposition per Charb port.",
        "# Regenerated by scripts/gen-charb-transcripts.py — do not hand-edit casually.",
    ]
    if revision:
        lines.insert(1, f"# source-revision: {revision}")
    for name, disp, digest in sorted(rows):
        lines.append(f"{name}\t{disp}\t{digest}")
    (OUT / "dispositions.tsv").write_text("\n".join(lines) + "\n")


def read_dispositions() -> tuple[list[tuple[str, str, str]], str]:
    path = OUT / "dispositions.tsv"
    if not path.is_file():
        return [], ""
    rev = ""
    rows: list[tuple[str, str, str]] = []
    for line in path.read_text().splitlines():
        if line.startswith("# source-revision:"):
            rev = line.split(":", 1)[1].strip()
            continue
        if not line or line.startswith("#"):
            continue
        parts = line.split("\t")
        name = parts[0]
        disp = parts[1] if len(parts) > 1 else ""
        digest = parts[2] if len(parts) > 2 else ""
        rows.append((name, disp, digest))
    return rows, rev


def hash_from_cairn(text: str) -> str:
    m = re.search(r"^-- Source-sha256:\s*(\S+)\s*$", text, re.M)
    return m.group(1) if m else ""


def rev_from_cairn(text: str) -> str:
    m = re.search(r"^-- Source-revision:\s*(\S+)\s*$", text, re.M)
    return m.group(1) if m else ""


def update_docs(rows: list[tuple[str, str, str]], revision: str = "") -> None:
    """Rewrite disposition counts in SOURCES.md / porcelain.md from the ledger."""
    counts = count_dispositions(rows)
    total = sum(counts.values())
    runnable, porcelain, deferred = counts["runnable"], counts["porcelain"], counts["deferred"]

    # --- SOURCES.md: replace the Charb suite counts table ---
    if SOURCES_MD.is_file():
        src = SOURCES_MD.read_text()
        block = (
            f"## Full Charb suite (`transcripts/charb/` — **{total}**/{total})\n"
            "\n"
            "Every `*-workflow.yaml` under a Charb transcripts checkout (pass `--source`)\n"
            "has a Cairn port. Counts below are **generated** from\n"
            "`transcripts/charb/dispositions.tsv` — do not hand-edit.\n"
            "\n"
            "| Kind | Count | Mechanism |\n"
            "| --- | --- | --- |\n"
            f"| Rich / thin runnable | {runnable} | publish/fetch, gossip, PKI, `expectfail` |\n"
            f"| **Porcelain-promoted** | {porcelain} | `porcelain THEME ;` → `Plumbing.charbTheme` "
            f"([porcelain.md](../docs/porcelain.md)) |\n"
            f"| Still `deferred` | {deferred} | §8 out of scope or no plumbing yet |\n"
            "\n"
            "Regenerate: `python3 scripts/gen-charb-transcripts.py --source DIR`\n"
            "(or `--pin-only` to refresh dispositions while preserving source hashes;\n"
            "`--update-docs` syncs this section). "
        )
        if revision:
            block += f"Pinned source revision: `{revision}`.\n"
        else:
            block += "Pinned dispositions: `transcripts/charb/dispositions.tsv`.\n"

        new, n = re.subn(
            r"## Full Charb suite \(.*?\)[\s\S]*?(?=## granit-rust)",
            block + "\n",
            src,
            count=1,
        )
        if n != 1:
            print("warning: could not locate Charb suite section in SOURCES.md", file=sys.stderr)
        else:
            SOURCES_MD.write_text(new)

    # --- porcelain.md: promoted / deferred headings ---
    if PORCELAIN_MD.is_file():
        text = PORCELAIN_MD.read_text()
        text = re.sub(
            r"## Promoted Charb themes \(~?\d+\)",
            f"## Promoted Charb themes ({porcelain})",
            text,
            count=1,
        )
        text = re.sub(
            r"## Still deferred \(~?\d+\)",
            f"## Still deferred ({deferred})",
            text,
            count=1,
        )
        text = re.sub(
            r"Regenerate with `python3 scripts/gen-charb-transcripts\.py`[^\n]*",
            "Regenerate with `python3 scripts/gen-charb-transcripts.py --source DIR` "
            "(then `--update-docs`).",
            text,
            count=1,
        )
        PORCELAIN_MD.write_text(text)

    print(
        f"docs: runnable={runnable} porcelain={porcelain} deferred={deferred} "
        f"total={total} rev={revision or '(none)'}"
    )


def pin_from_existing() -> int:
    """Refresh dispositions from .cairn bodies; preserve source hashes when present."""
    if not OUT.is_dir():
        print(f"missing {OUT}", file=sys.stderr)
        return 1
    prev, prev_rev = read_dispositions()
    prev_hash = {n: d for n, _, d in prev if d}
    rows: list[tuple[str, str, str]] = []
    rev = prev_rev
    for p in sorted(OUT.glob("*.cairn")):
        name = p.stem
        text = p.read_text()
        if re.search(r"^\s*deferred\s+", text, re.M):
            disp = "deferred"
        elif re.search(r"^\s*porcelain\s+", text, re.M):
            disp = "porcelain"
        else:
            disp = "runnable"
        digest = hash_from_cairn(text) or prev_hash.get(name, "")
        file_rev = rev_from_cairn(text)
        if file_rev and (not rev or rev == "unknown"):
            rev = file_rev
        elif file_rev and not rev:
            rev = file_rev
        rows.append((name, disp, digest))
    write_dispositions(rows, revision=rev)
    missing = sum(1 for _, _, d in rows if not d)
    print(
        f"pinned {len(rows)} dispositions under {OUT / 'dispositions.tsv'} "
        f"(hashes present={len(rows) - missing} missing={missing} rev={rev or '(none)'})"
    )
    return 0


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument(
        "--source",
        type=pathlib.Path,
        help="Directory containing Charb *-workflow.yaml files",
    )
    ap.add_argument(
        "--pin-only",
        action="store_true",
        help="Rewrite dispositions.tsv from existing transcripts/charb/*.cairn "
        "(preserves Source-sha256 / prior disposition hashes)",
    )
    ap.add_argument(
        "--update-docs",
        action="store_true",
        help="Sync SOURCES.md / porcelain.md counts from dispositions.tsv",
    )
    args = ap.parse_args()

    if args.pin_only:
        rc = pin_from_existing()
        if rc == 0 and args.update_docs:
            rows, rev = read_dispositions()
            update_docs(rows, rev)
        return rc

    if args.source is None:
        if args.update_docs:
            rows, rev = read_dispositions()
            if not rows:
                print("error: no dispositions.tsv — run with --source or --pin-only first",
                      file=sys.stderr)
                return 1
            update_docs(rows, rev)
            return 0
        print("error: --source DIR is required (or pass --pin-only / --update-docs)",
              file=sys.stderr)
        return 2

    charb = args.source.expanduser().resolve()
    if not charb.is_dir():
        print(f"missing Charb transcripts dir: {charb}", file=sys.stderr)
        return 1

    OUT.mkdir(parents=True, exist_ok=True)
    rev = git_rev(charb)
    # Logical identity only — never embed a developer absolute path.
    (OUT / "SOURCE.rev").write_text(
        "source=charb/transcripts\n"
        f"revision={rev}\n"
        f"workflow_count={len(list(charb.glob('*-workflow.yaml')))}\n"
    )

    rows: list[tuple[str, str, str]] = []
    runnable = deferred = 0
    for p in sorted(charb.glob("*-workflow.yaml")):
        text = p.read_text()
        digest = sha256_file(p)
        m = re.search(r'description:\s*"([^"]+)"', text)
        name = p.name.replace("-workflow.yaml", "")
        desc = m.group(1) if m else name
        rel = p.name
        header = (
            f"-- Ported from Marble/Charb `{rel}`\n"
            f"-- Source-file: {rel}\n"
            f"-- Source-sha256: {digest}\n"
            f"-- Source-revision: {rev}\n"
            f"-- Description: {desc}\n"
        )
        disp = disposition_of(name)
        if name in RICH or name in THIN:
            fn = THIN.get(name, thin_publish)
            note = (
                "-- Rich sibling adaptation also under transcripts/.\n"
                if name in RICH
                else "-- Thin Cairn mapping of the Charb theme (not a CLI clone).\n"
            )
            body = f"{header}{note}transcript {name} {{\n{fn(name)}}}\n"
            runnable += 1
        elif name in PORCELAIN:
            body = (
                f"{header}"
                f"-- Porcelain/plumbing promotion (docs/porcelain.md); not a Marble CLI clone.\n"
                f"transcript {name} {{\n"
                f"{porcelain_body(name)}"
                f"}}\n"
            )
            runnable += 1
        else:
            reason = esc(
                f"Charb/Marble CLI: {desc} — §8 out of scope or no Cairn plumbing yet "
                f"(see docs/porcelain.md)"
            )
            body = (
                f"{header}"
                f"-- Deferred: needs new plumbing or stays §8 out of scope.\n"
                f"transcript {name} {{\n"
                f'  deferred "{reason}" ;\n'
                f"}}\n"
            )
            deferred += 1
        (OUT / f"{name}.cairn").write_text(body)
        rows.append((name, disp, digest))

    write_dispositions(rows, revision=rev)
    if args.update_docs:
        update_docs(rows, rev)
    print(
        f"wrote {runnable + deferred} under {OUT} "
        f"(runnable+porcelain={runnable} deferred={deferred} porcelain={len(PORCELAIN)}) "
        f"rev={rev}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
