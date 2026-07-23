#!/usr/bin/env python3
"""Regenerate transcripts/charb/*.cairn from a Marble/Charb YAML suite.

Usage:
  python3 scripts/gen-charb-transcripts.py --source /path/to/charb/transcripts
  python3 scripts/gen-charb-transcripts.py --source ... --pin-only   # refresh dispositions.tsv only

Does not hard-code a developer home path. Records source revision (git HEAD
when available) and a SHA-256 of each input YAML into dispositions.tsv /
SOURCE.rev for reproducible regeneration.
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


def write_dispositions(rows: list[tuple[str, str, str]]) -> None:
    """rows: (name, disposition, source_sha256_or_empty)"""
    lines = [
        "# name\tdisposition\tsource_sha256",
        "# Pinned exact runnable/porcelain/deferred disposition per Charb port.",
        "# Regenerated by scripts/gen-charb-transcripts.py — do not hand-edit casually.",
    ]
    for name, disp, digest in sorted(rows):
        lines.append(f"{name}\t{disp}\t{digest}")
    (OUT / "dispositions.tsv").write_text("\n".join(lines) + "\n")


def pin_from_existing() -> int:
    """Refresh dispositions.tsv from already-generated .cairn files (no source dir)."""
    if not OUT.is_dir():
        print(f"missing {OUT}", file=sys.stderr)
        return 1
    rows: list[tuple[str, str, str]] = []
    for p in sorted(OUT.glob("*.cairn")):
        name = p.stem
        text = p.read_text()
        if re.search(r"^\s*deferred\s+", text, re.M):
            disp = "deferred"
        elif re.search(r"^\s*porcelain\s+", text, re.M):
            disp = "porcelain"
        else:
            disp = "runnable"
        rows.append((name, disp, ""))
    write_dispositions(rows)
    print(f"pinned {len(rows)} dispositions under {OUT / 'dispositions.tsv'}")
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
        help="Only rewrite dispositions.tsv from existing transcripts/charb/*.cairn",
    )
    args = ap.parse_args()

    if args.pin_only:
        return pin_from_existing()

    if args.source is None:
        print("error: --source DIR is required (or pass --pin-only)", file=sys.stderr)
        return 2
    charb = args.source.expanduser().resolve()
    if not charb.is_dir():
        print(f"missing Charb transcripts dir: {charb}", file=sys.stderr)
        return 1

    OUT.mkdir(parents=True, exist_ok=True)
    rev = git_rev(charb)
    (OUT / "SOURCE.rev").write_text(
        f"source={charb}\nrevision={rev}\n"
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

    write_dispositions(rows)
    print(
        f"wrote {runnable + deferred} under {OUT} "
        f"(runnable+porcelain={runnable} deferred={deferred} porcelain={len(PORCELAIN)}) "
        f"rev={rev}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
