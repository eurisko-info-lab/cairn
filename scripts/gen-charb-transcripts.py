#!/usr/bin/env python3
"""Regenerate transcripts/charb/*.cairn from the Marble/Charb YAML suite."""
from __future__ import annotations

import re
import pathlib
import sys

CHARB = pathlib.Path.home() / "Projects/all-git-repos/pi-forall/charb/transcripts"
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


def esc(s: str) -> str:
    return s.replace("\\", "\\\\").replace('"', '\\"')


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


def main() -> int:
    if not CHARB.is_dir():
        print(f"missing Charb transcripts dir: {CHARB}", file=sys.stderr)
        return 1
    OUT.mkdir(parents=True, exist_ok=True)
    runnable = deferred = 0
    for p in sorted(CHARB.glob("*-workflow.yaml")):
        text = p.read_text()
        m = re.search(r'description:\s*"([^"]+)"', text)
        name = p.name.replace("-workflow.yaml", "")
        desc = m.group(1) if m else name
        header = (
            f"-- Ported from Marble/Charb `{p.name}`\n"
            f"-- Source: ~/Projects/all-git-repos/pi-forall/charb/transcripts/{p.name}\n"
            f"-- Description: {desc}\n"
        )
        if name in RICH or name in THIN:
            fn = THIN.get(name, thin_publish)
            note = (
                "-- Rich sibling adaptation also under transcripts/.\n"
                if name in RICH
                else "-- Thin Cairn mapping of the Charb theme (not a CLI clone).\n"
            )
            body = f"{header}{note}transcript {name} {{\n{fn(name)}}}\n"
            runnable += 1
        else:
            reason = esc(f"Charb/Marble CLI: {desc} — no Cairn transcript equivalent yet")
            body = (
                f"{header}"
                f"-- Honest deferred coverage (granit-rust used expect-rejected stubs).\n"
                f"transcript {name} {{\n"
                f'  deferred "{reason}" ;\n'
                f"}}\n"
            )
            deferred += 1
        (OUT / f"{name}.cairn").write_text(body)
    print(f"wrote {runnable + deferred} under {OUT} (runnable={runnable} deferred={deferred})")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
