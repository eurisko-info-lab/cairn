# STATUS — end of 50-story plan (PLAN.md)

> **Superseded for current scorecard.** After PLAN-2 maximalization and the
> top-level parity pass, use **[STATUS-2.md](STATUS-2.md)** (golden digests,
> honest deviations, **parity vs sources** matrix). Digests below are the
> Phase 0–8 baseline and are intentionally stale relative to M19/M6 breaks.

Date: 2026-07-20. All phases 0–8 of [PLAN.md](PLAN.md) landed; `sbt test` green
(see per-phase suites under [tests/src/test/scala/cairn/tests](tests/src/test/scala/cairn/tests)).

## Golden digests (`sbt "examples/runMain cairn.examples.Main digests"`)

```text
language pki  007a5a5304c32e8a292b2857cb5dd46b676237145bba2e5820b60b6b47e95206
  fragment certs    268be89db7fb4417bdbb817eac961647f987403403660ec499bb55271c24d377
language stlc 237520fbe254d025bb27526694656d43678386452394132a7bae1bbfffe5b351
  fragment base     3d279db27e52b16cc9e6f814e549ca71393d13cef0593d70d58c4e409d700ff6
  fragment booleans 34d264068a638251405afa6c6f208b5bacfd718ba45c051f0b67ad3ba8fdbc9d
  fragment lambda   5a7687a302b03494fca9ee36fdf6325d11c8e81a6eeb326f3f0c20b515c9c08f
  fragment types    f48a2735f97681c0c78f83167a6d21d5c49c46f1e1d89067cced4bba49cf566f
  fragment typing   2bf54b6d966bfaeb3f11cae7d3ab86fc9f7f028166ec1536ce1467142d7ed783
rosetta quicksort   3d1c57ac126006a519cdb61cd41c61d1c25274f42a89ba710448cfa6ee9c3458
```

Module/block/certificate digests in transcript output vary with the dev authority
key generated per run; language and fragment digests above are deterministic.

## Transcript result (`transcripts/mvp.cairn`)

All 11 steps pass: lang, 2× roundtrip, 3× eval, 2× delta (add, rename-with-footprint),
claim certificate, publish (PoA block), fetch on second node by hash.

## Success criteria (§9)

| # | Criterion | Status |
|---|---|---|
| 1 | Composition: ≥2 non-trivial fragments via pushout | ✅ STLC = 5 fragments; order-independent digest; conflict errors cite fragments+paths |
| 2 | Bidirectional surface round-trips golden suites | ✅ STLC, Δstlc, meta, transcript, PKI, both port grammars |
| 3 | Semantic CAS with dual keys | ✅ `Digest` + `TypedKey`; typed-key mismatch is a structured error; disk CAS detects corruption |
| 4 | Certified path: proof checked, tamper fails | ✅ STLC typing derivation checks; forged/tampered rejected |
| 5 | Δ-net optional path | ✅ AffineNet (fan/era) reduces; well-formedness judgments; no replicator constructible |
| 6 | Polyglot projection ≥2 ports build/test | ✅ Scala port runs under scala-cli (`ALL TESTS PASS`); Lean skeleton golden-checked (toolchain not assumed) |
| 7 | Ledger publication + second-consumer verification | ✅ Phase5Suite + transcript publish/fetch |
| 8 | Layering matches L0–L6 | ✅ enforced by sbt module DAG; examples never imported by kernel layers |
| 9 | Reproducibility: clean machine runs mvp transcript | ✅ `sbt "examples/runMain cairn.examples.Main transcript transcripts/mvp.cairn"` |
| 10 | Exemplars: PKI or SDS real; Bend/Unison honest | ✅ PKI real (ΔPKI = generic ΔL, Ed25519 chains, ledger anchor); SDS/Bend/Unison documented deferrals, no stubs |
| 11 | Universal closure: recursive ΔL + bootstrap staged | ✅ `Δ(Δ(ΔL))` constructible for any language; meta surface (S44); staging documented |

## Known limitations

Catalogued in [docs/assumptions.md](docs/assumptions.md) — notably: proof logic has
no side-condition vocabulary; meta surface covers interfaces/sorts/ctors/binders only;
single-authority PoA; Rosetta expression vocabulary is QuickSort-sized.
