# Cairn

A landmark for semantic software: you compose language fragments into a typed,
hash-addressed definition; the same definition yields parser and printer, evaluator or
interaction-net machine, claims and checked proofs, and projections into familiar proof
assistants and functional languages; local work stays in a content-addressed store; when
you publish, a small proof-of-authority ledger records what was certified and where to
fetch it — so meaning, not merely text, is what moves between machines.

## Host decision

**Scala 3** (3.3 LTS) for all engines L0–L6, per [CAIRN-PROMPT.md](CAIRN-PROMPT.md) §6
Phase 0 / §11.2. Rosetta *targets* remain multi-host (Scala + Lean 4 ports shipped).
Zero runtime dependencies beyond the JDK (SHA-256, Ed25519); munit for tests.

## Layout (layers L0–L6, §3)

| Module | Layer | Contents |
|---|---|---|
| `kernel/` | L0 | canonical bytes, digests, dual typed keys, artifact kinds, Cst + generic binding, fragment IR + pushout composition laws, grammar-as-data vocabulary, pure ledger transition |
| `workbench/` | L1 | CAS (memory/disk), branches, ONE generic lexer/parser/printer, recursive ΔL generation, meta surface (self-description) |
| `proof/` | L2 | derivation checker (independent, decidable), claims, theorems, test suites, certificates |
| `compute/` | L3 | generic tree rewriter (rules as data), interaction-net engine (agents/ports/rules as data) |
| `rosetta/` | L4 | interchange artifacts + round-trip-verified Scala and Lean port emitters |
| `ledger/` | L5 | Ed25519 identities, PoA node, publication, pull sync, divergence surfacing |
| `surface/` | L6 | CLI (`hash/put/get/canon/transcript`), transcript DSL (defined in the grammar engine itself) |
| `examples/` | — | STLC, Claims, AffineNet, RosettaQuickSort, PKI (never imported by L0–L2) |
| `tests/` | — | per-phase acceptance suites |

The sbt module graph enforces the import DAG:
`kernel ← workbench ← {proof, compute} ← rosetta ← ledger ← surface ← examples ← tests`.

## Quick start

```bash
sbt test                                                          # all acceptance suites
sbt "examples/runMain cairn.examples.Main transcript transcripts/mvp.cairn"   # end-to-end MVP
sbt "examples/runMain cairn.examples.Main digests"                # golden digests
```

The MVP transcript composes STLC from fragments, round-trips its surface, evaluates
Church booleans, applies ΔL edits (add + rename-with-footprint), certifies a claim by
tests, publishes to a local PoA ledger, and fetches the head on a second node by hash.

## Documents

- [PLAN.md](PLAN.md) — the 50-story implementation plan (all stories S1–S50 landed)
- [STATUS.md](STATUS.md) — golden digests, transcript results, §9 success criteria
- [docs/bootstrap.md](docs/bootstrap.md) — empty CAS → published STLC in one sitting
- [docs/vocabulary.md](docs/vocabulary.md), [docs/ledger.md](docs/ledger.md),
  [docs/rosetta.md](docs/rosetta.md), [docs/lowering.md](docs/lowering.md),
  [docs/distribution.md](docs/distribution.md), [docs/exemplars.md](docs/exemplars.md),
  [docs/assumptions.md](docs/assumptions.md)
