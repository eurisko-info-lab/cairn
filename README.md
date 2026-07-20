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
| `examples/` | — | STLC, Claims, AffineNet, RosettaQuickSort(+App), PKI, SDS, Law, Bend, Unison (never imported by L0–L2) |
| `tests/` | — | per-phase acceptance suites |

The sbt module graph enforces the import DAG:
`kernel ← workbench ← {proof, compute} ← rosetta ← ledger ← surface ← examples ← tests`.

## Quick start

```bash
sbt test                                                          # all acceptance suites + 100k fuzz corpus
sbt "examples/runMain cairn.examples.Main transcript transcripts/mvp.cairn"   # end-to-end MVP
sbt "examples/runMain cairn.examples.Main transcript transcripts/max.cairn"   # maximal: 3 nodes, gossip, ports, queries
sbt "examples/runMain cairn.examples.Main digests"                # golden digests
sbt "examples/runMain cairn.examples.Main capabilities stlc"      # §2b capability manifest
sbt "examples/runMain cairn.examples.Bench"                       # benchmarks
```

The MVP transcript composes STLC from fragments, round-trips its surface, evaluates
Church booleans, applies ΔL edits (add + rename-with-footprint), certifies a claim by
tests, publishes to a local PoA ledger, and fetches the head on a second node by hash.
The MAX transcript additionally loads STLC **from checked-in text**
([languages/stlc.cairn](languages/stlc.cairn)), applies structural path edits,
asserts expected failures, runs queries, gossips over three nodes, and verifies a
host port. Languages in [languages/](languages/) load at runtime — adding one
requires no recompilation (the meta surface is self-describing: see the bootstrap
fixpoint in [languages/meta.cairn](languages/meta.cairn)). Exemplar apps
**PKI → Law → SDS** are `.cairn` languages with fragment `requires`/`provides`
([docs/exemplars.md](docs/exemplars.md)); Scala under `examples/` is host glue.

## Documents

- [PLAN.md](PLAN.md) — the original 50-story plan (S1–S50, all landed)
- [PLAN-2.md](PLAN-2.md) — the 50-story maximalization plan (M1–M50, all landed)
- [STATUS.md](STATUS.md) / [STATUS-2.md](STATUS-2.md) — scorecards, golden digests, honest deviations, **parity vs sources**
- [docs/bootstrap.md](docs/bootstrap.md) — empty CAS → published STLC in one sitting
- [docs/vocabulary.md](docs/vocabulary.md), [docs/ledger.md](docs/ledger.md),
  [docs/rosetta.md](docs/rosetta.md), [docs/lowering.md](docs/lowering.md),
  [docs/distribution.md](docs/distribution.md), [docs/exemplars.md](docs/exemplars.md),
  [docs/assumptions.md](docs/assumptions.md)
