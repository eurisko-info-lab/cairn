# Cairn

A landmark for semantic software: you compose language fragments into a typed,
hash-addressed definition; the same definition yields parser and printer, evaluator or
interaction-net machine, claims and checked proofs, and projections into familiar proof
assistants and functional languages; local work stays in a content-addressed store; when
you publish, a small proof-of-authority ledger records what was certified and where to
fetch it — so meaning, not merely text, is what moves between machines.

## Host decision

**Scala 3** (3.3 LTS) for the engines, per [CAIRN-PROMPT.md](CAIRN-PROMPT.md) §6
Phase 0 / §11.2. Rosetta *targets* remain multi-host (Scala + Lean 4 ports shipped).
Zero runtime dependencies beyond the JDK (SHA-256, Ed25519); munit for tests.

## Layout (post-migration)

Trust and effect boundaries are the live module story (full detail in
[docs/architecture.md](docs/architecture.md)):

| Module | Role | Contents |
|---|---|---|
| `kernel/` | Semantic TCB | Canonical bytes, digests, artifacts, fragment IR + composition laws, grammar vocabulary, derivation checker, authority models, Meta validation, pure ledger transition |
| `core/` | Pure proposals | Grammar engine, Meta elaboration, ΔL / change algebra, search & tactics, tree + interaction-net engines, Rosetta projection engine, policy evaluation — no I/O |
| `system-interface/` | Effect contracts | CAS trait, filesystem / workspace / process / clock / random / terminal / LSP / external-backend request schemas, PackAccess |
| `system-handler/` | Privileged I/O | MemCas / DiskCas / Branches, filesystem & process handlers, PoA node, crypto, distribution, AuthorityGate, Meta activation |
| `user/` | Extensible data | Language packs, policies, workflows (STLC, Law, SDS, MiniTT, LeanCore, UnisonCore, AffineNet, …); may name effects, never imports handlers |
| `runtime/` | Composition root | PackLoader — ties User + Handlers together |

Key prohibition: `user ↛ system-handler`.

### Compatibility façades

Older PLAN.md layers L0–L6 survive as thin shims / surface I/O — **not** owners of
CAS, grammar, ΔL, or Meta:

| Module | What it actually is |
|---|---|
| `workbench/` | Re-exports `runtime.PackLoader` |
| `compute/` | Re-exports `core` net engine / builder |
| `proof/` | Certification / property helpers on top of Core + Kernel |
| `rosetta/` | Scaffold emit façade (projection engine lives in `core`) |
| `ledger/` | Re-exports handler crypto/node/distribution + `user` PolicyLang |
| `surface/` | CLI, transcript DSL, LSP, Web explorer |
| `examples/` | Host-glue demos (PKI, SDS sealing, Search, Riemann, …); never imported by Kernel/Core |
| `tests/` | Acceptance suites |

sbt enforces the DAG (façades sit above the real graph):

```text
kernel
core                → kernel
system-interface    → kernel
system-handler      → kernel, core, system-interface
user                → kernel, core, system-interface
runtime             → user, system-handler, core, kernel, system-interface

workbench           → runtime (+ kernel, core, system-handler)
proof               → workbench, core, kernel
compute             → workbench, core
rosetta             → proof, compute, core, system-handler
ledger              → rosetta, system-interface, system-handler, user
surface             → ledger, runtime, system-handler
examples            → surface, user, runtime
tests               → examples, runtime, user
```

## Quick start

```bash
sbt test                                                          # all acceptance suites + 100k fuzz corpus
sbt "examples/runMain cairn.examples.Main transcript transcripts/mvp.cairn"   # seed $CAIRN_HOME (default ./.cas)
sbt "examples/runMain cairn.examples.Main home"                      # print CAIRN_HOME / ui root
sbt "examples/runMain cairn.examples.Main ui"                        # Web explorer → http://127.0.0.1:8765
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
([docs/exemplars.md](docs/exemplars.md)); Scala under `examples/` is host glue,
with pure pack defs in `user/`. Riemann is a separate, standalone `.cairn` pack
demonstrating `Claim` vs `Theorem` (§2) on a genuinely open problem — exploratory,
not a parity item
(see [docs/exemplars.md](docs/exemplars.md#riemann--an-open-claim-not-a-parity-item)).
Search is another standalone pack: Fact–Intent–Hint board objects on CAS
([docs/exemplars.md](docs/exemplars.md#search--factintenthint-board-spine)).

## Documents

- [docs/architecture.md](docs/architecture.md) — **module / trust / effect boundaries (source of truth)**
- [PLAN.md](PLAN.md) — the original 50-story plan (S1–S50, all landed)
- [PLAN-2.md](PLAN-2.md) — the 50-story maximalization plan (M1–M50, all landed)
- [STATUS.md](STATUS.md) / [STATUS-2.md](STATUS-2.md) — scorecards, golden digests, honest deviations, **parity vs sources**
- [MIGRATION-PLAN.md](MIGRATION-PLAN.md) — Kernel/Core/System/User refactor (phases 0–8 landed)
- [docs/bootstrap.md](docs/bootstrap.md) — empty CAS → published STLC in one sitting
- [docs/vocabulary.md](docs/vocabulary.md), [docs/ledger.md](docs/ledger.md),
  [docs/rosetta.md](docs/rosetta.md), [docs/lowering.md](docs/lowering.md),
  [docs/distribution.md](docs/distribution.md), [docs/exemplars.md](docs/exemplars.md),
  [docs/assumptions.md](docs/assumptions.md), [docs/explorer.md](docs/explorer.md)
