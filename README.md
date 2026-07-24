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

## Layout (live architecture)

Trust and effect boundaries are the live module story (detail in
[docs/architecture.md](docs/architecture.md)):

Reorganized into three sub-projects — **container** (CAS/ledger),
**content** (languages/proofs/workflows), and **app** (Branches, the glue,
the only layer allowed to depend on both) — with `kernel` as the one shared
foundation both container and content depend on:

| Module | Role | Contents |
|---|---|---|
| `kernel/` | Semantic TCB (shared) | Canonical bytes, digests, artifacts, fragment IR + composition laws, grammar vocabulary, derivation checker, authority models (`AuthorizedRequest` / `AuditedRequest`), Meta validation, pure ledger transition |
| `content/core/` | Pure proposals | Grammar engine, Meta elaboration, ΔL / change algebra, `PatchGraph`, search & tactics, tree + interaction-net engines, Rosetta projection engine, policy evaluation — no I/O |
| `container/system-interface/` | Effect contracts | CAS trait, filesystem / workspace / process / clock / random / terminal / LSP / external-backend request schemas |
| `container/system-handler/` | Privileged I/O | MemCas / DiskCas, filesystem & process handlers, PoA node, crypto, distribution, `AuthorityGate`, `EffectContext`, `RuntimeEffectRegistry`, `RevocationView` |
| `content/user/` | Extensible data | Language packs, policies, workflows (STLC, Law, SDS, MiniTT, LeanCore, UnisonCore, AffineNet, …); may name effects, never imports handlers |
| `app/runtime/` | Composition root | PackLoader, `EffectBootstrap`, `WorkflowRunner`, `Branches` — ties User + Handlers together |

Key prohibition: `user ↛ system-handler`.

### Aggregation modules (not ownership layers)

These sit above the Kernel/Core/System/User/Runtime DAG. They are **not**
L0–L6 owners of CAS / grammar / ΔL / Meta (that story is retired):

| Module | What it actually is |
|---|---|
| `content/proof/` | Certification / property helpers on top of Core + Kernel |
| `app/rosetta/` | Scaffold emit façade (projection engine lives in `core`) |
| `app/surface/` | CLI, transcript DSL, LSP, Web explorer |
| `app/examples/` | Host-glue demos (PKI, SDS sealing, Search, Riemann, …); never imported by Kernel/Core |
| `app/tests/` | Acceptance suites |

There is **no** `workbench/` or `ledger/` sbt project in the live graph — CAS
and PoA live under `system-handler` / `kernel`; Branches, Meta activation,
and pack loading live under `runtime`.

sbt enforces the DAG (`build.sbt`); project ids are unchanged by the physical
nesting below:

```text
kernel                                (shared)
container/kernel-container  → kernel
content/kernel-rewrite      → kernel
container/system-interface  → kernel, kernel-container
container/system-handler    → kernel, kernel-container, core, system-interface
content/core                → kernel, kernel-rewrite
content/user                → kernel, core, system-interface
app/runtime                 → user, system-handler, core, kernel, system-interface

content/proof               → core, kernel
app/rosetta                 → proof, core, system-handler
app/surface                 → proof, runtime, system-handler
app/examples                → surface, user, runtime
app/tests                   → examples, rosetta, runtime, user
```

## Quick start

```bash
sbt test                                                          # all acceptance suites + 100k fuzz corpus
sbt examples/assembly && ./bin/cairn home                         # fat jar + wrapper (no sbt at runtime)
./bin/cairn transcript transcripts/mvp.cairn                      # seed $CAIRN_HOME (default ./.cas)
./bin/cairn serve                                                 # HTTP node: /chain /heads /blob/… /peers
./bin/cairn peer add alice http://127.0.0.1:8743
./bin/cairn gossip once                                           # HttpGossip round
./bin/cairn bft agree <block-digest-hex>                          # network finality (replica peers)
./bin/cairn bft agree local <block-digest-hex>                    # in-process sealed-block lab
./bin/cairn serve replica <name> [port]                           # HTTP node with /bft/msg
./bin/cairn smoke distribution                                    # two-node gossip + four-node BFT via fat jar
sbt "examples/runMain cairn.examples.Main ui"                     # Web explorer → http://127.0.0.1:8765
sbt "examples/runMain cairn.examples.Main digests"                # golden digests
sbt "examples/runMain cairn.examples.Main capabilities stlc"      # §2b capability manifest
sbt "examples/runMain cairn.examples.Bench"                       # benchmarks
```

The MVP transcript composes STLC from fragments, round-trips its surface, evaluates
Church booleans, applies ΔL edits (add + rename-with-footprint), certifies a claim by
tests, publishes to a local PoA ledger, and fetches the head on a second node by hash.
The MAX transcript additionally loads STLC **from checked-in text**
([content/languages/stlc.cairn](content/languages/stlc.cairn)), applies structural path edits,
asserts expected failures, runs queries, gossips over three nodes, and verifies a
host port. Languages in [content/languages/](content/languages/) load at runtime — adding one
requires no recompilation (the meta surface is self-describing: see the bootstrap
fixpoint in [content/languages/meta.cairn](content/languages/meta.cairn)). Exemplar apps
**PKI → Law → SDS** are `.cairn` languages with fragment `requires`/`provides`
([docs/exemplars.md](docs/exemplars.md)); Scala under `examples/` is host glue,
with pure pack defs in `user/`. Riemann is a separate, standalone `.cairn` pack
demonstrating `Claim` vs `Theorem` (§2) on a genuinely open problem — exploratory,
not a parity item
(see [docs/exemplars.md](docs/exemplars.md#riemann--an-open-claim-not-a-parity-item)).
Search is another standalone pack: Fact–Intent–Hint board objects on CAS
([docs/exemplars.md](docs/exemplars.md#search--factintenthint-board-spine)).

## Documents

- [docs/architecture.md](docs/architecture.md) — **current** module / trust / effect boundaries, golden digests, honest deviations, and parity-vs-sources (single source of truth)
- [docs/bootstrap.md](docs/bootstrap.md) — empty CAS → published STLC in one sitting
- [docs/vocabulary.md](docs/vocabulary.md), [docs/ledger.md](docs/ledger.md),
  [docs/rosetta.md](docs/rosetta.md), [docs/lowering.md](docs/lowering.md),
  [docs/distribution.md](docs/distribution.md), [docs/exemplars.md](docs/exemplars.md),
  [docs/assumptions.md](docs/assumptions.md), [docs/explorer.md](docs/explorer.md)
