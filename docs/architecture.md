# Architecture: trust and effect boundaries

Target module boundaries for the Kernel/Core/System/User split
(`MIGRATION-PLAN.md`). This doc tracks what's *actually true today*.

## Target areas

| Area                 | Responsibility                                                        | Trust status                                     |
| --------------------- | ---------------------------------------------------------------------- | ------------------------------------------------- |
| **Kernel**            | Validate artifacts, languages, proofs, changes, authority, transitions | Semantic TCB                                      |
| **Core**              | Parse, derive, elaborate, search, evaluate, project, merge, propose    | Pure but not automatically trusted                |
| **System Interface**  | Define effect operations, resources, requests, responses              | Pure platform contract                            |
| **System Handler**    | Perform filesystem, process, crypto, network, persistence, UI effects  | Operationally privileged, semantically untrusted  |
| **User**              | Define languages, policies, programs, proofs, changes, workflows       | Extensible data                                   |
| **Runtime**           | Composition root — PackLoader, CLI wiring                              | Ties User + Handlers together                     |

## Current module graph

```text
kernel
core                → kernel
system-interface    → kernel
system-handler      → kernel, core, system-interface
user                → kernel, core, system-interface   (↛ system-handler)
runtime             → user, system-handler, core, kernel, system-interface

# Compatibility façades
workbench           → runtime (+ kernel, core, system-handler)
proof               → workbench, core, kernel
compute             → workbench, core          # NetEngine re-export from core
rosetta             → proof, compute, core, system-handler
ledger              → rosetta, system-interface, system-handler, user
surface             → ledger, runtime, system-handler
examples            → surface, user, runtime   # demos / host glue
tests               → examples, runtime, user
```

## What landed by phase

| Phase | Status | Summary |
| ----- | ------ | ------- |
| 0 | Done | architecture.md; ModuleBoundarySuite |
| 1 | Done | Cas → system-interface; MemCas/DiskCas/Branches → system-handler; BranchManifest → kernel |
| 2 | Done | Core holds Search/Tactics, TreeEngine, PackCompose, Grammar, Meta, Surfaces, Delta, ChangeAlgebra, Rosetta engine, ScaffoldPlan, NetEngine |
| 3 | Done | Full effect-family contracts + handlers (fs, workspace, process, crypto, clock, random, network, http, ledger transport, terminal, lsp, external backend) |
| 4 | Done | Kernel Authority models; Core PolicyEval; AuthorityGate audit mode |
| 5 | Done | Enforce mode + LedgerAppend gate on Node.append; AuthoritySuite |
| 6 | Done | `user/` module; PackAccess; language packs moved; boundary test |
| 7 | Done | MetaValidate (kernel) + MetaActivation (handler); core.Meta stays pure |
| 8 | Done | `runtime.PackLoader`; façades; docs |

## Effect interfaces: Meta-definition status

Post-migration priority #1 (distinct from `MIGRATION-PLAN.md`, now fully
landed above): make effect interfaces Meta-defined — generate typed rights
and resource vocabularies instead of hand-maintaining them independently of
each other, the way `core.Meta` already treats the Fragment IR as a Cairn
language rather than an opaque Scala shape.

- **`Random`, `Clock`, `Process`** (done): `kernel.EffectMeta.{random,clock,process}`
  are Kernel-owned `Fragment`s (sorts + constructors, no grammar — effect
  requests are host-constructed, not user-typed source text) describing each
  family's `Request`/`Response`/`Error` shapes. `EffectMeta.actionsOf`
  projects a family's rights vocabulary from its Fragment and is checked
  against `kernel.Effects.Action`'s hand-tagged cases and against the
  matching `system-interface` object's actual Scala case names
  (`EffectMetaSuite`) — both were previously free to drift independently.
  `Clock` had already drifted before this mechanism existed: `Request` had 2
  cases (`Now`, `TimestampSlug`) but `Action` had only 1 (`ClockNow`) —
  `system-handler.Clock` was executing a request with no corresponding
  right. Fixed by adding `ClockTimestampSlug` alongside introducing the
  Fragment, and now caught by `EffectMetaSuite` going forward. `Process`
  confirmed the mechanism doesn't cry wolf (no drift found) and that a
  plain `case class` response (`Process.Result`, not a `Response` enum)
  maps to a single-constructor sort without friction.
- **Remaining 10 families** (`Filesystem`, `Cas`, `Workspace`, `Crypto`,
  `Network`, `Http`, `LedgerTransport`, `Terminal`, `Lsp`,
  `ExternalBackend`) — not yet converted; `Random`/`Clock`/`Process` are the
  template, each is a separate future slice. Same incremental-adoption
  shape as `AuthorityGate`'s per-family `Enforce` rollout below.
- **Not yet attempted**: replacing `Effects.Action` itself (still a closed,
  hand-written Scala enum — `EffectMeta.actionsOf` checks it, doesn't
  replace it) and typed per-family resources (`Authority.Resource` is still
  one shared untyped `(kind, path)` shape across every family).

## Forbidden-import rules (ModuleBoundarySuite)

- `kernel` / `core`: no filesystem, networking, or process APIs
- `user`: no `cairn.systemhandler`, no `java.nio.file` / net / process

## Intentional residuals

- **BFT / gossip daemon / peer discovery** — deferred forever (docs/distribution.md)
- **Separate `grammar.cairn`** — deferred (docs/bootstrap.md)
- **PKI/Search/Riemann host glue, Claims, SDS sealing tutorials** — remain in `examples/` because they need handler crypto/CAS/filesystem; pure language defs that can live without handlers are in `user/`
- **Full per-family Enforce on every handler entry** — pattern established (LedgerAppend); remaining families opt in incrementally via `AuthorityGate.check`
- **Facade modules** (`workbench`, `proof`, `compute`, `ledger` re-exports, `rosetta.Scaffold`) retained as documented compatibility shims

## Final principle

```text
User defines intent.
Core derives consequences.
System Interface names possible interactions.
Kernel grants authority and validates results.
System Handler touches the world.
Runtime composes User + Handlers.
```
