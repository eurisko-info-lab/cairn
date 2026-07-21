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
