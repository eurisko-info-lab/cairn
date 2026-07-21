# Architecture: trust and effect boundaries

Target module boundaries for the ongoing Kernel/Core/System/User split
(`MIGRATION-PLAN.md`, applied incrementally). This doc tracks what's *actually
true today*, not the end state — update it as each migration phase lands.

## Target areas

| Area                 | Responsibility                                                        | Trust status                                     |
| --------------------- | ---------------------------------------------------------------------- | ------------------------------------------------- |
| **Kernel**            | Validate artifacts, languages, proofs, changes, authority, transitions | Semantic TCB                                      |
| **Core**              | Parse, derive, elaborate, search, evaluate, project, merge, propose    | Pure but not automatically trusted                |
| **System Interface**  | Define effect operations, resources, requests, responses              | Pure platform contract                            |
| **System Handler**    | Perform filesystem, process, crypto, network, persistence, UI effects  | Operationally privileged, semantically untrusted  |
| **User**              | Define languages, policies, programs, proofs, changes, workflows       | Extensible data                                   |

Purity alone does not place Core (or anything else) outside the trusted base.
Every Core result used for acceptance needs an independent Kernel validation
path — Core proposes, Kernel certifies, same "untrusted proposer, independent
checker" pattern already used throughout the proof/derivation machinery
(`proof.Checker` vs `proof.Search`).

## Current module graph

As of this revision, the pre-migration linear chain, unchanged:

```text
kernel ← workbench ← {proof, compute} ← rosetta ← ledger ← surface ← examples ← tests
```

No `system-interface`, `system-handler`, `core`, `user`, or `runtime` modules
exist yet — `workbench/Cas.scala` still bundles the `Cas` interface, its
filesystem-backed implementations (`MemCas`/`DiskCas`), and the repository-
domain `BranchManifest`/`Branches` types in one file, one module. Every
exemplar language and domain pack lives under `examples/`.

## Forbidden-import rules

Enforced today, mechanically, by `tests/ModuleBoundarySuite`:

- `kernel` must not import `java.nio.file`, `java.net.*`, file-handle I/O
  (`java.io.File`/`FileInputStream`/`FileOutputStream`/`FileReader`/
  `FileWriter`), `java.lang.ProcessBuilder`, or `scala.sys.process`. Kernel's
  existing `java.io`/`java.nio.charset`/`java.security` imports (in-memory
  buffers, charset constants, message digests) are not filesystem/network/
  process I/O and are not flagged.

Named in `MIGRATION-PLAN.md` but **not yet checkable**, because the modules
they constrain don't exist yet:

- `core` must not import filesystem or networking APIs (no `core` module yet
  — `workbench`/`proof`/`compute`/`rosetta` still play that role, unsplit).
- `user` must not import `system-handler` packages (no `user` module yet).
- `system-handler` must not contain language-specific domain logic, e.g. SDS
  or Unison Core internals (no enforcement needed yet — nothing has moved
  into `system-handler` that isn't `Cas`-adjacent).

## Migration status

| Phase | Status | What landed |
| ----- | ------ | ------------ |
| 0. Freeze and characterize | Done | This doc; `ModuleBoundarySuite`; baseline suite/transcript/language-sync confirmed green |
| 1. Split System Interface from System Handler | Not started | Planned: `Cas` trait → `system-interface`; `MemCas`/`DiskCas`/`Branches`/`CasAdmin` → `system-handler`; `BranchManifest` → `kernel` |
| 2. Introduce Core | Not started | |
| 3. Complete the System split (12 effect families) | Not started | |
| 4–5. Authority: audit mode, then enforcement | Not started | |
| 6. Establish the User boundary | Not started | |
| 7. Complete Meta and bootstrap migration | Not started | |
| 8. Compatibility facades and cleanup | Not started | |

Each remaining phase is planned and executed separately once the prior one is
validated in practice — see `MIGRATION-PLAN.md` §6 for the full phase
breakdown and rationale; this doc only tracks current, ground-truth state.
