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
(`kernel.Checker` vs `core.Search`/`core.Tactics`).

## Current module graph

As of this revision:

```text
kernel ← workbench ← {proof, compute} ← rosetta ← ledger ← surface ← examples ← tests
system-interface → kernel
system-handler   → kernel, system-interface
core             → kernel
workbench        → kernel, core, system-handler (in addition to the chain above)
rosetta          → proof, compute, core (in addition to the chain above)
ledger           → rosetta, system-interface, system-handler (in addition to the chain above)
```

`system-interface`/`system-handler` (Phase 1): the `Cas` trait lives in
`system-interface` (pure — only `cairn.kernel.*`); `MemCas`, `DiskCas`,
`Branches`, and CAS maintenance (`CasAdmin`/`Chunker`/`HashAlgo`/
`DigestMigration`) live in `system-handler` (filesystem I/O). `BranchManifest`
moved to `kernel` (pure data; its validity is a Kernel concern per the
migration plan's own mapping).

`core` (Phase 2, first slice): `object Search` and `object Tactics` (plus
`Tactic`/`TacticScript`) moved out of `proof.Proof.scala` into `core` — the
untrusted proposer half. `Derivation`/`CheckError`/`CheckerCfg`/`object
Checker` (the independent, decidable validator half) moved into `kernel`
instead. `proof` keeps only `Claim`/`Theorem`/`TestCase`/`TestSuite`/
`Certificate`/`object Certify` — orchestration/data that sits above both and
didn't fit either side cleanly.

`core` (Phase 2, second slice): the same pattern applied to `compute`'s
`TreeEngine`/`TraceChecker`. `TraceChecker` used to reach into `TreeEngine`'s
own `matchPattern`/`instantiate` and the `EvalTrace`/`TraceStep` types nested
inside it — splitting as-is would have inverted the dependency direction
(Kernel→Core). Fixed by hoisting `matchPattern`/`instantiate` (as `object
Rewrite`) and `TraceStep`/`EvalTrace` out to independent, top-level `kernel`
definitions (`kernel/Rewrite.scala`), so `kernel.TraceChecker` calls only
same-module code. `core.TreeEngine` now keeps just the proposer/search half
(`stepRoot`/`step`/`normalize`/`stepLocated`/`normalizeTraced`), calling into
`kernel.Rewrite`, Core→Kernel, correct direction. `core.CompiledTreeEngine`
(the compiled/indexed rule-dispatch alternative proposer) moved alongside it.
`compute` now holds only `NetBuilder`/`NetEngine` (interaction-net reduction,
untouched — no coupling to any of the above).

`core`/`system-handler` (Phase 2, third slice): `workbench.PackLoader` split
along the "Placement of Meta" lines `MIGRATION-PLAN.md` §5 already names.
`system-handler.PackFiles` does the raw filesystem work (listing `.cairn`
files, reading text) — deliberately `Fragment`/`Meta`-agnostic, just
`Path`/`String`. `core.PackCompose` holds the pure composition algorithm
(`demote`/`bindSurface`/`bindPack`/`close`/`unmetRequires`), calling
`kernel.Compose.compose`. `SurfacePack` (pure data) moved to `kernel` so
`core.PackCompose.bindSurface` can reference it without `core` depending on
`workbench`. `PackLoader` itself stays in `workbench` as a thin orchestrator
combining both — there's no `runtime`/`user` composition-root module yet for
that glue to live in — re-exporting the exact same public API (`requireOwn`,
`requireClosed`, `loadRaw`, `bindSurface`, `demote`, `unmetRequires`,
`surfacesFor`, `close`, `loadClosed`, `DefaultSurface`), so none of its ~25
external call sites needed to change.

`surface`/`examples`/`tests` see all of the above transitively through
`ledger`/`rosetta` — no `build.sbt` changes were needed at those layers,
only import updates at call sites (none at all for the third slice). `user`
and `runtime` do not exist yet; most of `workbench` (`Meta`/`Grammar`/`Delta`/
`Capabilities`) and all of `rosetta`'s own projection engine still play
Core's role, unsplit. Every exemplar language and domain pack still lives
under `examples/`.

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
| 1. Split System Interface from System Handler | Done | `Cas` trait → `system-interface`; `MemCas`/`DiskCas`/`Branches`/`CasAdmin` → `system-handler`; `BranchManifest` → `kernel` |
| 2. Introduce Core | In progress | First slice: `proof`'s `Checker`→`kernel` / `Search`+`Tactics`→`core` split. Second slice: `compute`'s `TreeEngine`/`TraceChecker` split, via a `kernel.Rewrite` hoist. Third slice: `workbench.PackLoader` split into `system-handler.PackFiles` (I/O) + `core.PackCompose` (pure) + a thin `PackLoader` facade. Still pending within Phase 2: `workbench`'s remaining Meta/Grammar/Delta/Capabilities machinery, `rosetta`'s projection engine |
| 3. Complete the System split (12 effect families) | Not started | |
| 4–5. Authority: audit mode, then enforcement | Not started | |
| 6. Establish the User boundary | Not started | |
| 7. Complete Meta and bootstrap migration | Not started | |
| 8. Compatibility facades and cleanup | Not started | |

Each remaining phase is planned and executed separately once the prior one is
validated in practice — see `MIGRATION-PLAN.md` §6 for the full phase
breakdown and rationale; this doc only tracks current, ground-truth state.
