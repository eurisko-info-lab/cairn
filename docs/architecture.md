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

`system-handler` (Phase 2, fourth slice): `rosetta.Scaffold`'s file I/O
(`Files.createDirectories`/`Files.writeString`, scaffolding Lean/Haskell/Rust
host projects from Rosetta port output) extracted to a small, generic
`system-handler.Filesystem` (`mkdirs`/`writeFile`) — the first cut of Phase
3's eventual "filesystem" effect family. `Scaffold` itself gained a private
`plan` (pure: every `Project` record + every `(path, content)` write,
computed with zero disk access) with `emitAll` shrunk to a thin executor
over `Filesystem`. `plan` stays in `rosetta`, **not** `core`: it calls
`obligationsManifest`, which needs `cairn.workbench.JsonSurface`, and `core`
cannot depend on `workbench` (the dependency runs the other way) — moving it
would cycle. Unlike the first three slices, this one is an intentionally
partial win: the pure/impure boundary is now explicit and the I/O primitive
is now generic and reusable, but the *pure* half couldn't fully migrate to
`core` today. A future slice could revisit once `JsonSurface` (confirmed
I/O-free) has its own Core/Kernel-reachable home.

`core` (Phase 2, fifth slice): `workbench.Grammar` (`Lexer`/`Parser`/
`Concrete`/`Printer`/`RoundTrip`, 592 lines) moved to `core.Grammar` wholesale
— it already imported only `cairn.kernel.*` and stdlib, so this was pure code
motion with no internal restructuring, unlike every prior Phase 2 slice. This
is exactly `MIGRATION-PLAN.md` §5's "Core Meta: grammar interpretation;
parser and printer derivation." No `build.sbt` change was needed (`core`
already depended on `kernel`; `workbench` already depended on `core`). ~40
call sites across `examples`/`surface`/`rosetta`/`ledger`/`tests` updated
(import-line or fully-qualified-prefix changes only — no call-site logic
changed). **Scope boundary**: this slice moved `Grammar` only, not
`workbench.Meta`/`workbench.Surfaces` (`JsonSurface`) — turned out (per the
sixth slice's research, below) that both only ever needed `Parser`/`Printer`
from `workbench`, not `Delta`/`Module`/`Capabilities` as this doc previously
assumed; the fifth slice was their prerequisite, not an independent guess.

`core` (Phase 2, sixth slice): `workbench.Meta` (695 lines: elaborates/prints
the full `Fragment` IR as a Cairn language — parses and re-emits language and
surface packs, including format-preserving printing) and `workbench.Surfaces`
(98 lines: `JsonSurface` encode/decode via the JSON grammar) moved wholesale
to `core.Meta`/`core.Surfaces`. Both files only ever imported `cairn.kernel.*`
and `cairn.core.*` — pure code motion again, no restructuring, no `build.sbt`
change. This directly unblocks (but by itself only partly resolves)
`rosetta.Scaffold`'s deferred pure `plan`: the specific blocker named in the
fourth slice (`cairn.workbench.JsonSurface`) is gone, since `JsonSurface` now
lives in `core` where `rosetta` already reaches; `Scaffold.plan` itself was
not revisited in this slice. ~19 files updated: 15 needed no change (an
existing `core.*` wildcard already in scope picked up the relocated symbols
once compiled), 2 needed an added import (`tests/MetaPreserveFormatSuite.scala`,
`workbench/PackLoader.scala`), 3 needed a `cairn.workbench.X` →
`cairn.core.X` prefix fix (`examples/Main.scala`, `rosetta/Scaffold.scala`,
`tests/WaveFSuite.scala`).

`surface`/`examples`/`tests` see all of the above transitively through
`ledger`/`rosetta` — no `build.sbt` changes were needed at those layers,
only import updates at call sites. `user` and `runtime` do not exist yet;
`rosetta`'s own port-generation engine (`Rosetta.scala`/`Rosetta2.scala`/
`Ports2.scala`, confirmed I/O-free but not yet relocated) still plays Core's
role, unsplit. Every exemplar language and domain pack still lives under
`examples/`. `workbench` retains only `PackLoader`.

`core` (Phase 2, seventh slice): `workbench.Delta` (Module + ΔL derivation/
apply/compose/flatten/format-preserving apply, 456 lines),
`workbench.ModuleSurface` (40 lines; required by format-preserving ΔL), and
`workbench.Capabilities`+`Query` (210 lines) moved wholesale to
`core.Delta`/`core.ModuleSurface`/`core.Capabilities`. Pure code motion for
Delta/ModuleSurface; Query's `run` had a filesystem CAS walk for
`artifacts kind` — purified by taking a preloaded `List[Artifact]` instead of
a `Path` (one WaveH2Suite call site updated to pass artifacts it already
`cas.put`). No `build.sbt` change. `ModuleBoundarySuite` now also enforces
the core no-filesystem/networking/process rule.

`core` (Phase 2, eighth slice): `workbench.ChangeAlgebra`/`Merge`/`Migrate`
(+ `LangMigration`, ~177 lines) moved wholesale to `core.ChangeAlgebra`.
Pure code motion — already depended only on `cairn.kernel.*` and
`cairn.core.{Delta, Module}`. No `build.sbt` change; call sites already had
`import cairn.core.*` and picked up the relocated symbols with no edits.
`workbench` now contains only `PackLoader.scala` (I/O + pure orchestration
seam awaiting a future `runtime`/`user` composition root).

## Forbidden-import rules

Enforced today, mechanically, by `tests/ModuleBoundarySuite`:

- `kernel` must not import `java.nio.file`, `java.net.*`, file-handle I/O
  (`java.io.File`/`FileInputStream`/`FileOutputStream`/`FileReader`/
  `FileWriter`), `java.lang.ProcessBuilder`, or `scala.sys.process`. Kernel's
  existing `java.io`/`java.nio.charset`/`java.security` imports (in-memory
  buffers, charset constants, message digests) are not filesystem/network/
  process I/O and are not flagged.
- `core` must not import the same filesystem/networking/process APIs (same
  list as kernel). Added in the seventh Phase 2 slice once Query's CAS walk
  was removed.

Named in `MIGRATION-PLAN.md` but **not yet checkable**, because the modules
they constrain don't exist yet:

- `user` must not import `system-handler` packages (no `user` module yet).
- `system-handler` must not contain language-specific domain logic, e.g. SDS
  or Unison Core internals (no enforcement needed yet — nothing has moved
  into `system-handler` that isn't generic I/O).

## Migration status

| Phase | Status | What landed |
| ----- | ------ | ------------ |
| 0. Freeze and characterize | Done | This doc; `ModuleBoundarySuite`; baseline suite/transcript/language-sync confirmed green |
| 1. Split System Interface from System Handler | Done | `Cas` trait → `system-interface`; `MemCas`/`DiskCas`/`Branches`/`CasAdmin` → `system-handler`; `BranchManifest` → `kernel` |
| 2. Introduce Core | In progress | First–seventh slices as previously recorded. Eighth slice: `workbench.ChangeAlgebra`/`Merge`/`Migrate` → `core`. Still pending within Phase 2: `rosetta`'s port-generation engine, optional `Scaffold.plan` revisit. `workbench` is down to `PackLoader` only |
| 3. Complete the System split (12 effect families) | Not started | |
| 4–5. Authority: audit mode, then enforcement | Not started | |
| 6. Establish the User boundary | Not started | |
| 7. Complete Meta and bootstrap migration | Not started | |
| 8. Compatibility facades and cleanup | Not started | |

Each remaining phase is planned and executed separately once the prior one is
validated in practice — see `MIGRATION-PLAN.md` §6 for the full phase
breakdown and rationale; this doc only tracks current, ground-truth state.
