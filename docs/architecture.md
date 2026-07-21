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

- **`Random`, `Clock`, `Process`, `ExternalBackend`** (done):
  `kernel.EffectMeta.{random,clock,process,externalBackend}` are each an
  `EffectMeta.EffectFamily` — a Kernel-owned `Fragment` (sorts +
  constructors, no grammar — effect requests are host-constructed, not
  user-typed source text) describing the family's `Request`/`Response`/
  `Error` shapes, paired with an explicit `requestActions: Map[String,
  Effects.Action]` grouping (constructor name → the right that gates it).
  `EffectMeta.actions` projects a family's rights vocabulary from that
  grouping; `EffectMeta.completeness` checks it against
  `kernel.Effects.Action`'s hand-tagged cases and (via `EffectMetaSuite`)
  against the matching `system-interface` object's actual Scala case names —
  both were previously free to drift independently.
  `Clock` had already drifted before this mechanism existed: `Request` had 2
  cases (`Now`, `TimestampSlug`) but `Action` had only 1 (`ClockNow`) —
  `system-handler.Clock` was executing a request with no corresponding
  right. Fixed by adding `ClockTimestampSlug` alongside introducing the
  Fragment, and now caught by `EffectMetaSuite` going forward. `Process`
  confirmed the mechanism doesn't cry wolf (no drift found) and that a
  plain `case class` response (`Process.Result`, not a `Response` enum)
  maps to a single-constructor sort without friction. `ExternalBackend` had
  the same shape of gap as `Clock`: `Request.Find` had no matching `Action`
  (only `Run` did) despite `system-handler.ExternalBackend.perform`
  executing `Find` directly — fixed by adding `BackendFind`. Its `Host` enum
  (the tool being invoked: `ScalaCli`/`Cargo`/`Runghc`/`Lake`) became an
  informal `"Host"` argSort tag rather than its own sort, same convention as
  `Process`'s `"Command"`/`"Path"` tags — `CtorDef.argSorts` are free-form,
  unvalidated by `Compose.compose` (confirmed by reading it in full; already
  relied on by `user.policy.PolicyLang`'s `"Name"` tags), so this needed no
  new Fragment-IR machinery.
- **Many-to-one rights mechanism** (added after these four; a retrofit, zero
  semantic change to any of them): the original `actionsOf` assumed one
  `Action` per `Request`-sorted constructor, matched by exact name — true
  for all four families above only because each had ≤2 distinct request
  shapes, not because rights are actually designed that way. Scoping the
  next family surfaced that they aren't: `Filesystem` has 12 `Request`
  constructors but only 3 `Action`s (`FsRead`/`FsWrite`/`FsMkdirs` —
  intentional capability classes, not a gap), `Workspace` has 5 constructors
  and 1 `Action` (`WorkspaceRead`, matching none of the 5 by name at all),
  `Terminal` has 3 and 1 (`TerminalWrite`), and `Lsp` has 2 and 1
  (`LspServe`, matching neither). Applying the old exact-name-match logic to
  these would misreport intentional groupings as bugs, and "fixing" it by
  minting one Action per Request shape would be a real regression in the
  authority model (more separately-revocable rights than intended). Fixed by
  making the grouping an explicit, checked declaration
  (`EffectFamily.requestActions`) instead of an inferred name match —
  `EffectMeta.completeness` verifies every `Request` constructor has an
  entry, every entry names a real constructor, and every mapped `Action`
  belongs to the right family.
- **`Terminal`** (done): the first real (non-retrofitted) many-to-one
  grouping — `Write`/`WriteLine` (both terminal-output operations) share the
  existing `TerminalWrite` right; `ReadLine` had no right at all
  (`system-handler.Terminal.perform` executed it directly), fixed by adding
  `TerminalRead`.
- **Vestigial families** (found while scoping the `ExternalBackend` slice,
  by checking for a `def perform(req: X.Request)` entry point in
  `system-handler/`): `Http`, `Network`, `Crypto`, `LedgerTransport` have
  **no handler implementation at all** — real functionality (if any) is
  implemented elsewhere bypassing the `system-interface` contract entirely
  (HTTP: `system-handler.HttpNode`/`HttpSync`/`Distribution` and
  `surface.Browser` use raw JDK `com.sun.net.httpserver.HttpServer`
  directly; crypto: `cairn.ledger.Ed25519`/`Encryption`, not
  `system-interface.Crypto`). Converting these to Meta-defined form would be
  Meta-defining dead code — lower priority than the still-live families, and
  possibly a case for removing them instead of converting them (an explicit
  decision for later, not made here). `Cas` is a trait-based contract, not a
  Request/Response enum family, and is out of scope for this mechanism as
  designed.
- **Remaining live families** (`Filesystem`, `Workspace`,
  `Lsp`) — not yet converted, now unblocked by the many-to-one mechanism
  above. Each still needs its own per-family judgment call about which
  requests belong to which capability class (e.g. is `Filesystem.Delete` a
  `FsWrite` operation or does it deserve its own right? is
  `CreateTempDirectory` a `FsMkdirs` operation? does `Lsp` need new,
  per-message actions, or is `LspServe` intentionally a session-level gate?)
  — real design work per family, each a separate future slice. Same
  incremental-adoption shape as `AuthorityGate`'s per-family `Enforce`
  rollout below.
- **Not yet attempted**: replacing `Effects.Action` itself (still a closed,
  hand-written Scala enum — `EffectMeta.completeness` checks it, doesn't
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
