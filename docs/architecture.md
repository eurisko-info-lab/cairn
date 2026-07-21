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
- **`Workspace`** (done): the cleanest many-to-one case so far — all 5
  `Request` constructors are read-only discovery/reading (confirmed via
  `system-handler.PackFiles.perform`: nothing mutates), so all 5 map to the
  single existing `WorkspaceRead` right. No new `Action`, no drift.
- **`Filesystem`** (done): the last big live family — 12 `Request`
  constructors, 3 existing `Action`s (`FsRead`/`FsWrite`/`FsMkdirs`). Three
  requests were genuinely ambiguous and were put to the user directly
  (`AskUserQuestion`) rather than decided silently: `Delete` → grouped under
  `FsWrite` (mutating, like `Write`/`WriteBytes`; no family has finer-grained
  rights than broad classes yet); `CreateTempDirectory` → grouped under
  `FsMkdirs` (it creates a directory regardless of the path being
  system-chosen); `Resolve` → **no right required at all**, confirmed by
  reading `system-handler.Filesystem.perform`: it's pure path-string
  arithmetic (`toNio(base).resolve(rel.value)`) with zero `Files.*` calls,
  so it can't leak, corrupt, or touch anything. This is a new case the
  mechanism hadn't needed before, so `EffectFamily.requestActions` widened
  from `Map[String, Effects.Action]` to `Map[String, Option[Effects.Action]]`
  — `None` is an explicit, checked declaration of "no authorization needed,"
  distinct from an omitted entry (which `completeness` still flags as an
  ungated request). The six prior families were retrofitted onto the wider
  type with every existing entry wrapped in `Some(...)` — mechanical, zero
  semantic change. No new `Action` was needed for `Filesystem`: the existing
  3 already cover all 11 authorized requests.
- **`Lsp`** (done): the last live family — completes conversion of all 8. Its
  existing `Action` (`LspServe`) had been a naming mismatch, not a
  deliberate session-level gate: `ReadMessage`/`WriteMessage` are symmetric
  Content-Length-framed stdio operations, structurally identical to
  `Terminal.ReadLine`/`Write`, and `system-handler.LspTransport.perform`
  executes both directly (no separate "start serving" request exists,
  unlike `Http.Request.Serve`, which actually opens a listening socket).
  `LspServe` was referenced nowhere outside its own declaration — a
  confirmed orphan. Asked the user directly rather than deciding silently:
  replaced it with `LspRead`/`LspWrite`, mirroring `Terminal` exactly.
- **Vestigial families — removed** (found while scoping the
  `ExternalBackend` slice, by checking for a `def perform(req: X.Request)`
  entry point in `system-handler/`): `Http`, `Network`, `Crypto` had **no
  handler implementation at all** — real functionality (if any) was
  implemented elsewhere bypassing the `system-interface` contract entirely
  (HTTP: `system-handler.HttpNode`/`HttpSync`/`Distribution` and
  `surface.Browser` use raw JDK `com.sun.net.httpserver.HttpServer`
  directly; crypto: `cairn.ledger.Ed25519`/`Encryption`, not
  `system-interface.Crypto`). The user chose removal over conversion or
  leaving them as-is. `system-interface/{Http,Network,Crypto}.scala` and
  their `Effects.Family`/`Action` entries are gone entirely. **Precise
  boundary**: `LedgerTransport` was *not* wholesale-removed —
  `Effects.Family.LedgerTransport` and `Effects.Action.LedgerAppend` are
  real, actively gated (`Node.append`, running under real `Enforce` mode).
  Only `system-interface/LedgerTransport.scala` (its own unused
  `Request`/`Response`/`Error` types — `Node.append`/`Sync.pull` never
  used them, calling methods directly instead) and the 3 dead sibling
  actions (`LedgerPull`, `LedgerPublish`, `BranchAdvance`, zero usage
  anywhere) were removed. `Cas` is a trait-based contract, not a
  Request/Response enum family, and stays out of scope for this mechanism
  as designed — untouched.
- **All 8 live families are now Meta-defined; the vestigial ones are
  removed.** What's left of this priority: the two "not yet attempted"
  items below.
- **Not yet attempted**: replacing `Effects.Action` itself (still a closed,
  hand-written Scala enum — `EffectMeta.completeness` checks it, doesn't
  replace it).
- **Typed per-family resources — first slice (`Filesystem`)**:
  `Authority.Resource(kind, path)`'s `matches` already supports path-prefix
  scoping, and `AuthoritySuite`'s own tests already construct example
  policies against real paths — but every gated handler was constructing
  its `Resource` with a hardcoded wildcard path (`Resource("filesystem",
  "*")`, etc.), never threading the real target of the request in at all.
  So no policy could ever restrict *which* file/command/resource was
  touched, only *which action* — the matching machinery existed and was
  tested, just never fed real data. `Filesystem.perform` now extracts the
  real `Fs.Path` (or, for `CreateTempDirectory`, its `prefix` — the
  closest thing to a target it has) from each request into
  `Resource("filesystem", <realPath>)`. The bootstrap policy (previous
  slice) still uses a wildcard resource, so this doesn't change behavior
  yet — it makes the *data* real, proving the prefix-matching machinery
  against real paths for the first time; a genuinely restrictive
  path-scoped policy is a separate, later exercise once real identity
  exists.
- **Typed per-family resources — remaining 7 families, closed out.**
  `Workspace`, `ExternalBackend`, and `Process` had a natural per-request
  resource identifier and now thread it through, same pattern as
  `Filesystem`: `Workspace.perform` uses the real `dir`/`langDir`/`path`
  per request (`LanguageDirs` takes no input at all, so `"*"` there is
  honestly correct, not a placeholder); `ExternalBackend.perform` uses the
  `Host` being invoked (`ScalaCli`/`Cargo`/`Runghc`/`Lake`) — there's no
  path to scope by until `find` resolves one, and the tool itself is what
  a policy would actually want to restrict; `Process.perform` uses the
  executable name (`command.headOption`). `Clock`, `Random`, `Terminal`,
  and `Lsp` genuinely have **no** per-request resource to thread —
  wall-clock time, randomness, and stdio/LSP session transport aren't
  scoped to any target a policy could restrict by (`Random.Bytes(n)`'s `n`
  is a quantity, not something to restrict; a terminal/LSP session isn't
  "which terminal", there's only one). Each of those 4 handlers now has an
  inline comment stating this explicitly, so a `"*"` resource reads as a
  documented judgment call rather than unfinished work. All 8 families'
  resource-path status is now settled one way or the other.

## Capability-gating handler entry points (post-migration priority #3)

Reassessment finding: `perform(req: X.Request)` — the typed entry point the
Meta-definition work above builds rights for — had **zero callers anywhere**
in the repo, for any of the 8 families. Every real call site used each
handler's separate "convenience methods" instead (`Filesystem.writeFile`,
`Random.bytes`, etc.), bypassing the Request/Response contract and any
authorization hook entirely. `system-handler/Filesystem.scala`'s own
docstring names the original intent — *"Convenience methods keep existing
call sites compiling; perform is the typed effect entry point"* — Phase 3
built `perform` as the intended future path, but the migration of real
callers onto it never happened, and Phase 8's "Compatibility facades and
cleanup" cleaned up module-level facades, not this one.

A call-site audit found the 8 families split cleanly in two:

| Family | Real external callers |
| --- | --- |
| `Filesystem` | 1 (`rosetta/Scaffold.scala`, `writeFile`) |
| `Process` | 1 (`surface/Transcript.scala`, `run`) |
| `PackFiles`/`Workspace` | 6, all in `runtime/PackLoader.scala` |
| `LspTransport` | 2, both in `surface/Lsp.scala` |
| `Random`, `Clock`, `ExternalBackend`, `Terminal` | **0 — fully dead** |

**`Random`/`Clock`/`ExternalBackend`/`Terminal`** (done): for these four,
"migrate callers onto `perform`" was vacuously already true — nothing
called their convenience methods at all. Each now has its convenience
method(s) made `private` (`bytes`; `nowMillis`/`timestampSlug`;
`find`/`run`; `write`/`writeLine`/`readLine`) — Scala visibility now
structurally enforces "quarantine the raw entry point," not just
convention — and `perform` is gated via a new `AuthorityGate.checked`
combinator (`system-handler/AuthorityGate.scala`), deriving the
`Effects.Action` per `Request` case from the same groupings
`kernel.EffectMeta` already declares. Stays in `Audit` mode (never blocks),
same as every other family — this is the first time any family besides
`LedgerAppend` calls `AuthorityGate` at all, but it doesn't yet change
behavior for anyone. Verified zero regression: the compiler catching zero
external breakage on privatization confirmed the call-site audit was
complete.

**Placeholder, flagged explicitly**: all four use `Authority.Subject("local")`
and `Authority.Resource(<family>, "*")` — there's no real multi-tenant
identity concept for local, non-ledger effects today (the whole system
runs as one local process; only the ledger's PoA layer has real per-
authority identity). Replacing this placeholder with real injected
capabilities is the user's priority #2 ("replace ambient globals... with
explicit runtime contexts and injected capabilities") — noted here as a
forward pointer, not solved.

**`LspTransport`** (done): the first family with real callers to be
migrated. `surface.Lsp.serve`'s session loop — the actual live effect path,
reading/writing real stdio during an LSP session — now calls
`LspTransport.perform` instead of the raw `readMessage`/`writeMessage`,
gated the same way as the four above. Its `export
cairn.systemhandler.LspTransport.{readMessage, writeMessage}` line was
removed (re-exporting a raw primitive under the `Lsp` surface name is
exactly the pattern this priority moves away from). Unlike the four
fully-dead families, `readMessage`/`writeMessage` themselves were **not**
made `private`: `tests/WaveH2Suite.scala` legitimately calls
`LspTransport.writeMessage` directly on a `ByteArrayOutputStream` to build
framed test-fixture bytes — pure byte-buffer construction with no
real-world effect, the same reasoning as `Filesystem.Resolve` needing no
right at all. That test now calls `LspTransport.writeMessage` by its real
name instead of through the removed re-export.

**`Filesystem`** (done): a full call-site check of all 12 convenience
methods (not just the two the initial audit found) showed it was almost
entirely dead too — only `writeFile` (1 real caller: `rosetta.Scaffold
.emitAll`, where host-project files actually get written to disk) and
`readText` (1 caller, `system-handler.MetaActivation.loadAndValidate`,
itself uncalled anywhere) had any callers at all; the other 10
(`mkdirs`/`writeBytes`/`readBytes`/`exists`/`isDirectory`/
`isRegularFile`/`isExecutable`/`list`/`delete`/`createTempDirectory`,
plus `toNio`/`fromNio`) had none. `perform` is now the only public entry
point, restructured into a gated wrapper calling a private `performRaw` —
the **first slice where `kernel.EffectMeta`'s rights groupings are
actually consumed at runtime**: the `Action` derivation in `perform`
mirrors `EffectMeta.filesystem.requestActions` exactly (read/write/mkdirs
classes; `Resolve` stays ungated, matching its `None` entry). Both real
callers migrated onto `perform`: `Scaffold.emitAll`'s write loop (which
also turned an uncaught-exception-on-I/O-failure path into a clean
`Left(String)`, a small correctness improvement the typed entry point
enabled) and `MetaActivation.loadAndValidate` — migrated even though
`MetaActivation` itself is still uncalled anywhere, because its effect is
real (Phase 7 language-pack activation), unlike the two confirmed
"no real effect" precedents (`Filesystem.Resolve`, the `Lsp` test
fixture).

**`Process`** (done): the last small family. `run` and `perform` already
had matching return types (`Process` has no `Response` enum, just the
`Result` case class) and `Request.Run` already had the same default
parameters as `run`, so both call-site migrations were direct
substitutions with no restructuring — `surface.Transcript.scala`'s real
production caller (verifying a Rosetta scala port by running the host
toolchain) and `ExternalBackend`'s internal cross-object call, which now
goes through `Process.perform` too instead of bypassing it via the old
raw `run` — so `ExternalBackend.perform`'s own gate no longer has an
ungated escape hatch into `Process`.

**`Workspace`/`PackFiles`** (done): the last of the 8 families — 6 call
sites, all in `runtime/PackLoader.scala`, itself the system's composition
root (its own docstring: *"Implements `PackAccess` so User code never
imports this module directly"*). `system-handler/PackFiles.scala` actually
defined two objects: `Workspace` (the real handler) and `PackFiles` (a
pure compatibility alias re-exporting `Workspace`'s methods, kept because
"call sites and `PackLoader` historically used `PackFiles`"). Confirmed
`PackFiles` had exactly one consumer anywhere — `PackLoader`'s 6 call
sites — so once `PackLoader` migrated onto `Workspace.perform`, `PackFiles`
became fully dead and was **removed entirely** rather than left as an
empty shim with nothing left to be compatible with. `PackLoader`'s own
established convention (throw on failure, not `Either`-propagation) was
preserved via two small private unwrap helpers, rather than threading
`Either` through `PackLoader`'s public API (used throughout `runtime`/
`examples`/`tests` — changing those signatures would be a much bigger,
unrelated risk). No public `PackLoader` signature changed.

**All 8 effect families are now capability-gated.** Every `perform` is the
sole entry point for its family, with every convenience method either
`private` or (for the two confirmed "no real effect" cases — `Filesystem.
Resolve`, `Lsp`'s test-fixture `writeMessage`/`readMessage`) left public
with the reasoning documented inline. All eight still run in `Audit` mode
(never blocks) with the `Subject("local")` placeholder — flipping any
family to `Enforce`, and replacing that placeholder with real injected
capabilities, remain open (the latter is priority #2's territory).

## Replacing ambient globals (post-migration priority #2)

Two ambient globals were named: `AuthorityGate` (mutable `@volatile`
`mode`/`policies`/`events` on a singleton `object`, just wired into all 8
gated handlers) and `PackAccess` (a process-global `Option[PackAccess]`
with a classloader hack, read by 5 `user/` language packs). The user chose
to start with `AuthorityGate` (smaller blast radius, more urgent) and to
scope the first slice as "instantiable + a default instance" rather than
full injection everywhere at once.

**`AuthorityGate`** (done, first slice): converted from a singleton
`object` with module-level `@volatile var mode`/`policies` into a `final
class AuthorityGate`, matching the `Node`/`Cas`/`DiskCas`/`Branches`
pattern already used elsewhere in `system-handler` (real, established
precedent — these are already constructed instances, not singleton
objects) — `mode`/`policies` are now constructor parameters, `events` an
instance-level buffer, no shared mutable JVM-wide state. `AuthorityGate.
default` is a process-wide instance the 9 existing call sites (all 8
handlers + `Node.append`) now reach via `AuthorityGate.default.checked`/
`.check` instead of the bare module-level call — a one-line change per
file, no other logic touched. `AuthoritySuite`'s tests previously mutated
the shared singleton via a `beforeEach` reset (`clearPolicies()`,
`setMode(Audit)`, `drainEvents()`) — the textbook symptom of ambient
global state, where tests must cooperatively reset a hidden shared thing.
Each test now constructs its own local `AuthorityGate()` inline instead —
genuinely isolated regardless of test execution order or grouping, not
just reset-before by convention.

**`PackAccess`** (done, second slice — smaller cut than a full injection
rewrite, per the user's choice): removed two things rather than
redesigning the whole registry. The `Class.forName("cairn.runtime.
PackLoader$")` reflection fallback in `get` existed to force
`runtime.PackLoader`'s object initializer (which calls
`PackAccess.install(this)`) to run without `system-interface` having a
compile-time dependency on `runtime`. Research found `PackLoader` is
referenced directly and constantly elsewhere (`examples/pki/Pki.scala`,
`examples/riemann/Riemann.scala`, `examples/search/Search.scala`, dozens
of test call sites) — something else always touches `PackLoader` first in
every real run, so the reflection hack was dead-in-practice. Verified
empirically (not just by static reading, since that can't fully prove a
reflection fallback is unneeded): removed it, ran the full suite plus
both transcripts plus an isolated run of the three test suites that
exercise `PackAccess`-dependent `user/` objects (`LeanCoreSuite`,
`MiniTTSuite`, `UnisonCoreSuite`) — all green, confirming nothing actually
needed it. Also removed `PackAccess.withInstalled` (a "test helper... "
escape hatch with zero callers anywhere in the repo — unlike
`AuthorityGate`'s equivalent reset pattern, which was actually used, just
badly, this one was never used at all).

**Explicit injection — DONE.** Language packs are classes taking
`PackAccess` (`Law(packs)`, `MiniTT(packs)`, …). Handlers take an explicit
`AuthorityGate` on `perform`. `Node(root, gate)`, `PackLoader(workspaceGate)`,
`Lsp.serve(..., gate)`, `BrowserServer.serve(..., gate)`, and `Cli.main` /
`Transcript.run` receive distinct bootstrapped gates at composition roots
(`examples.Main`, tests). There is no `PackAccess.get`/`install`, no
`AuthorityGate.default`/`forFamily` registry — each composition root
constructs fresh gates and passes them down.

## Enforce mode is now genuinely running (post-migration priority item 2)

Prior finding: `Mode.Enforce` had **never actually run in the real
program** — the only code anywhere calling `setMode(Mode.Enforce)` was
`AuthoritySuite`'s tests, on their own throwaway local `AuthorityGate`
instances, never on `AuthorityGate.default`. Despite `MIGRATION-PLAN.md`
describing Phase 5 as "Enforce mode + `LedgerAppend` gate on
`Node.append`... Done," the real singleton sat in `Audit` mode the whole
time.

All 9 gated call sites (8 handlers + `Node.append`) share one
`AuthorityGate` instance, and `Mode` is a property of that shared
instance, not per-family — so there is currently **no way to enforce one
family while auditing another** (the "remaining families opt in
incrementally via `AuthorityGate.check`" language elsewhere in this doc
described a per-family granularity the current single-shared-instance
design doesn't actually support — a real gap, noted rather than silently
left inconsistent). The user chose the simpler of two fixes: enforce
everything at once through the shared instance, rather than giving each
family its own gate for real incremental rollout (bigger, separate design
work).

`AuthorityGate.default` now installs one bootstrap policy per known
`Effects.Action` (`gate.install(Effects.Action.values.toList.map(...))`)
and runs in `Mode.Enforce`. First attempt used subject `Subject("local")`
only (matching the 8 effect handlers' placeholder) — empirical
verification (the full suite, not just compilation) caught a real bug:
`Node.append` authenticates as the actual signing authority
(`Subject(authority.name)`, e.g. `"alice"`), not `"local"`, so every
ledger-touching test failed under real enforcement. Fixed by using
subject `"*"` (any subject) in the bootstrap policy instead.

Stated honestly: this proves the `Authority.validate`/Kernel-checked code
path works end-to-end in the real program for the first time, but it is
**not** meaningful access control — a blanket allow-everyone-everything
policy can't deny anything. Real enforcement needs real, distinct
subjects and narrower policies, which needs real identity.

## Deferred work, tackled: per-family Enforce + the PackAccess bug

A full call-graph audit (an Explore agent) mapped every file "no ambient
fallback anywhere" would touch before doing it, and found two things that
reshaped scope:

- Literal "no ambient fallback ever" for `AuthorityGate` means touching
  **~30 files**, a dozen of them independent test suites (`Phase5Suite`,
  `Phase7Suite`, `Phase8Suite`, `WaveGSuite`, `BrowserSuite`, `ParitySuite`,
  etc.) that construct `Node`/call `.append` directly and don't test
  authority behavior at all. 4 of the 9 `AuthorityGate` call sites
  (`Random`, `Clock`, `Terminal`, `ExternalBackend`) have **zero real
  callers anywhere** in the repo.
- **There is no real composition root today.** `runtime.PackLoader` is
  documented as one, but its `PackAccess.install(this)` self-install only
  actually runs because of *accidental* JVM class-init ordering:
  `examples.Main`'s pack map happens to evaluate `Pki` (which touches
  `PackLoader` directly) before `Sds`/`Law` (which need `PackAccess.get`);
  test suites rely on some *other* test class in the same JVM run having
  touched `PackLoader` first; `tests/FuzzSuite.scala` has **no
  `PackLoader` reference of its own at all** and only works by luck of
  sbt's test-class ordering — one import/map-key reorder away from
  `RuntimeException("PackAccess not installed...")` at startup. A real
  latent bug, not just a style concern.

**`AuthorityGate`: per-family registry (done)** — resolves the "per-family
Enforce granularity" gap named above via `AuthorityGate.forFamily(family:
Effects.Family): AuthorityGate`, a lazily-bootstrapped registry replacing
the single shared `default`. Each family now gets its own instance, so
`AuthorityGate.forFamily(Family.Filesystem).setMode(Mode.Audit)` affects
only `Filesystem`, leaving every other family in `Enforce` — proven by two
new `AuthoritySuite` cases (distinct instances per family; the same
family always returns the same instance). All 9 call sites changed from
`AuthorityGate.default.checked(...)`/`.check(...)` to
`AuthorityGate.forFamily(Effects.Family.<X>).checked(...)`/`.check(...)`
— purely internal to each handler, zero external call-site changes.
**The literal 30-file full-injection rewrite was considered and not
done**: the cost (rewriting test suites that don't exercise authority at
all) didn't match the benefit over this registry. Available as separate
future work if still wanted.

**`PackAccess`: composition-root bug — fixed, correcting an earlier
mistake.** Two slices ago ("PackAccess — remove reflection bootstrap and
dead escape hatch," commit `7251f34`), the `Class.forName("cairn.runtime.
PackLoader$")` fallback in `PackAccess.get` was removed as apparently-dead
code — an empirical check (full suite, both transcripts, an isolated run
of the three `user/`-dependent test suites) all passed without it. That
check was **not actually testing the failure mode**: every real call path
in the repo happens to have *something else* touch `PackLoader` first
(accidental JVM-wide class-init ordering within the one shared `sbt test`
JVM), so removing the fallback couldn't have failed any existing test —
it just removed the one thing that made correctness *not* depend on that
accident. The `Class.forName` fallback is the **only** available
mechanism for forcing `PackLoader`'s installation without
`system-interface` depending on `runtime` at compile time (the dependency
must run the other way, per `PackAccess`'s own docstring) — restored,
with the real mechanism now documented in place of the original
"composition-root bootstrap" hand-waving. `withInstalled` (the *other*
thing removed in that slice) stays removed — confirmed genuinely dead,
no ordering-safety role. Honest limitation: this fix can't be verified
against a truly cold JVM within the existing single-JVM test harness (see
`tests/FuzzSuite.scala`, which has no `PackLoader` reference of its own at
all and only worked by luck of test-class ordering) — verified by code
inspection (restoring the exact prior mechanism) and the full suite
continuing to pass, not by reproducing the failure mode itself.

## Forbidden-import rules (ModuleBoundarySuite)

- `kernel` / `core`: no filesystem, networking, or process APIs
- `user`: no `cairn.systemhandler`, no `java.nio.file` / net / process

## Intentional residuals

- **BFT / gossip daemon / peer discovery** — deferred forever (docs/distribution.md)
- **Separate `grammar.cairn`** — deferred (docs/bootstrap.md)
- **PKI/Search/Riemann host glue, Claims, SDS sealing tutorials** — remain in `examples/` because they need handler crypto/CAS/filesystem; pure language defs that can live without handlers are in `user/`
- **Facade modules** (`workbench`, `proof`, `compute`, `ledger` re-exports, `rosetta.Scaffold`) retained as documented compatibility shims
- **Full AuthorityGate/PackAccess injection** — **DONE**: explicit constructor/`perform` params; composition roots build `AuthorityGate.bootstrapped()` + `PackLoader(gate)` and pass them; no ambient `get`/`install`/`forFamily`/`default`

## Final principle

```text
User defines intent.
Core derives consequences.
System Interface names possible interactions.
Kernel grants authority and validates results.
System Handler touches the world.
Runtime composes User + Handlers.
```
