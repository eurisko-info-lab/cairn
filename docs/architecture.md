# Architecture: trust and effect boundaries

Present-tense source of truth for Cairn's Kernel / Core / System / User /
Runtime split, current test health, and parity against the summarized prior
projects (§13 of [CAIRN-PROMPT.md](../CAIRN-PROMPT.md)). This file describes
what *is*, not the history of how it got here.

## Areas

| Area                 | Responsibility                                                        | Trust status                                     |
| --------------------- | ---------------------------------------------------------------------- | ------------------------------------------------- |
| **Kernel**            | Validate artifacts, languages, proofs, changes, authority, transitions | Semantic TCB                                      |
| **Core**              | Parse, derive, elaborate, search, evaluate, project, merge, propose    | Pure but not automatically trusted                |
| **System Interface**  | Define effect operations, resources, requests, responses              | Pure platform contract                            |
| **System Handler**    | Perform filesystem, process, crypto, network, persistence, UI effects  | Operationally privileged, semantically untrusted  |
| **User**              | Define languages, policies, programs, proofs, changes, workflows       | Extensible data                                   |
| **Runtime**           | Composition root — PackLoader, CLI wiring                              | Ties User + Handlers together                     |

Purity alone does not place Core outside the TCB. Every Core result used for
acceptance has an independent Kernel validation path.

## Module graph

```text
kernel
core                → kernel
system-interface    → kernel
system-handler      → kernel, core, system-interface
user                → kernel, core, system-interface   (↛ system-handler)
runtime             → user, system-handler, core, kernel, system-interface

# Aggregation above the DAG (not CAS/Meta/authority owners)
proof               → core, kernel
rosetta             → proof, core, system-handler
surface             → proof, runtime, system-handler
examples            → surface, user, runtime   # demos / host glue
tests               → examples, rosetta, runtime, user
```

`workbench` and `ledger` (former re-export façades) have been retired outright
— their real owners (`runtime.PackLoader`, `system-handler.{Node,Encryption,...}`,
`user.policy.PolicyLang`) are imported directly now.

Key prohibition: `user ↛ system-handler`.

## Digests and surfaces

- **CAS keys:** `Digest` (content) and `TypedKey` (kind + digest). Typed-key
  mismatch is a structured error; disk CAS detects corruption.
- **Artifacts:** Kernel `Artifact` / `ArtifactKind` (including proofs,
  certificates, branch manifests, change-sets, agreement certificates).
- **Surfaces:** grammar parse/print round-trips; Meta fragment IR; language
  packs under `languages/` and `user/`.
- **Ports:** Rosetta projections are obligations, not Kernel-checked host
  proofs — see [rosetta.md](rosetta.md).

## EffectContext, AuthorizedEffect, and AuditedEffect

Composition roots own authorization:

1. Construct an `EffectContext` (`forPackLoader()`, `forLedger()`,
   `forFilesystem()`, `bootstrapped()`, or `local(gate)`), optionally with a
   disk-loaded `RuntimeEffectRegistry` from `EffectBootstrap.Loaded.registry`
   and a `RevocationView`.
2. `ctx.authorize(req)` → opaque `AuthorizedEffect` (Kernel
   `AuthorizedRequest`) — **Enforce only**. Handlers resolve ActionKeys via
   `ctx.registry`.
3. Handlers `perform(req, auth: AuthorizedEffect)` only — no raw request+gate
   path. Thin `run(req, ctx)` adapters authorize then perform.
4. Audit mode uses `ctx.recordAudit` / `AuthorityGate.audit` → `AuditedEffect`
   (`AuditedRequest`). Audit **cannot** mint `AuthorizedEffect`.

There is no ambient `PackAccess.get`/`install`, no `AuthorityGate.default` or
`forFamily` registry. Language packs are classes taking `PackAccess`
(`Law(packs)`, …). `Node`, `PackLoader`, `Lsp`, `Browser`, and `Cli` receive
gates / contexts from composition roots (`examples.Main`, tests).

`EffectContext.capabilities` holds Kernel-minted `VerifiedCapability` values
only (`VerifiedCapability.fromProof`). A covering verified grant is Kernel-checked
with mandatory `RevocationView` consultation (stable `CapabilityGrant.capabilityId`)
before mint; empty capabilities fall back to Core
`prove` → Kernel `checkProof`. Raw `CapabilityGrant` construction remains for
proof / delegation building — it cannot enter `withCapabilities`.
Nonce / `requestId` replay uses an issuer-scoped `ReplayStore` on the gate
(default in-memory; durable `ReplayStore.filesystem` may be shared across gates).
Snapshots publish as CAS `replay-snapshot` digests (`ReplayStore.publish` /
`mergeFromCas`) for multi-node absorb — **merge, not distributed consensus**.
Capability revocation digests sync via the same want/have shape
(`ReplayReplication` / `RevocationLog`) and feed the live authorize path via
`RevocationView`. Explorer **Trust**
tab views/manages revocations and delegation hops (`DelegationLog` /
CAS `capability-delegation`) — not Studio.

## Authority

- **Live effect families (Meta-defined):** Filesystem, Workspace, Process,
  Clock, Random, Terminal, Lsp, ExternalBackend, Cas, LedgerTransport. Each
  has an `EffectMeta.EffectFamily` (Fragment + `InterfaceDecl` /
  `requestActions` + digest-bound `ActionKey` / `ResourceSchema` with typed
  `PathPattern`: `Path` / `Command` / `Host` / `Digest` / `Digest|Path` /
  `*`). Disk SoT via `EffectBootstrap`; completeness checked by
  `EffectMetaSuite`.
- **Gating:** every live family’s `perform` is the sole public effect entry
  point; convenience methods are private (or documented ungated exceptions
  such as pure `Filesystem.Resolve` and LSP test-fixture framing). Cas trait
  put/get/contains go through `CasEffects`; admin fsck/gc/stats through
  `CasAdminEffects`; LedgerTransport `append` through `LedgerTransport.run`
  (`Node.append` is a thin adapter).
- **Sync:** `Sync.pull` / `HttpSync.pull` compose CAS `contains` / get / put
  as `Either` — authorized failures abort and do **not** advance the consumer
  chain with a partial blob set (`Either.forall` is never used to treat denial
  as “missing”).
- **Mode:** Enforce is live at composition roots. Narrow deployment policies:
  PackLoader (`packLoaderWorkspace`), ledger+CAS+chain FS (`forLedger`), process
  (`forProcess`), LSP (`forLsp`), backends (`forBackend`), CAS (`forCas`),
  branches (`forBranches` = CAS + refs FS), filesystem (`forFilesystem` —
  CLI Transcript/Cli home/run/ui paths, hash/put/canon/transcript source,
  load-language, emit-languages, Browser UI filesystem fallback, Riemann/Search
  tutorial artifact I/O).
  `bootstrapped()` remains available for rare allow-all fixtures; composition
  roots and suites use narrow gates. `forProcess` defaults to
  `scala-cli`/`cargo`/`runghc`/`lake`/`git` (not `*`).
- **Capabilities:** composition-root factories accept optional
  `VerifiedCapability` grant bundles; non-empty lists take the capability-first
  authorize path (SDS causal workflow + AuthoritySuite).
- **Resource matching:** exact path, full `*`, or explicit `prefix*` — never
  accidental prefix of a non-wildcard path.
- **Meta conditions:** known `meta:*` keys validate value shape fail-closed
  (e.g. `meta:expiresAtEpochMillis = "banana"` does not match).
- **Calculus:** fail-closed conditions; grant expiry/nonce; replay denial;
  delegation chains fully justify the **root grant** (expiry, nonce, resource,
  conditions) against cited policies **before** hop validation — widened TTL
  or omitted policy nonce on a constructed root is rejected; final grant covers
  the grantee request; Kernel `AttenuationWitness` checks `parentCanon` and
  forbids subject changes except via Delegation.
- **Proofs:** Core `PolicyEval.prove` / `proveDelegated` builds
  `AuthorizationProof`; Kernel `checkProof` validates the witness. Proof
  `canon` includes request, full cited policies, attenuation, and delegation
  links so distinct proofs do not collide.
- **VerifiedCapability:** minted only after `checkProof` / covering
  `checkCapability`; see EffectContext above.

## Semantic repository spine

```text
branch state
→ causal semantic change   (Delta.apply / SemanticRepository.commit)
→ dependency validation    (opaque ValidatedChangeSet / ValidatedTip)
→ commutation              (ChangeAlgebra.commutes)
→ merge                    (Merge.threeWay / common-ancestor suffixes)
→ conflict artifact        (Merge.Conflict → CAS)
→ migration                (Migrate, optional)
→ accepted new branch state (Branches.merge → BranchManifest head)
```

| Piece | Module | Role |
| ----- | ------ | ---- |
| `SemanticRepository` | `core` | Pure orchestration; proposes `Tip`, mints `ValidatedTip` |
| `Delta` / `ChangeAlgebra` / `Merge` / `Migrate` | `core` | Engines; opaque `ValidatedChangeSet` via apply/check |
| `PatchGraph` | `core` | Explicit causal patch DAG (parent edges + LCA); thin vs full Pijul |
| `BranchManifest` | `kernel` | Accepted branch state + causal digests |
| `Branches` | `system-handler` | Effectful refs; `commitTip` = ΔL path; `importModule` = bootstrap/import only |
| `Provenance` | `system-handler` | Records `semantic-merge` edges for `cairn why` |
| CLI `repo` | `surface` | `cairn repo branches` / `cairn repo demo` |

`ValidatedChangeSet` is opaque: minted by `Delta.apply` / `ValidatedChangeSet.check`
(replay). `decodeClaim` does not mint. `ValidatedTip` requires
`apply(language, base, change) = tip`; `Branches.commitTip` accepts only
`ValidatedTip`. **`importModule` (formerly `commitModule`) is bootstrap/import
acceptance** — plants a module tip without a ValidatedChangeSet; it is not the
ordinary ΔL path. Loaded histories are replay-checked before merge.

`BranchManifest` carries `causalHistoryRoot`, `parents`, `acceptedChange`,
`changeHistory`, `conflictState` (CAS digests), plus **domain ancestry**:
`primaryAncestor` (strongest binding, or `None` = hang off the ledger trunk)
and `references` (soft cross-domain ancestors). The ledger / blockchain is the
global trunk of domains (like DNS roots); e.g. `LAW` off the trunk, `SDS`
with primary `LAW` and a reference to `CHEMISTRY`. Kernel `DomainBranch.wellFormed`
checks names; `Branches.forkFrom` / `referTo` plant and extend that graph.
Refs `.change` / `.changes` sidecars remain write-through caches;
`loadChangeHistory` / `loadChange` prefer manifest digests. `mergeBranches`
prefers `PatchGraph` DAG LCA when change-set digests form an explicit parent
graph, falling back to shared module-result digests, then merges divergent
suffixes.
Branch accepts are journaled: CAS blobs → accept journal → refs → optional
ledger publish → journal clear (`recoverPendingAccepts` rolls forward).
`Branches.reclaimOrphanBlobs(casRoot)` recovers then mark/sweeps via
`liveCasRoots` (refs, change/conflict sidecars, pending journals). Crash
before the journal is written leaves unreferenced blobs until reclaim.

Ledger `SetBranchHead` is **opt-in** via `Branches.publishHead` or
`merge(..., publish = Some(...))`. `Branches` CAS / refs FS go through
`EffectContext.forBranches`. Node/Sync/HttpSync chain FS uses `forLedger`.
CLI / tutorial FS uses `forFilesystem`. Browser board discovery authorizes CAS
`stats` then walks via `CasAdminEffects.artifacts`.

## Agreement envelopes (Lean · HVM)

LeanCore and AffineNet/IcNet are **Cairn-native** calculi in those lineages —
not Lean-kernel or HVM-ABI compatible. Boundaries:

- Doc: [agreement.md](agreement.md)
- Types: `cairn.core.Agreement` (`Envelope`, `AgreementCertificate` with
  `envelopeDigest` + `nativeEvidence`, `check` / `certify`)
- Tests: `AgreementSuite` — always Cairn reference; optional `lean` on PATH;
  classical-IC goldens for nets; `HvmSurface` HVM2 export; live `hvm run` when
  on PATH

Rosetta LeanPort remains projection + `sorry` obligations, orthogonal to the
LeanCore `#check` envelope.

## Forbidden-import rules (ModuleBoundarySuite)

- `kernel` / `core`: no filesystem, networking, or process APIs
- `user`: no `cairn.systemhandler`, no `java.nio.file` / net / process

## Intentional residuals

- **BFT / gossip daemon / peer discovery** — gossip daemon + peer discovery
  deferred; `BftQuorum` is an in-process research/sim slice (`f < n/3`), not
  production finality ([distribution.md](distribution.md))
- **Separate `grammar.cairn`** — deferred ([bootstrap.md](bootstrap.md))
- **PKI/Search/Riemann host glue, Claims, SDS sealing tutorials** — remain in
  `examples/` (need handler crypto/CAS/filesystem); pure packs that need no
  handlers live in `user/`
- **Aggregation modules** (`proof`, `rosetta.Scaffold`) — thin layers above
  the trust-tier DAG (certification helpers, scaffold emit), not owners of
  CAS/grammar/ΔL/Meta/authority. `workbench`, `compute`, and `ledger` — former
  re-export façades — have been retired; callers import the real owners
  directly.
- **HVM surface limits** — `HvmSurface` projects the envelope corpus to HVM2
  CON/DUP/ERA books (not full ABI / Bend / HVM5); live `hvm` optional on PATH
- **Semantic merge** — everyday path is `commitTip(ValidatedTip)` →
  `mergeBranches` (causal LCA by shared module-result digests; replay-checked
  histories; causal digests on `BranchManifest`). Ledger publish remains
  **opt-in**. Accepts are journaled across CAS + refs + optional ledger
  (`recoverPendingAccepts`); `reclaimOrphanBlobs` is the mark/sweep reclaim
  path for unreferenced accept blobs (`liveCasRoots`).
- **Effect-interface pinning** — `ActionKey` is digest-bound via
  `EffectMeta` Fragment digests; families load as CAS-pinned
  `effect-interface` artifacts (`EffectMeta.PinnedInterface` /
  `ActionKey.fromPinned`). Bootstrap path: primordial Meta → load
  `languages/effect-interface.cairn` → load each `effect-*` vocabulary +
  `iface.cairn` declaration module (`EffectBootstrap`) → derive actions /
  resources / pins. **No hand-maintained Action enum** — ActionKeys register
  from packs. Residual: `Effects.Family` thin JVM routing tag (ids ↔ packDecls)
  + cold-start Fragment / `packDecls` seeds (fixpoint-checked against disk).
- **Replay sync** — `ReplayStore` snapshots publish/merge via CAS digests
  (issuer-scoped absorb). Revocation via `ReplayReplication` / `checkGrant`.
  Not multi-node consensus; `BftQuorum` is separate research/sim.
- **SDS vs report formats** — SDS (`languages/sds.cairn`) is semantic only.
  JSON/XML/CSV/PDF are `sds-report` projection surfaces that *consume* SDS
  modules — not SDS constructors. Host residual: `toCst`; print path is
  `SectionReport.printSurface`; PDF bytes via `PdfMinimal`.
- **Dirty-subtree (M7)** — `dirtyOps` / `putReassociated`: structural equality
  + LCS delete alignment; inserts without an original span still parent-reprint.
- **SDS workflow / evidence packs** — `languages/sds-workflow.cairn` +
  `causal.cairn` declare the author→…→publish sequence (`workflowStepOk` /
  `workflowPhaseOk`). `languages/sds-certificate.cairn` +
  `workflow-kinds.cairn` declare approval/sign/publication kinds
  (`certificateKindOk`). `SdsCausalWorkflow` is the thin effectful driver.
  Residual: CAS/Branches/Ed25519/ledger orchestration stays Scala.
- **Phase0 MemCas/DiskCas + WaveA M4 algo agility** — intentional direct
  trait-contract tests (no authority surface). Branch seeds, admin, chunking,
  Unison host glue, sync paths, Browser stats, provenance `why`, Branches
  refs FS, and Node/Sync chain-file I/O go through `CasEffects` /
  `CasAdminEffects` / `Filesystem` (`forBranches` / `forLedger` /
  `forFilesystem`).

## Test health and golden digests

Full suite: `sbt test`. Golden digests for every shipped language and Rosetta
artifact: `sbt "examples/runMain cairn.examples.Main digests"` — checked
against the values recorded in CI (`.github/workflows/ci.yml`), not frozen
here, since they change whenever a language's semantic content changes.
Both bootstrap transcripts (`sbt "examples/runMain cairn.examples.Main
transcript transcripts/mvp.cairn"` and `.../max.cairn`) exercise the full
publish/fetch/gossip/query path end to end. `emit-languages` regenerates the
checked-in `.cairn` mirrors for `pki`/`law`/`sds`/`search`/`stlc`/`meta`;
`git diff --exit-code languages/` must be clean after running it — CI enforces
this (the language-sync check).

## Parity vs summarized sources (§13)

Constitution rule 16 requires matching each summarized source's top-level
(README-advertised, flagship) surface — not every deep experiment. Current
state, at the level that stays true across individual pack changes:

| Source | Top-level capability | State |
|---|---|---|
| GRANITE | Workbench: fragments, grammar-as-data, ΔL, CAS, meta bootstrap | Parity — Waves A–C, H1; `languages/meta.cairn` fixpoint |
| GRANITE | PKI pack: registry, ΔPKI, chain validation, tutorial, ledger publish | Parity — `languages/pki.cairn` + `PkiMax`/`DemoPki`/`PkiTutorial` |
| GRANITE | Sharing encryption (X25519 hybrid seal) | Parity — `system-handler.Encryption`; seal/open tests |
| GRANITE | SDS flagship spine: objects, ΔSDS, shadow, multilingual, sealing, tutorial, publish, causal workflow | Parity — `languages/sds.cairn` + `eu-clp` + `sds-report` (incl. PDF) + `sds-workflow`/`sds-certificate` packs |
| GRANITE | Law pack (PKI→Law→SDS) | Parity (thin) — `languages/law.cairn` requires `cert`; `LawTutorial` |
| GRANITE | Computation / Bend profile | Parity — `AffineNet`/`IcNet`/`Bend` (GRANITE Bend is spec-only) |
| GRANITE | SDS Studio UI / auth web app | N/A — explicit anti-goal (§8), not attempted |
| ROSETTA | QuickSort Ord + effects + multi-host ports + sample entrypoints | Parity — `QuickSort2` + `QuickSortApp`; four host ports pass the whole-file byte fixpoint |
| granit-rust | MetaLego grammar VM + CAS + PoA + Unison-as-fragments CLI demos | Parity — L0–L6 engines; Unison pack; CLI/transcript |
| marblego | Grammar VM + artifact/branch/store crates | Parity (absorbed) — same story in Scala |
| MetaLego / Kiwi | Composable fragments + STLC | Parity — STLC fragments + pushout |
| Eurisko / HVM | Lattice meta / IC lineage | Envelope, not full compatibility — see Agreement envelopes above |

Full granit-rust's MetaLego catalog of other host languages (Unison/ASN.1/JVM/…)
is absorbed as a platform capability here, not forked into separate packs —
a deliberate scope decision, not a gap.

## Final principle

```text
User defines intent.
Core derives consequences.
System Interface names possible interactions.
Kernel grants authority and validates results.
System Handler touches the world.
Runtime composes User + Handlers.
```
