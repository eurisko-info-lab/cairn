# Architecture: trust and effect boundaries

Present-tense source of truth for Cairn’s Kernel / Core / System / User /
Runtime split. Chronological migration notes and superseded designs live in
[migration-history.md](migration-history.md). The operational refactor checklist
remains [MIGRATION-PLAN.md](../MIGRATION-PLAN.md) (phases 0–8 landed).

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

## EffectContext and AuthorizedEffect

Composition roots own authorization:

1. Construct an `EffectContext` (`forPackLoader()`, `forLedger()`,
   `forFilesystem()`, `bootstrapped()`, or `local(gate)`).
2. `ctx.authorize(req)` → opaque `AuthorizedEffect` (Kernel
   `AuthorizedRequest`).
3. Handlers `perform(req, auth: AuthorizedEffect)` only — no raw request+gate
   path. Thin `run(req, ctx)` adapters authorize then perform.

There is no ambient `PackAccess.get`/`install`, no `AuthorityGate.default` or
`forFamily` registry. Language packs are classes taking `PackAccess`
(`Law(packs)`, …). `Node`, `PackLoader`, `Lsp`, `Browser`, and `Cli` receive
gates / contexts from composition roots (`examples.Main`, tests).

`EffectContext.capabilities` holds Kernel-minted `VerifiedCapability` values
only (`VerifiedCapability.fromProof`). A covering verified grant is Kernel-checked
without broad policy re-evaluation; empty capabilities fall back to Core
`prove` → Kernel `checkProof`. Raw `CapabilityGrant` construction remains for
proof / delegation building — it cannot enter `withCapabilities`.
Nonce / `requestId` replay uses an issuer-scoped `ReplayStore` on the gate
(default in-memory; durable `ReplayStore.filesystem` may be shared across gates).
Snapshots publish as CAS `replay-snapshot` digests (`ReplayStore.publish` /
`mergeFromCas`) for multi-node absorb — **merge, not distributed consensus**.
Capability revocation digests sync via the same want/have shape
(`ReplayReplication` / `RevocationLog`) — BFT deferred.

## Authority

- **Live effect families (Meta-defined):** Filesystem, Workspace, Process,
  Clock, Random, Terminal, Lsp, ExternalBackend, Cas, LedgerTransport. Each
  has an `EffectMeta.EffectFamily` (Fragment + `requestActions` + digest-bound
  `ActionKey` / `ResourceSchema`). Completeness is checked by
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
  roots and suites use narrow gates.
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
| `BranchManifest` | `kernel` | Accepted branch state + causal digests |
| `Branches` | `system-handler` | Effectful refs; accepts only `ValidatedTip` |
| `Provenance` | `system-handler` | Records `semantic-merge` edges for `cairn why` |
| CLI `repo` | `surface` | `cairn repo branches` / `cairn repo demo` |

`ValidatedChangeSet` is opaque: minted by `Delta.apply` / `ValidatedChangeSet.check`
(replay). `decodeClaim` does not mint. `ValidatedTip` requires
`apply(language, base, change) = tip`; `Branches.commitTip` accepts only
`ValidatedTip`. Loaded histories are replay-checked before merge.

`BranchManifest` carries `causalHistoryRoot`, `parents`, `acceptedChange`,
`changeHistory`, `conflictState` (CAS digests). Refs `.change` / `.changes`
sidecars remain write-through caches; `loadChangeHistory` / `loadChange`
prefer manifest digests. `mergeBranches` finds a causal LCA by shared
module-result digests (not only identical linear change prefixes), then merges
divergent suffixes.
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

- **BFT / gossip daemon / peer discovery** — deferred ([distribution.md](distribution.md))
- **Separate `grammar.cairn`** — deferred ([bootstrap.md](bootstrap.md))
- **PKI/Search/Riemann host glue, Claims, SDS sealing tutorials** — remain in
  `examples/` (need handler crypto/CAS/filesystem); pure packs that need no
  handlers live in `user/`
- **Facade modules** (`workbench`, `proof`, `compute`, `ledger` re-exports,
  `rosetta.Scaffold`) — documented compatibility shims
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
  `ActionKey.fromPinned`). Host-embedded Meta fragments remain the bootstrap
  vocabulary; thin reduction via `languages/effect-clock.cairn` +
  `languages/effect-random.cairn` (`clockFromFragment` / `randomFromFragment`).
- **Replay sync** — `ReplayStore` snapshots publish/merge via CAS digests
  (issuer-scoped absorb). Revocation via `ReplayReplication` / `checkGrant`.
  Not multi-node consensus / BFT.
- **SDS vs report formats** — SDS (`languages/sds.cairn`) is semantic only.
  JSON/XML/CSV(+ deferred PDF) are `sds-report` projection surfaces that
  *consume* SDS modules — not SDS constructors.
- **Phase0 MemCas/DiskCas + WaveA M4 algo agility** — intentional direct
  trait-contract tests (no authority surface). Branch seeds, admin, chunking,
  Unison host glue, sync paths, Browser stats, provenance `why`, Branches
  refs FS, and Node/Sync chain-file I/O go through `CasEffects` /
  `CasAdminEffects` / `Filesystem` (`forBranches` / `forLedger` /
  `forFilesystem`).

## Final principle

```text
User defines intent.
Core derives consequences.
System Interface names possible interactions.
Kernel grants authority and validates results.
System Handler touches the world.
Runtime composes User + Handlers.
```
