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

1. Construct an `EffectContext` (`forPackLoader()`, `bootstrapped()`, or
   `local(gate)`).
2. `ctx.authorize(req)` → opaque `AuthorizedEffect` (Kernel
   `AuthorizedRequest`).
3. Handlers `perform(req, auth: AuthorizedEffect)` only — no raw request+gate
   path. Thin `run(req, ctx)` adapters authorize then perform.

There is no ambient `PackAccess.get`/`install`, no `AuthorityGate.default` or
`forFamily` registry. Language packs are classes taking `PackAccess`
(`Law(packs)`, …). `Node`, `PackLoader`, `Lsp`, `Browser`, and `Cli` receive
gates / contexts from composition roots (`examples.Main`, tests).

`EffectContext.capabilities` grant-bundle threading remains a placeholder.

## Authority

- **Live effect families (Meta-defined):** Filesystem, Workspace, Process,
  Clock, Random, Terminal, Lsp, ExternalBackend. Each has an
  `EffectMeta.EffectFamily` (Fragment + `requestActions` + derived
  `ActionKey` / `ResourceSchema`). Completeness is checked by
  `EffectMetaSuite`.
- **Host-only bridges:** Cas (trait contract) and LedgerTransport
  (`LedgerAppend` on `Node.append`) stay outside the Meta Request/Response
  mechanism.
- **Gating:** every live family’s `perform` is the sole public effect entry
  point; convenience methods are private (or documented ungated exceptions
  such as pure `Filesystem.Resolve` and LSP test-fixture framing).
- **Mode:** Enforce is live at composition roots. PackLoader uses a narrow
  policy (`PolicyEval.packLoaderWorkspace`: subject `local`,
  `WorkspaceRead`, resource `languages*`). Other roots still use allow-all
  `bootstrapped()` until scoped.
- **Calculus:** fail-closed conditions; grant expiry/nonce; replay denial;
  delegation chains; Kernel `AttenuationWitness`.
- **Proofs:** Core `PolicyEval.prove` builds `AuthorizationProof`; Kernel
  `checkProof` validates the witness. Minting requires prove → checkProof.

## Semantic repository spine

```text
branch state
→ causal semantic change   (Delta.apply / SemanticRepository.commit)
→ dependency validation    (ValidatedChangeSet)
→ commutation              (ChangeAlgebra.commutes)
→ merge                    (Merge.threeWay)
→ conflict artifact        (Merge.Conflict → CAS)
→ migration                (Migrate, optional)
→ accepted new branch state (Branches.merge → BranchManifest head)
```

| Piece | Module | Role |
| ----- | ------ | ---- |
| `SemanticRepository` | `core` | Pure orchestration of the story above |
| `Delta` / `ChangeAlgebra` / `Merge` / `Migrate` | `core` | Existing engines composed, not rewritten |
| `BranchManifest` | `kernel` | Accepted branch state record |
| `Branches` | `system-handler` | Effectful refs + merge-aware advance / conflict persist |
| `Provenance` | `system-handler` | Records `semantic-merge` edges for `cairn why` |
| CLI `repo` | `surface` | `cairn repo branches` / `cairn repo demo` |

Residuals: ledger `SetBranchHead` is still a separate publication path;
change histories on branch refs are passed explicitly at merge time;
grant-bundle threading via `EffectContext.capabilities` is still a placeholder.

## Agreement envelopes (Lean · HVM)

LeanCore and AffineNet/IcNet are **Cairn-native** calculi in those lineages —
not Lean-kernel or HVM-ABI compatible. Boundaries:

- Doc: [agreement.md](agreement.md)
- Types: `cairn.core.Agreement` (`Envelope`, `AgreementCertificate`, `check` /
  `certify`)
- Tests: `AgreementSuite` — always Cairn reference; optional `lean` on PATH;
  classical-IC goldens for nets; `hvm` stubbed (`no-hvm-surface-exporter`)

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
- **Grant-bundle threading** — `EffectContext.capabilities` still empty
- **Per-family restrictive policies** — only PackLoader workspace is narrow;
  ledger / process / LSP still allow-all `bootstrapped()`
- **HVM surface exporter** — agreement uses classical-IC goldens until an
  exporter exists

## Final principle

```text
User defines intent.
Core derives consequences.
System Interface names possible interactions.
Kernel grants authority and validates results.
System Handler touches the world.
Runtime composes User + Handlers.
```
