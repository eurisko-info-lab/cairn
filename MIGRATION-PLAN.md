# Cairn Migration Plan: Kernel, Core, System and User

## Goal

Refactor Cairn into four conceptual areas with explicit trust, effect and extensibility boundaries:

```text
Kernel
Core
System
  ├── Interface
  └── Handler
User
```

The architecture follows five operational roles:

```text
Kernel            validates
Core              computes and proposes
System Interface  describes effects and resources
System Handler    performs effects
User              defines languages, policies and workflows
```

The refactor must preserve current behavior while making all trusted decisions, effectful interactions and language-defined components visibly separate.

## 1. Current state

The current sbt projects are:

```text
kernel
workbench
proof
compute
rosetta
ledger
surface
examples
tests
```

Their dependency chain is largely linear, with `examples` able to use the entire stack.

Several files already reveal the future seams. For example, `workbench/Cas.scala` currently contains:

* the abstract `Cas` interface;
* the mutable `MemCas` implementation;
* the filesystem-backed `DiskCas`;
* the pure `BranchManifest` artifact;
* the filesystem-backed `Branches` implementation.

It therefore combines System Interface, System Handler and repository-domain structures in one file.

The constitution already establishes that the semantic repository is intrinsic Cairn substrate and that hosted languages inherit CAS, changes, branches and provenance rather than implementing their own repositories.

## 2. Target architecture

| Area                 | Responsibility                                                                     | Trust status                                     |
| -------------------- | ---------------------------------------------------------------------------------- | ------------------------------------------------ |
| **Kernel**           | Validate artifacts, languages, proofs, changes, authority and accepted transitions | Semantic TCB                                     |
| **Core**             | Parse, derive, elaborate, search, evaluate, project, merge and propose             | Pure but not automatically trusted               |
| **System Interface** | Define effect operations, resources, requests, responses and capabilities          | Pure platform contract                           |
| **System Handler**   | Perform filesystem, process, crypto, network, persistence and UI effects           | Operationally privileged, semantically untrusted |
| **User**             | Define languages, policies, programs, proofs, changes, projections and workflows   | Extensible data                                  |

Purity is not sufficient to place Core outside the trusted base. Every Core result used for acceptance must have an independent Kernel validation path.

Examples:

```text
Core derives ΔL
Kernel checks the derived language

Core elaborates a program
Kernel checks the elaborated artifact

Core proposes a merge
Kernel validates the merge witness

Core evaluates a policy
Kernel checks the authorization derivation

Core executes a term
Kernel checks the reduction trace
```

## 3. Dependency graph

The proposed linear graph:

```text
Kernel ← Core ← System ← User
```

must not be used. It would force User code to depend on concrete System implementations.

The module graph should instead be:

```text
kernel

core
  → kernel

system-interface
  → kernel

user
  → kernel
  → core
  → system-interface

system-handler
  → kernel
  → core
  → system-interface

runtime
  → user
  → system-handler
```

The key prohibition is:

```text
user ↛ system-handler
```

A user language may construct `Filesystem.Read` or `Crypto.Sign` requests, but it must never import `java.nio.file`, socket APIs, process runners or private-key implementations.

Although Cairn has four conceptual areas, the System split and composition root naturally produce at least six sbt projects:

```text
kernel
core
system-interface
system-handler
user
runtime
```

## 4. Authority architecture

### 4.1 Effect interfaces

System Interface declares operations and their resource schemas:

```text
Filesystem.Read(Path)
Filesystem.Write(Path)
Process.Run(Backend)
Crypto.Sign(KeyHandle, Digest)
Network.Connect(Endpoint)
Ledger.Publish(Namespace)
Language.Change(LanguageKey, SemanticFootprint)
Branch.Advance(BranchKey)
```

Effect declarations generate the valid action vocabulary
(`Effects.ActionKey` derived from `EffectMeta` action decls +
`ResourceSchema`). Policies cannot reference nonexistent operations or
malformed resources. Closed `Effects.Action` is a host bridge during
migration.

### 4.2 Policies

Policies are User artifacts:

```text
Policy {
  subject
  action
  resource
  conditions
  Allow | Deny
}
```

The initial policy semantics should remain deliberately small:

* exact and typed action matching;
* typed resource scopes;
* explicit conditions;
* deny overrides allow;
* no implicit ambient authority;
* deterministic evaluation.

Full AWS IAM wildcard and condition complexity should not be copied wholesale.

### 4.3 Capability grants

Core evaluates policies and proposes an attenuated `CapabilityGrant`.

The Kernel validates:

* policy identity;
* subject identity;
* action and resource compatibility;
* policy derivation;
* attenuation from broader authority;
* expiry and nonce constraints;
* delegation depth;
* request arguments;
* overriding denials.

Capability minimization is a Core optimization. The Kernel must validate that the grant is no broader than its authority source, but need not prove that no narrower equivalent grant exists.

### 4.4 Authorized requests

The flow is:

```text
Policy + Subject + EffectRequest
              ↓
Core produces AuthorizationDerivation
and proposed CapabilityGrant
              ↓
Kernel checks the derivation and grant
              ↓
Kernel constructs AuthorizedRequest
              ↓
System Handler performs the operation
              ↓
Kernel validates the response and evidence
```

`AuthorizedRequest` must not be a publicly constructible case class. It should use an opaque type, private constructor or equivalent Kernel-controlled token.

For remote execution, the receiving side must revalidate the signed capability and replay constraints. Scala type safety alone is not an authority boundary.

## 5. Placement of Meta

Meta spans three areas.

### Kernel Meta

Contains:

* primordial Meta representation;
* canonical Meta decoding;
* language well-formedness;
* binding and rule validation;
* composition-result validation;
* language identity;
* authority-model primitives required for acceptance.

### Core Meta

Contains:

* Meta elaboration;
* grammar interpretation;
* parser and printer derivation;
* fragment composition algorithm;
* ΔL derivation;
* Mogensen-style interpreter derivation;
* bootstrap fixpoint computation;
* rule compilation.

### System Meta

Contains:

* loading Meta artifacts;
* language discovery;
* persistent registries;
* dependency fetching;
* activation and caching;
* network distribution;
* compiled-engine caching.

The System reads bytes, Core parses and derives, and Kernel validates.

## 6. Migration phases

## Phase 0: Freeze and characterize

Before moving code:

1. Record the current head and generated-language digests.
2. Run the complete test, transcript, language-sync and fuzz suites.
3. Add characterization tests around every cross-module behavior.
4. Add `docs/architecture.md` with the target boundaries (current source of
   truth; chronological notes later split to `docs/migration-history.md`).
5. Add forbidden-import checks.

Examples of forbidden imports:

```text
kernel must not import java.nio, networking or process APIs
core must not import filesystem or networking APIs
user must not import system-handler packages
system-handler must not contain language-specific SDS or Unison logic
```

Do not create empty modules and immediately move everything. First make the intended boundaries executable as tests.

## Phase 1: Split System Interface from System Handler

Use CAS as the first vertical migration because it already combines the relevant concerns.

Move:

```text
Cas
CAS request and response types
artifact-fetch effect declarations
```

to `system-interface`.

Move:

```text
MemCas
DiskCas
filesystem-backed branch references
```

to `system-handler`.

Move or retain:

```text
BranchManifest
repository transition structures
canonical branch representation
```

in Kernel, provided their validity affects accepted repository state.

This first slice establishes the pattern without disturbing every subsystem at once.

Acceptance:

* existing CAS and branch tests remain green;
* user code sees only the CAS interface;
* disk access exists only in handlers;
* all loaded bytes have their digest and typed identity rechecked.

## Phase 2: Introduce Core

Create `core/` and migrate pure proposal machinery.

**Progress (phase narrative in `docs/migration-history.md`; current Core
surface in `docs/architecture.md`):** slices 1–10 landed — Search/Tactics,
TreeEngine, PackCompose, Filesystem extraction, Grammar, Meta/Surfaces,
Delta/ModuleSurface/Capabilities+Query, ChangeAlgebra/Merge/Migrate, the
Rosetta port-generation engine, and the optional `Scaffold.plan` revisit
(`core.ScaffoldPlan` with relative-path purity; `rosetta.Scaffold` remains the
thin I/O façade). **Phase 2 Core introduction is complete.** `workbench` holds
only `PackLoader`; `rosetta` holds only `Scaffold` emit over Filesystem.

**Post-Phase-2 summit (semantic repository):** `core.SemanticRepository`
composes Delta/ChangeAlgebra/Merge/Migrate into one operational story;
`system-handler.Branches` is merge-aware (`merge` / `commitModule` /
`headModule`). See `docs/architecture.md` § Semantic repository spine.

Likely destinations:

### From `workbench`

```text
grammar interpretation
parser and printer
format preservation
composition construction
Delta generation and composition
Meta elaboration
bootstrap computation
```

Validation counterparts remain in Kernel.

### From `proof`

```text
proof search
tactics
goal construction
derivation discovery
```

The independent derivation checker remains in Kernel.

### From `compute`

```text
TreeEngine
net reduction drivers
normalization strategies
readback
rule dispatch compilation
```

Reduction-step and trace validation remain in Kernel.

### From `rosetta`

```text
generic projection engine
obligation generation
target artifact construction
```

Target-language mappings belong in User. Native compiler invocation belongs in System Handler.

Acceptance:

* Core depends only on Kernel;
* Core contains no I/O imports;
* every acceptance-relevant Core output has a Kernel checker;
* current evaluation, proof and projection tests remain green.

## Phase 3: Establish the complete System split

**Status: Done.** Effect families land as request/response types in
`system-interface` and handlers in `system-handler`:

```text
filesystem, cas, workspace, process, crypto, clock, random,
network, http, ledger transport, terminal, lsp, external backend
```

CAS was Phase 1. Filesystem expanded beyond write-only. Workspace absorbs
`PackFiles`. Crypto/ledger node/HTTP/gossip/provenance moved out of
`ledger/` into `system-handler` (ledger keeps compatibility re-exports).
LSP framing → `LspTransport`. Process + ExternalBackend cover host toolchains.
Kernel `Effects` vocabulary names typed actions for authority (Phase 4+).

## Phase 4: Introduce authority in audit mode

**Status: Done.** Kernel `Authority` models (`EffectRequest`, `EffectPolicy`,
`CapabilityGrant`, `AuthorizationDerivation`, opaque `AuthorizedRequest`,
`AuthorityEvent`). Core `PolicyEval` proposes; Kernel `Authority.validate`
checks. `AuthorityGate` defaults to **Audit** (never blocks; records
would-permit). `AuthoritySuite` covers the vertical.

## Phase 5: Enforce authority family by family

**Status: Done (skeleton).** `AuthorityGate.Mode.Enforce` rejects without
matching allow. `Node.append` checks `LedgerAppend` through the gate (audit
by default; enforce when mode set). Remaining families can opt into the same
`AuthorityGate.check` call — full per-family rollout of mandatory tokens on
every handler entry is incremental; the gate and ledger-append hook establish
the pattern. BFT / gossip daemon remain deferred forever.

## Phase 6: Establish the User boundary

**Status: Done.** New `user/` module (`→ kernel, core, system-interface`).
Language packs moved: STLC, Law, SDS, MiniTT, LeanCore, UnisonCore,
AffineNet, IcNet, Bend, QuickSort mappings, PolicyLang. User loads packs via
`PackAccess` (never `system-handler`). Host-glue demos that need crypto/CAS
I/O (PKI signing, SDS sealing tutorials, Search provenance, Riemann emit,
Claims/proof orchestration) remain under `examples/` which may use the full
stack. `ModuleBoundarySuite` enforces `user ↛ system-handler`.

## Phase 7: Complete Meta and bootstrap migration

**Status: Done (thin split).** Meta elaboration stays in `core.Meta` (already
pure). Kernel gains `MetaValidate` (fixpoint digest, composability, fragment
shapes). System Handler gains `MetaActivation` (load bytes → Core parse →
Kernel check). Separate `grammar.cairn` remains deferred (bootstrap.md).
Pinned Meta artifact bootstrap via `languages/meta.cairn` unchanged.

## Phase 8: Compatibility facades and cleanup

**Status: Done.** New `runtime/` holds `PackLoader` (composition root).
Facades re-export relocated APIs:

```text
workbench → runtime.PackLoader
compute   → core.NetEngine/NetBuilder
ledger    → system-handler crypto/node/distribution; user PolicyLang
examples  → user language packs (where moved)
rosetta   → Scaffold emit façade (engine already in core)
```

`docs/architecture.md` holds the current boundaries; phase archaeology lives in
`docs/migration-history.md`. Module DAG matches the diamond + runtime root.

**AuthorityGate / PackAccess injection — DONE.** No ambient
`PackAccess.get`/`install` or `AuthorityGate.default`/`forFamily`. Composition
roots (`Main`, tests) construct `EffectContext` / gates and
`PackLoader(ctx)`, pass them into handlers/`Node`/`Lsp`/`Browser`/`Cli`, and
construct language packs as `Law(packs)` / `Pki(packs)` / … classes.

**EffectContext + AuthorizedEffect — DONE.** Composition roots mint
`EffectContext(subject, gate, capabilities = Nil, audit)` via
`EffectContext.forPackLoader()` (PackLoader), `.bootstrapped()` (other
families / tests), or `.local(gate)`. Authorization is a single
entry point: `ctx.authorize(req) → AuthorizedEffect` (wraps Kernel
`AuthorizedRequest`). Handlers `perform` only an `AuthorizedEffect` (plus
the interface request payload); thin `run(req, ctx)` adapters authorize then
perform for composition roots. Handler-internal `Subject("local")` and
`gate.checked` removed. Ledger `Node.append` authorizes as
`Subject(authority.name)` (seal identity). `capabilities` remains a
placeholder pending grant-bundle threading.

**Narrow PackLoader policy — DONE.** First restrictive deployment path:
`PolicyEval.packLoaderWorkspace` allows only workspace `read` (derived
`ActionKey`) for a single subject under `languages*`. `examples.Main` wires
PackLoader with `EffectContext.forPackLoader()` (not allow-all).
Ledger/process/LSP remain on `bootstrapped()` until scoped similarly.

**Derived ActionKey + resource schemas (priority #4) — DONE.** Effect-interface
artifacts (`EffectMeta.EffectFamily`) declare capability-class names and a
`ResourceSchema(kind, pathPattern)`. Policies and `EffectRequest`s use
`Effects.ActionKey`; handlers derive intents via `keyFor` / `resource.at`.
Closed `Effects.Action` remains a host bridge (`toHost` / `.key`) for
Cas/Ledger and completeness checks — not the policy-boundary type.

**Authority calculus (priority #5) — DONE.** Conditions evaluated fail-closed
and canonicalized; grant expiry + nonce in canon; gate replay sets for nonce /
`requestId`; delegation chains validated; Kernel `AttenuationWitness` for
non-widening attenuation/delegation. Injectable `EffectContext.clock` for
expiry tests.

**Core-generated authorization proofs (priority #6) — DONE.** Core
`PolicyEval.prove` constructs structured `AuthorizationProof` witnesses;
Kernel `Authority.checkProof` validates them (cited allows in store + match,
no matching Deny, condition evidence, grant justification, optional
attenuation/delegation) without accepting via re-run of `decide`.
`AuthorityGate` Enforce: prove → checkProof → mint `AuthorizedRequest`.
Residual: `EffectContext.capabilities` grant-bundle threading still empty.

## 7. Concrete old-to-new mapping

| Current module                            | Target                   |
| ----------------------------------------- | ------------------------ |
| `kernel` artifact and checker types       | Kernel                   |
| `workbench.Meta` validation               | Kernel                   |
| `workbench.Meta` elaboration and printing | Core                     |
| `workbench.Parser` / grammar interpreter  | Core                     |
| `workbench.Cas` trait                     | System Interface         |
| `MemCas` / `DiskCas`                      | System Handler           |
| `BranchManifest` validity                 | Kernel                   |
| filesystem-backed `Branches`              | System Handler           |
| proof checker                             | Kernel                   |
| proof search and tactics                  | Core                     |
| tree and net execution                    | Core                     |
| reduction trace checker                   | Kernel                   |
| Rosetta projection machinery              | Core                     |
| Rosetta mapping definitions               | User                     |
| native compiler invocation                | System Handler           |
| ledger state transition                   | Kernel                   |
| ledger networking and persistence         | System Handler           |
| CLI, HTTP and LSP transports              | System Handler / Runtime |
| command and workflow languages            | User                     |
| exemplar and standard languages           | User                     |

## 8. Revised success criteria

The migration is complete when:

1. Kernel has no I/O, process, network, clock, randomness or mutable persistence operations.
2. Core has no effects and no acceptance result that bypasses Kernel validation.
3. User imports System Interface but never System Handler.
4. All concrete interactions occur through handlers.
5. Effect interfaces generate typed action and resource vocabularies.
6. Policies, grants and requests are canonical Cairn artifacts.
7. Handlers cannot execute privileged operations without Kernel-authorized requests.
8. CAS results, signatures, traces and external-backend results are checked before acceptance.
9. Existing language, proof, computation, repository, ledger and transcript behavior remains intact.
10. At least one end-to-end workflow uses real enforced capabilities.
11. Test, memory, replay and local handlers execute the same User workflow.
12. The old `workbench`, `proof`, `compute`, `ledger` and `surface` boundaries are removed or retained only as documented compatibility facades.

## 9. Important changes to the original proposal

The following statements should be replaced:

```text
Strict DAG: Kernel ← Core ← System ← User
```

with the diamond-shaped module graph and runtime composition root.

Replace:

```text
Core trust level: High
```

with:

```text
Core is pure but semantically untrusted unless its result has no
independent Kernel checker.
```

Replace:

```text
System receives effect requests
```

with:

```text
System Handler receives only Kernel-authorized requests.
```

Replace:

```text
No behavior change for end users
```

with:

```text
No semantic regression, except that previously ambient or unauthorized
effects may now be explicitly rejected.
```

Remove the fixed calendar estimate. The scope changes dramatically depending on whether the first authority pass is a native typed skeleton or a fully Meta-defined, delegated, replay-safe policy system.

## Final principle

```text
User defines intent.
Core derives consequences.
System Interface names possible interactions.
Kernel grants authority and validates results.
System Handler touches the world.
```
