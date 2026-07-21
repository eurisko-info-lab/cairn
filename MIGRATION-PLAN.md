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

Effect declarations generate the valid action vocabulary. Policies cannot reference nonexistent operations or malformed resources.

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
4. Add `docs/architecture.md` with the target boundaries.
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

**Progress (ground truth in `docs/architecture.md`):** slices 1–7 landed —
Search/Tactics, TreeEngine, PackCompose, Filesystem extraction, Grammar, Meta/
Surfaces, and Delta/ModuleSurface/Capabilities+Query. Remaining Phase 2
candidates from the lists below: `ChangeAlgebra`/`Merge`/`Migrate` (workbench),
rosetta port-generation engine, optional Scaffold.plan revisit.

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

Create standard effect families:

```text
filesystem
cas
workspace
process
crypto
clock
random
network
ledger transport
terminal
http
lsp
external backend
```

For each family, provide:

```text
Interface
  operations
  resource schemas
  request types
  response types
  error types

Handler
  local implementation
  memory/test implementation
  optional replay implementation
```

Migrate one family at a time:

1. filesystem and CAS;
2. process execution;
3. crypto and keys;
4. network and HTTP;
5. ledger persistence and transport;
6. clock, randomness and UI.

The pure ledger transition relation remains in Kernel. Gossip, persistence, signing-key access and transport move to handlers.

## Phase 4: Introduce authority in audit mode

Add Kernel data models for:

```text
EffectRequest
EffectResponse
Policy
CapabilityGrant
AuthorizationDerivation
AuthorizedRequest
AuthorityEvent
```

Add Core policy evaluation and capability attenuation.

Initially, run authorization in audit mode:

```text
existing operation executes
authority engine computes whether it would be permitted
test asserts expected decision
```

This exposes missing resource identities and accidental ambient authority before enforcement starts.

The first vertical transcript should cover:

```text
policy
→ capability derivation
→ Kernel authorization
→ file read
→ artifact validation
→ ledger publication
→ authority audit event
```

SDS publication is a suitable first workflow because it already combines files, CAS, PKI, policy and ledger publication.

## Phase 5: Enforce authority family by family

Replace audit mode with mandatory authorization in this order:

1. ledger publication;
2. branch advancement and semantic changes;
3. crypto signing and decryption;
4. process and external backend execution;
5. filesystem writes;
6. network publication;
7. filesystem reads where sandboxing requires it.

Handlers accept only `AuthorizedRequest`.

Add replay protection for one-shot capabilities and explicit capability propagation through nested workflows.

## Phase 6: Establish the User boundary

Move into `user/`:

```text
Meta-defined standard languages
STLC
Unison Core
MiniTT
AffineNet and IcNet language definitions
Bend
PKI
Law
SDS
Search
Rosetta target mappings
policies
workflows
transcripts
examples
```

User contains Cairn’s first-party standard library as well as third-party languages. “User” means replaceable and language-defined, not unimportant.

Language definitions must use System Interface effects without importing handlers.

## Phase 7: Complete Meta and bootstrap migration

Only after Kernel, Core and System boundaries are stable:

1. split current Meta implementation into trusted validation, pure derivation and effectful activation;
2. bootstrap the pinned Meta artifact;
3. derive or load the effect-interface vocabulary through Meta;
4. Kernel-check the bootstrap fixpoint;
5. activate accepted language and effect interfaces through System handlers.

This ordering avoids simultaneously changing the bootstrap language, module graph, effect model and authority model.

## Phase 8: Compatibility facades and cleanup

During migration, preserve temporary facade modules:

```text
workbench
proof
compute
ledger
surface
```

They may re-export the new APIs so existing examples continue compiling.

Once all imports have migrated:

* remove deprecated facades;
* enforce module-boundary tests;
* update package names;
* regenerate language artifacts;
* update architecture, bootstrap and exemplar documentation;
* update the capability manifest;
* run complete parity and fuzz suites.

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
