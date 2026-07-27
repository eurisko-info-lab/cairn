# Core concepts

This is the conceptual map behind Cairn's APIs and documentation.

## Artifact

An artifact is a typed envelope around canonical bytes. Its SHA-256 digest is
its storage identity; its kind and structural type fingerprint prevent an
untyped blob from silently standing in for another schema.

Artifacts are immutable. Mutable names—branch names, workspace aliases, peer
registries—are pointers to artifact digests, not alternate identities.

## Language and grammar

A language is composed from fragments that declare sorts, constructors,
binding, rules, judgments, requirements, and providers. A grammar supplies its
surface syntax and printer. Composition is checked and produces one digest-bound
`ComposedLanguage`.

Meta is itself a Cairn language. Grammar is a separate language associated with
it. This allows Language Studio to evolve semantic assets through ΔMeta and
grammar assets through ΔGrammar.

## Language capabilities

A `LanguageCapabilities` artifact selects the runtime behavior associated with
one exact language revision:

```text
language
change semantics + change surface
validation model
migrations
queries and policies
foreign projections
Studio semantic and presentation profiles
```

This bundle replaces host-supplied clouds of models and callbacks. A capability
whose digest or target language does not match fails during resolution.

## ΔL: changes are language terms

Every language L has a free change language ΔL. Changes add, replace, remove,
rename, or edit semantic paths; a pack may declare additional operations as
data. The generic interpreter derives behavior from fixed query, boolean,
mutation, access, and inverse primitives rather than operation-name switches.

Applying a change produces a new module and a replayable
`ValidatedChangeSet`. Changes can themselves be changed, giving Δ(ΔL) and the
recursive closure required by Cairn's constitution.

## Semantic paths and access traces

Paths use persistent field IDs and keyed-element identities. Numeric positions
are traversal hints, not semantic identity. Read/write traces distinguish whole
definitions, bindings, and subtrees, making sibling edits mergeable while
parent/child or read/write overlaps conflict.

Both execution orders are still witnessed before a merge is accepted. Access
analysis improves precision; it never replaces semantic execution.

## Studio and workspace

Studio is a generated interaction surface over a language bundle. Grammar data
provides a valid fallback form; optional pack-declared profiles add domain views,
widgets, commands, and workflows. Profile commands elaborate into ΔL templates,
not host callbacks.

A workspace stages proposals. Durable workspaces are content-addressed graphs
of drafts, signed reviews, approvals, handoffs, and rebases. They survive
restart and replication. Studio never saves a mutated module directly.

## Validation and acceptance

A `ValidationModel` determines model/domain checks. An
`AcceptanceConstitution` unifies validation, domain ancestry, certificates,
authority, migration, and publication rules. Acceptance produces a complete
evidence artifact and an accepted tip; orchestration code cannot bypass it with
a convenience save path.

## Branch, conflict, and migration

A branch records accepted semantic state and causal history. A conflict is a
first-class artifact naming overlapping semantic locations and witnesses.
Conflict resolution is an ordinary ΔConflict program that can accept, replace,
compose, defer, or split locations while retaining unresolved state explicitly.

Migrations are pack-declared, decodable artifacts. They transport modules,
changes, paths, capability bundles, validation references, pending work, and
stored conflicts between declared language revisions.

## Proof, projection, and agreement

These are deliberately different:

| Evidence | Establishes |
|---|---|
| Checked derivation/proof term | A kernel checker accepted the proof under a specified calculus |
| Test certificate | A declared test path passed for the cited subject |
| Projection evidence | Particular bytes were deterministically projected from an artifact, with obligations retained |
| Agreement evidence | Cairn and an identified external/native model agreed for the stated case |
| Signature | The holder of a key signed exact bytes |
| Digest | Exact byte identity |

No row implies all the others.

## Application and ecosystem release

An `ApplicationManifest` is the sole startup root for an application. It names
languages, grammars, capability bundles, typed entries, and dependencies by
digest. Installation discovers and verifies the complete graph recursively.

An ecosystem release signs an application or pack root, semantic version,
migrations, predecessors, namespace, and publisher identity. Trust policy
governs discovery; ledger publication anchors the exact signed bundle.

## The three histories

- **Release history** evolves the software and semantic definition available
  to install.
- **Branch history** evolves accepted domain state under a constitution.
- **Workspace history** evolves proposals and their review before acceptance.

Keeping them distinct prevents “available software,” “accepted data,” and
“work under consideration” from collapsing into one misleading timeline.

## Trusted computing boundary

Cairn trusts a small generic substrate: canonical encoding, hashing and
signature verification, generic parsing/printing and rule interpretation,
change replay, proof/validation checking, ledger transition, and effect
execution. Domain meaning and application assembly belong in artifacts.

Hardening audits emit a `TrustedClosure` that distinguishes semantic artifacts,
host interpreter identities, native providers, external assumptions, and the
reason each evidence artifact is trusted. Optimizations are digest-keyed,
replaceable caches whose result must agree with canonical interpretation.
