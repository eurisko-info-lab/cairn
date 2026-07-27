# Self-hosting and hardening

Cairn's self-development boundary is `SelfHosting.propose`. It accepts a
`LanguageStudioProject` and ordinary language-asset edits, delegates them to
Language Studio, and retains the resulting ΔMeta and ΔGrammar replay witnesses.
There is no source or module mutation path in this API. An edit crossing both
partitions succeeds only when both reconstructed modules equal the proposed
project and the complete capability graph validates.

`SelfHostingCeremony.run` closes the release loop from a signed ecosystem
bundle digest. It recursively installs that bundle into an empty CAS,
reconstructs the Language Studio project and its Meta/Grammar interpreters
from the application manifest, applies ordinary asset edits, builds and signs
a successor manifest, publishes it to the ledger, and recursively installs it
into a second empty CAS. The second node independently resolves the same graph,
reopens Language Studio, emits a continuation change, and produces a byte-equal
audit. Callers supply no language map, capability list, migration list, Studio
profile, or application entry-point map.

Application hardening begins from the same single root digest used at startup.
`ApplicationHardeningAuditor` resolves the complete application, including its
languages and capability bundles, walks every discovered dependency, and emits
a canonical `audit-report` artifact. A partial graph, wrong artifact kind,
language reconstruction mismatch, or invalid capability selection fails closed
and produces no report.

Dependency discovery is a performance-sensitive path. Its cache is keyed only
by artifact digest and memoizes the result of the pure canonical dependency
decoder. It neither supplies dependencies nor bypasses validation, and deleting
it changes performance but not results.

The report embeds the explicit trusted boundary. Canonical encoding, SHA-256
identity, language checking, change replay, validation checking, and ledger
transition are trusted mechanisms. Studio orchestration, application assembly,
caches, surface renderers, and audit orchestration remain outside the TCB; their
outputs must pass through the trusted mechanisms before acquiring authority.

The embedded `TrustedClosure` is the machine-readable accounting view. It
separates semantic artifact digests, digest-bound host interpreter identities,
native providers, external cryptographic/durability assumptions, and checked
evidence. Evidence is individually classified as independently checked,
replayed, digest-bound, signed, host code, or external-native agreement; these
categories do not imply one another.

Optimization paths carry `OptimizationEquivalence` traces keyed by semantic
model digest, interpreter-version digest, and input digest. The dependency
cache is accepted only when its canonical and optimized result digests agree.
Deleting the cache leaves artifact installation and audit semantics unchanged.

## Remaining contraction work

PR25 makes the boundary explicit; it does not claim to erase native code.
[PR28](../ROADMAP.md#pr28--contract-the-remaining-host-tcb) targets the
remaining bootstrap seeds, host interpreters, and effect routing until the host
is a small generic decoder/interpreter/checker/dispatcher machine.

`OptimizationEquivalence` currently proves only dependency-discovery cache
agreement. [PR29](../ROADMAP.md#pr29--generalized-semantic-equivalence-evidence)
extends the same digest-bound law to parsing, printing, evaluation, change
replay, merge, migration, and native surface providers. Existing Lean and HVM
certificates remain narrow agreement envelopes throughout that work.
