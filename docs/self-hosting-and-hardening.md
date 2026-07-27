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

The report embeds the explicit trusted boundary. The resolved application's
`GenericMachine` artifact selects exactly six trusted mechanisms: canonical
decoder, grammar interpreter, rule/search interpreter, change-program
interpreter, proof checker, and effect dispatcher. SHA-256, Ed25519, and durable
I/O are recorded separately as external assumptions. Studio orchestration,
application resolution, caches, surface renderers, and audit orchestration
remain outside the TCB; their outputs must pass through the selected machine
before acquiring authority.

The embedded `TrustedClosure` is the machine-readable accounting view. It
separates semantic artifact digests, digest-bound host interpreter identities,
native providers, external cryptographic/durability assumptions, and checked
evidence. Evidence is individually classified as independently checked,
replayed, digest-bound, signed, host code, or external-native agreement; these
categories do not imply one another.

Optimization paths carry `SemanticEquivalence` artifacts keyed by semantic
model, implementation, input, and both outcome digests. The dependency cache
is accepted only when its canonical and optimized results agree.
Deleting the cache leaves artifact installation and audit semantics unchanged.

## Generic machine closure

PR28 contracts rather than erases native code. An application manifest now
binds one generic-machine digest. Its transitive dependency graph contains the
bootstrap roots, semantic programs, and effect-family routes; startup fails if
any are absent. A hardening report takes interpreter identities from that
loaded artifact, never from a process-local default. Pack authors may declare a
machine, but production startup only decodes and verifies it. PR29 closes the
remaining naming gap: every selection is an installed
`InterpreterImplementation` with executable, interface, version, explicit
resource bounds, compatibility rules, corpus, checker, and conformance result
artifacts. Missing or disagreeing evidence fails startup. Existing Lean and HVM
certificates remain narrow agreement envelopes.
