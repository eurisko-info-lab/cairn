# Self-hosting and hardening

Cairn's self-development boundary is `SelfHosting.propose`. It accepts a
`LanguageStudioProject` and ordinary language-asset edits, delegates them to
Language Studio, and retains the resulting ΔMeta and ΔGrammar replay witnesses.
There is no source or module mutation path in this API. An edit crossing both
partitions succeeds only when both reconstructed modules equal the proposed
project and the complete capability graph validates.

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
