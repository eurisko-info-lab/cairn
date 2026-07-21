# Vocabulary (implementation mapping)

How the §2 vocabulary of [CAIRN-PROMPT.md](../CAIRN-PROMPT.md) maps to code.

| Term | Code |
|---|---|
| Fragment | `cairn.kernel.Fragment` — provides/requires/excludes, sorts, constructors, grammar part, rewrite rules, judgments |
| Language | `cairn.kernel.ComposedLanguage` via `Compose.compose`; identity = digest of canonical form (sorted fragment digests) |
| Sort / constructor | `SortDef` (mode `Tree`/`Graph`), `CtorDef` (with binder positions) |
| Bidirectional grammar | `GrammarSpec` (kernel data) interpreted by `workbench.Lexer/Parser/Printer`; laws: `RoundTrip.check` / `fixpoint`; format-preserving edit: `RoundTrip.put` / `Concrete.splice`. Default `print` is derived from `syntax` via `PrintDerive` at compose; explicit `print tag : …` is an override. Language digests still include grammar (surface split = Phase 2). |
| Judgment / rule | `JudgmentDef`/`InferRule` (checked by `proof.Checker`), `RewriteRule` (driven by `compute.TreeEngine`) |
| Artifact | `cairn.kernel.Artifact` (kind + canonical body) |
| Key / digest | `Digest` (SHA-256 of canonical bytes) + `TypedKey` (valueHash + typeHash + kind) |
| CAS | `workbench.Cas` (`MemCas`, `DiskCas` with digest re-verification) |
| Change / ΔL | `workbench.Delta.deltaOf` — module-level ops + structural path edits; forced recursive `deltaOf` closure; `ValidatedChangeSet` records each gated application |
| Branch / selection | `workbench.BranchManifest` + `Branches` (append-only history) |
| Claim vs proof | `proof.Claim` (proof-free ok) vs `proof.Theorem` (+ checked `Derivation`) |
| Kernel gate | `proof.Checker.check`, `Delta.apply` validation, `LedgerKernel.applyBlock` |
| VM / runtime IR | `compute.TreeEngine` (tree) and `compute.NetEngine` (graph) — generic engines only |
| Δ-net / interaction net | `compute.NetEngine` + `examples.affinenet.AffineNet` (Fan/Eraser, no replicator) |
| Rosetta | `rosetta.RosettaModule` + `ScalaPort`/`LeanPort` (round-trip-verified emitters) |
| Ledger | `kernel.Tx/Block/LedgerState/LedgerKernel` (pure) + `ledger.Node` (I/O) |
| Transcript | `surface.Transcript` — DSL defined in the grammar engine itself |
| Meta-language / grammar-language | `workbench.Meta` — fused surface (fixpoint achieved; STLC/meta `.cairn` are host-emitted mirrors; see docs/assumptions.md §11) |
| Surface / encoding | every grammar is a surface artifact; ports and canonical bytes are non-text encodings of the same artifacts |
| Capability bundle | per language: `Present` (CAS/artifact digests), `PlatformProvided` (host mechanisms), or `Deferred` — see `workbench.Capabilities`; not every language ships every row (§2b) |
