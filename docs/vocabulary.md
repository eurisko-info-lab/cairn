# Vocabulary (implementation mapping)

How the §2 vocabulary of [CAIRN-PROMPT.md](../CAIRN-PROMPT.md) maps to code.

| Term | Code |
|---|---|
| Fragment | `cairn.kernel.Fragment` — provides/requires/excludes, sorts, constructors, grammar part, rewrite rules, judgments |
| Language | `cairn.kernel.ComposedLanguage` via `Compose.compose`; identity = digest of canonical form (sorted **semantic** fragment digests — no grammar) |
| Sort / constructor | `SortDef` (mode `Tree`/`Graph`), `CtorDef` (with binder positions) |
| Bidirectional grammar | `GrammarSpec` (kernel data) interpreted by `core.Lexer` / `Parser` / `Printer`; laws: `RoundTrip.check` / `fixpoint`; format-preserving edit: `RoundTrip.put` / `Concrete.splice`. Default `print` is derived from `syntax` via `PrintDerive` at compose; explicit `print tag : …` is an override. **Language digests exclude grammar** (Phase 2); concrete syntax lives in `languages/<lang>/surfaces/<style>.cairn` as `surface <style> for <lang> { … }`, bound by `PackLoader.bindSurface`. |
| Surface pack | `runtime` / PackLoader surface binding — named concrete syntax for a language; default style `default`. Meta top form: `surface <style> for <lang> { fragment … }`. Meta itself stays fused (bootstrap fixpoint) but describes the surface top. |
| Judgment / rule | `JudgmentDef`/`InferRule` (checked by `kernel.Checker`), `RewriteRule` (driven by `core.TreeEngine`) |
| Artifact | `cairn.kernel.Artifact` (kind + canonical body) |
| Key / digest | `Digest` (SHA-256 of canonical bytes) + `TypedKey` (valueHash + typeHash + kind) |
| CAS | `systemhandler.MemCas` / `DiskCas` (digest re-verification) via `systeminterface.Cas` |
| Change / ΔL | `core.Delta.deltaOf` — module-level ops + structural path edits; forced recursive `deltaOf` closure; `ValidatedChangeSet` records each gated application |
| Branch / selection | `kernel.BranchManifest` + `systemhandler.Branches` (append-only history) |
| Claim vs proof | `proof.Claim` (proof-free ok) vs `proof.Theorem` (+ checked `Derivation`) |
| Kernel gate | `kernel.Checker.check`, `Delta.apply` validation, `LedgerKernel.applyBlock` |
| VM / runtime IR | `core.TreeEngine` (tree) and `core.NetEngine` (graph) — generic engines only |
| Δ-net / interaction net | `core.NetEngine` + `examples.affinenet.AffineNet` (Fan/Eraser, no replicator) |
| Rosetta | `rosetta.RosettaModule` + `ScalaPort`/`LeanPort` (round-trip-verified emitters) |
| Ledger | `kernel.Tx/Block/LedgerState/LedgerKernel` (pure) + `systemhandler.Node` (I/O) |
| Transcript | `surface.Transcript` — DSL defined in the grammar engine itself |
| Meta-language / grammar-language | `core.Meta` — fused surface (fixpoint achieved; STLC/meta `.cairn` are host-emitted mirrors; see docs/assumptions.md §11). Meta grammar includes both `language …` and `surface … for …` tops. |
| Surface / encoding | named syntax surfaces plus encodings (`text`/`json`/`canon` via Surfaces); ports are projection surfaces |
| Capability bundle | per language: `Present` (CAS/artifact digests), `PlatformProvided` (host mechanisms), or `Deferred` — see `core.Capabilities`; not every language ships every row (§2b) |
