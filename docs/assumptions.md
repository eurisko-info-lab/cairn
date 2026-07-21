# Assumptions (§11.6)

Assumptions taken to keep progress unblocked, stated rather than asked.
Items struck through were assumptions of the MINIMAL build (PLAN.md) that the
maximalization (PLAN-2.md) has since discharged; see STATUS-2.md.

1. **Host = Scala 3.3 LTS, sbt, JDK 17+.** Zero runtime deps beyond the JDK
   (SHA-256/SHA-512 via `MessageDigest`, Ed25519 via `java.security`,
   `com.sun.net.httpserver` for the node API); munit for tests.
2. **Canonical bytes**: bespoke deterministic TLV encoding (big-endian lengths,
   UTF-8 strings, maps sorted by unsigned byte order of keys, duplicate keys
   rejected) rather than CBOR — smaller surface, fully specified in `Canon.scala`.
3. **typeHash** = digest of (kind, recursive structural fingerprint of the body)
   since M1. Values with the same schema share a fingerprint; any shape change
   splits it.
4. ~~Grammar engine subset~~ — discharged by M6: the full vocabulary
   (block/run/adjacent1/restOfLine/anyident/tokfield) is implemented; the
   shipped surfaces use layout combinators where natural (Bend, demo grammars).
5. **Whitespace policy**: the tree-level law `parse(print(t)) == t` plus
   byte-exact concrete printing for UNEDITED files and span-precise splicing /
   `RoundTrip.put` / `Concrete.put` (M7) for format-preserving subtree edits.
   General dirty-subtree re-association is not implemented; `put`/`splice` only
   replace one recorded span. Format-preserving ΔL `remove`/`rename` are still
   absent (rename needs leaf-name spans). Default print rules are derived from
   syntax productions (`PrintDerive`); an explicit `print` line is an override.
   RoundTrip still gates trust — derivation is not trusted alone.
6. **ΔL scope**: module-level ops PLUS structural path edits (M15). Footprints
   are name-reference sets via the language's variable constructor. Change
   composition/inverses/commutation exist (M16); three-way semantic merge
   exists (M17) and `Branches.merge` is merge-aware through
   `SemanticRepository` (conflict → CAS artifact, accept → new head +
   provenance). Everyday branch path persists `ValidatedTip` via `commitTip`
   (opaque tip + replay-checked change-sets; tip sidecar + `.changes` history;
   causal digests on `BranchManifest`) and merges via `mergeBranches`
   (causal LCA by shared module-result digests) / `loadTip`. Accepts are
   journaled (CAS → journal → refs → optional ledger). Ledger `SetBranchHead`
   is opt-in (`Branches.publishHead` or `publish = Some(...)` on merge) —
   accept stays local by default. `Branches` CAS and refs FS are gated
   (`EffectContext.forBranches`). Node / Sync / HttpSync chain files are gated
   (`EffectContext.forLedger`); Sync aborts on authorized CAS failure before
   advancing the chain. CLI / tutorial FS uses `forFilesystem`. CAS
   `contains`, admin, chunking, Unison store, provenance `index`/`why`, and
   Browser board inventory use `CasEffects` / `CasAdminEffects`; Phase0
   MemCas/DiskCas and WaveA M4 algo tests remain intentional trait-contract
   exceptions. Capability nonce/requestId replay uses issuer-scoped
   `ReplayStore` (memory or durable filesystem), shareable across gates.
   Effect interfaces pin as CAS `effect-interface` artifacts
   (`PinnedInterface` / `ActionKey.fromPinned`).
7. **Rename footprint in the MVP transcript** is `[]` because the demo module's
   other definitions do not reference `id`; max.cairn exercises the non-empty
   and failing cases.
8. ~~No side conditions~~ — discharged by M19: `$neq/$fresh/$lt/$le` plus
   injected extension evaluators (PKI uses `$sig-ok`/`$anchor`). The checker
   remains decidable and the sole certifier.
9. ~~Single-authority PoA~~ — discharged by M36: on-chain authority sets,
   majority-quorum add/remove, round-robin sealing. BFT finality still out.
10. **Ports**: Scala runs under scala-cli when present; Haskell (runghc) and
    Rust (cargo) run when their toolchains are present, else assume-skip; Lean
    Rosetta skeletons are golden-checked. All four pass whole-file byte
    fixpoints (M31). Expression bodies inside port files are verbatim
    single-line regions. **Agreement envelopes** (LeanCore ↔ Lean `#check`,
    AffineNet/IcNet ↔ classical IC) are separate from Rosetta — see
    [docs/agreement.md](agreement.md); native tools optional with goldens/stubs.
11. ~~Meta-language staging~~ — discharged by M41/M42: the fused meta surface
    covers grammar productions, print rules, infix tables, rewrite rules, and
    judgments; `languages/meta.cairn` passes the self-description fixpoint
    (Meta can describe/reconstruct itself). The initial seed and STLC/meta
    source-of-truth migration remain host-backed: STLC/meta `.cairn` are
    checked-in canonical mirrors from Scala via `emit-languages`. Exemplar
    packs (PKI/Law/SDS/Search) are `.cairn` source of truth and load at
    runtime. **Surface-file split (Phase 2)** landed for stlc/search/pki/law/sds:
    semantic `languages/<name>.cairn` + `languages/<name>/surfaces/default.cairn`.
    **Phase 3**: Meta top `surface <style> for <lang> { … }` replaces the interim
    `language <style> { … }` hack; remaining fused packs (riemann/minitt/leancore/
    unisoncore) split the same way. Meta itself stays fused (bootstrap fixpoint)
    while describing the surface top.
12. **CLI CAS location**: `$CAIRN_HOME` or `./.cas` (gitignored).
13. **LSP scope** (M44): full-document sync, diagnostics, formatting, rename
    (= ΔL rename emitting a `ValidatedChangeSet`), hover; no incremental
    edits or workspace folders.
14. **Gossip** (M39) is an in-process simulation over real node stores; the
    HTTP surface (M38) is the transport a daemon would use.
