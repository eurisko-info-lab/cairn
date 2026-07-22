# STATUS-2 — end of the maximalization plan (PLAN-2)

Date: 2026-07-22. All 50 stories M1–M50 of [PLAN-2.md](PLAN-2.md) landed on top of
the S1–S50 base, plus a **top-level parity pass** (constitution §4.16), an
**exemplar elevation** (PKI/Law/SDS as `.cairn` languages with a real
`PKI → Law → SDS` `requires`/`provides` DAG), and a **trust-hardening pass**
(Sync abort, delegation root justification, opaque `ValidatedTip` /
`ValidatedChangeSet`, `VerifiedCapability`, BranchManifest causal digests,
agreement envelope digests) and a **deferred-trust follow-on** (issuer-scoped
`ReplayStore`, journaled transactional accept, CAS-pinned effect interfaces,
causal-LCA merge) plus **reclaim / sync** (`reclaimOrphanBlobs`, CAS
`replay-snapshot` merge) and an **HVM surface exporter** (`HvmSurface` HVM2
books + live `hvm` when on PATH), plus a **Lean agreement expansion**
(`natRec` ι corpus + live `#check` stdout digests), plus an **SDS
phrase-staleness stub** (`corpusPhrase` + `PhraseStaleness.restale`), an
**SDS regulatory section-numbering** path (`SectionNumbering` + versioned
`eu-clp` pack + `sectionNumberOk` judgment), **chemical `.cairn` instance
sources** (`languages/sds/chemicals/` acetone/ethanol; host maps emit/load),
an **SDS section-report surface pack** (`sds-report`), **SDS language section
maps** (`euSection` / `outline` / `sectionField` / `sectionFieldRef` in
`sds.cairn`), **multilingual section fields** (corpus refs + free-text +
shadow overrides), **BranchManifest.changeHistory** (sidecars as caches), and
an **SDS causal workflow** (`SdsCausalWorkflow`: author → shadow → rebase →
conflict → approve → sign → publish), **typed identification/hazards sections**,
and an **sds-report `json` surface**. Full suite: **491 tests green** (+2 skipped;
`tests` module; `sbt test`),

including a 100 000-term fuzz corpus with zero round-trip failures,
`ParitySuite`, and `ExemplarPackSuite`.

## Post-parity priority scorecard (2026-07-22)

| # | Priority | Status | Evidence |
|---|---|---|---|
| 1 | Chemical instances as `.cairn` sources | **Partial→advanced** | `languages/sds/chemicals/{acetone,ethanol}{,-thin}.cairn`; `ChemicalSource`; host maps remain emit fixtures |
| 2 | Versioned regulatory-profile languages (EU REACH/CLP) | **Partial** | `languages/eu-clp.cairn` + `languages/sds/profiles/eu-clp-annex-ii.cairn` (v1); `EuClp` / `sectionNumberOk` |
| 3 | Typed section-specific structures | **Partial** | `identificationSection` / `hazardsSection` (+ `fieldLocale`); thin acetone/ethanol s1/s2 typed; sections 3–16 remain `euSection` maps |
| 4 | Section-numbering / phrase-staleness as judgments or ΔL | **Partial** | `sectionNumberOk` judgment; phrase/section-field staleness remain projected restale over EN-hash drift |
| 5 | Multilingual section fields: corpus refs + overrides | **Partial** | `sectionFieldRef` + `sectionFieldShadow`; thin FR overlays; typed locales via `fieldLocale` |
| 6 | Report formats as surface packs | **Partial→advanced** | `sds-report` `default` text + `json` (unquoted-key JSON-ish); XML/PDF/XLS not started |
| 7 | Branch manifests alone reach semantic history | **Partial→advanced** | `BranchManifest.changeHistory`; `loadChangeHistory` prefers manifest; sidecars write-through caches |
| 8 | VerifiedCapability / issuer evidence at trust boundaries | **Partial** | `SdsCausalWorkflow` mints `VerifiedCapability` + Ed25519 tip-signature certificate on publish path |
| 9 | Widen Lean/HVM only via explicit corpus claims | **Done (process)** | No new claim this pass; envelopes unchanged |
| 10 | Complete SDS workflow through causal repo | **Partial→advanced** | `SdsCausalWorkflow` + ParitySuite: author/shadow/rebase/conflict/approve/sign/publish |

## Story scorecard

| Wave | Stories | Status | Evidence |
|---|---|---|---|
| A identity/CAS | M1–M5 | ✅ | `WaveASuite` — structural fingerprints, α-invariant digests, fsck/GC/stats, sha256+sha512 agility with validated migrations, 100 MiB chunked streaming with dedup |
| B grammar engine | M6–M12 | ✅ | `WaveBSuite` — block/run/adjacent1/restOfLine, byte-exact trivia print + span splice, caret diagnostics + furthest-failure, lints wired into `Compose`, packrat (exponential→linear fixture) + incremental reparse, panic-mode recovery (3 errors → 3 diagnostics), JSON/canon surfaces |
| C ΔL/merge | M13–M18 | ✅ | `WaveCSuite` — rename-morphism pushouts, `List[E]` functors, path edits (`edit f at [2] = …`), compose/invert/commute laws, three-way semantic merge + conflict artifacts, migrations transporting modules *and* change-sets |
| D proofs | M19–M24 | ✅ | `WaveDSuite` — `$neq/$fresh/$lt/$le` + injected extensions (the shadowing exploit now rejected), computational `$ctx-lookup`, unification search (type inference for all goldens), tactic replay through the independent checker, seeded property claims with shrinking, certified evaluation traces (3 tamper modes fail) |
| E computation | M25–M29 | ✅ | `WaveESuite` — full interaction combinators (γδε + labelled-konst copy), general λ lowering with δ-trees + readback (α-equivalent to tree eval), parallel reduction (identical normal forms), compiled dispatch agrees over 300 random terms, Bend surface runs end-to-end |
| F rosetta | M30–M34 | ✅ | `WaveFSuite` — `quicksort : ∀a. Ord a ⇒ …` + counter effect + ADT in ONE artifact; four ports (Scala/Lean/Haskell/Rust) all passing the whole-file byte fixpoint; Scala port RUNS (`ALL TESTS PASS`); scaffolds + obligations.json (8 entries) via the JSON surface |
| G ledger/trust | M35–M40 | ✅ | `WaveGSuite` — Merkle state + light-client inclusion proofs, 2-of-3 quorum authority rotation (round-robin enforced), policy language (with ΔPolicy) gating head updates, localhost-HTTP want/have resumable sync, 3-node gossip with explicit reorg events, 4-hop provenance `why` |
| H bootstrap/packs | M41–M49 | ✅ | `WaveH1Suite`/`WaveH2Suite` — full meta surface (STLC as text = same digest; [languages/stlc.cairn](languages/stlc.cairn) checked in), **bootstrap fixpoint** ([languages/meta.cairn](languages/meta.cairn) describes the meta language, digest-for-digest), runtime language loading (toy language from pure text), capability manifests + lint, LSP (diagnostics/format/hover; rename = ΔL rename emitting a ValidatedChangeSet) + REPL, query language (3 golden queries, ΔQuery), PKI-max (chain validity as declarative rules checked by the same kernel checker; expiry windows; CRLs), SDS (acetone, ΔSDS domain gate, shadow overrides, re-parseable render, ledger publish), Unison pack (α-digest store; renames touch nothing) |
| M50 | CI/fuzz/bench | ✅ | `FuzzSuite` (100k terms, 6 grammars, zero failures), [.github/workflows/ci.yml](.github/workflows/ci.yml) (JDK 17/21, transcripts, language-file sync check), `cairn.examples.Bench` |

## Golden digests (`sbt "examples/runMain cairn.examples.Main digests"`)

```text
language eu-clp e89b47154f856831680c4f85c0b2049d1fc7a2bc482f79d4e9f39fa77f86ac79
language law    6d39fe8e82738f0da994d62064e4bc74035d5476e570ce85923f4741d127072a
language pki    bebf85a46279c76fb90c3dae71b138e85def650449912bc691aae5e5b72eb3e8
language policy df25113d6bd70b5af73d8eb3a7f86660b742caad33c2e45e0141d630432a01b6
language query  14a04e0fbdbc91a07da588c10ff909dfb8cba28544722e97052e38dfc031150b
language sds    c1b5f87edb106e50a6614bba825475caa7f26ca53e4aebd371c03590a86c9f6f
language sds-report e1506ddf2c9e8ce66b794f2ec229e8606b016fb5aa802f5e8edf4491c7dd810b
language search a5e2f932d079c1b0d4ba6d19e8b6a3e2aefba105c7a29a733ff046d0722a85a9
language stlc   ef1188f151541c1e7dcb738cce62dd3ec0f7172e32313ce4b9d4aa2676bc2f2e
rosetta quicksort2   c2de9525e314f240a4dea977e9ad3992e31d1789b03bce8d5e70ce87dc9d04fb
rosetta quicksortApp f86cc325dd7b736b642893561d555aed3a725624dc2ab912251e5a0a4aa29e9f
```

Closed Law/SDS digests include demoted dependency fragments (`PackLoader`). Phase 2
excluded grammar from all language digests (surface packs). See
[docs/exemplars.md](docs/exemplars.md).

## Benchmarks (`sbt "examples/runMain cairn.examples.Bench"`, this machine)

```text
interpretive TreeEngine      3.62 ms   (400 terms, depth 7)
compiled dispatch (M28)      2.01 ms   (~1.8x)
parse 200-lambda chain       2.85 ms   (packrat)
sequential net reduction     0.62 ms
parallel net reduction       0.34 ms   (3 sweeps, pairs 1,2,3)
```

## Honest deviations from PLAN-2 ACs

- **M7**: byte-identical round-trip holds for whole unedited files + span-precise
  splicing / `RoundTrip.put` (format-preserving single-subtree edit). There is
  still no general dirty-subtree re-association; format-preserving ΔL
  `remove`/`rename` remain unsupported.
- **M10**: incremental reparse retains prefix memo entries; suffix entries are
  discarded (index shift), so "O(affected)" is prefix-anchored.
- **M17**: semantic merge operates on module change histories; `Branches` refs
  are merge-aware via `Branches.merge` / `mergeBranches` → `SemanticRepository.integrate`
  (conflict artifacts persist to CAS without advancing the head; clean merges
  advance and record provenance). `commitTip` persists ValidatedChangeSet
  tip sidecars plus a `.changes` history log (`loadTip` /
  `loadChangeHistory`); `mergeBranches` composes full stacked histories.
  Ledger `SetBranchHead` is opt-in via
  `Branches.publishHead` (or `publish = Some(...)` on merge) — not automatic
  on accept. `Branches` CAS put/get authorize via `EffectContext`; refs FS
  is gated through `Filesystem` (`EffectContext.forBranches`). Node/Sync/HttpSync
  chain-file I/O is gated through `Filesystem` (`EffectContext.forLedger`). CLI
  Transcript/Cli home/run/ui paths, hash/put/canon/transcript source,
  load-language, and `emit-languages` are gated through `Filesystem`
  (`EffectContext.forFilesystem`). Riemann/Search tutorial artifact I/O uses
  `forFilesystem`; Search CAS puts stay on `CasEffects`. Browser board CAS
  inventory authorizes via `CasAdminEffects.artifacts`; UI
  filesystem fallback uses `forFilesystem`. CAS
  `contains` / admin / chunking / Unison store are gated (`CasEffects` /
  `CasAdminEffects`); provenance `index`/`why` authorize CAS `stats` then walk.
  Phase0 trait-contract and WaveA M4 algo-agility tests stay direct by design.
- **M31**: signatures/declarations are fully grammatical; expression BODIES are
  verbatim single-line regions inside the file grammars (rendered by one shared
  fold, not per-host string soup). Whole-file byte fixpoint still enforced.
- **M32/M34**: Haskell/Rust/cargo runs and the Haskell effect projection are
  assume-skipped when toolchains are absent; Scala effect + tests always run
  when scala-cli is present. Lean remains golden-checked.
- **M39**: gossip is an in-process round-based simulation over real node stores
  (validated adoption + reorg events), not a network daemon.
- **M44**: LSP covers initialize/didOpen/didChange/formatting/rename/hover over
  framed JSON-RPC; no workspace folders, no partial edits (full-document sync).
- **M50**: fuzzing covers the six non-layout grammars; layout combinators
  (block/run/adjacent1) are exercised by directed tests instead.
- **M41 source-of-truth**: STLC/meta `.cairn` are checked-in canonical mirrors
  from the Scala seed (`emit-languages`), not yet the runtime load path;
  exemplar packs (PKI/Law/SDS/Search) *are* `.cairn` source of truth.
- **Print derivation (Phase 1)**: `PrintDerive` fills default print rules from
  `syntax` at compose; explicit `print tag : …` overrides win. Redundant prints
  dropped from search/pki/law/sds/stlc. RoundTrip laws remain the trust gate.
- **Surface split (Phase 2)**: semantic language files exclude grammar from
  fragment/language digests. Concrete syntax for stlc/search/pki/law/sds lives
  under `languages/<name>/surfaces/default.cairn`; `PackLoader.bindSurface` /
  `requireClosed` attach the default surface. Capability `grammar` digests the
  bound `GrammarSpec`; `surfaces` digests registered `SurfacePack`s.
- **Surface top (Phase 3)**: Meta parses/prints `surface <style> for <lang> { … }`.
  Existing surfaces migrated off the interim `language <style>` hack; riemann/
  minitt/leancore/unisoncore split. Optional STLC `haskell-style` surface proves
  language digest is surface-invariant. **Meta stays fused** (bootstrap fixpoint)
  but its grammar now describes the surface top.

## Success criteria (§9) — re-verified at maximal level

All 11 still hold, now with: α-invariant identity (3), proofs with side
conditions + search + tactics + traces (4), full interaction combinators with
readback (5), four host ports with byte fixpoints (6), Merkle-proof-verifiable
publication with governed authorities and policies (7), and a self-describing
meta language loaded from text at runtime (11 — the §2b bootstrap is no longer
staged: the fixpoint test passes).

## Parity vs sources (constitution §4.16)

Date: 2026-07-20. Compared each §13 source’s **top-level** surface (README /
flagship demos / primary packs) to Cairn. “On par” = real code + tests matching
that surface, not docs-only stubs. Suite: `ParitySuite` + prior wave suites.

| Source | Top-level capability | Was | Now | Evidence |
|---|---|---|---|---|
| GRANITE | Workbench: fragments, grammar-as-data, ΔL, CAS, meta bootstrap | parity | parity | Waves A–C, H1; `languages/meta.cairn` fixpoint |
| GRANITE | PKI pack: registry, ΔPKI, chain validation, tutorial, ledger publish | partial | **parity** | `languages/pki.cairn` + glue; `PkiMax`/`DemoPki`/`PkiTutorial`; ParitySuite |
| GRANITE | Sharing encryption (X25519 hybrid seal) | missing | **parity** | `ledger/Encryption.scala`; seal/open tests |
| GRANITE | SDS flagship spine: objects, ΔSDS, shadow, multilingual, sealing, tutorial, publish | partial | **parity** | `languages/sds.cairn` (+ `sectionFieldRef` / typed `identificationSection`+`hazardsSection`); `eu-clp` / `sds-report`(+json) packs; `languages/sds/chemicals/*.cairn`; `SdsCausalWorkflow`; ExemplarPackSuite / ParitySuite |
| GRANITE | Law pack (PKI→Law→SDS) | missing | **parity** (thin) | `languages/law.cairn` (requires cert); `enactedBy`; LawTutorial |
| GRANITE | Computation / Bend profile | partial | parity | `AffineNet`/`IcNet`/`Bend` (GRANITE Bend is spec-only) |
| GRANITE | SDS Studio UI / auth web app | N/A | **N/A deferred** | §8 anti-goal (full IDE/studio) |
| ROSETTA | QuickSort Ord + effects + multi-host ports + sample entrypoints | partial | **parity** | `QuickSort2` + `QuickSortApp`; WaveF + ParitySuite |
| granit-rust | MetaLego grammar VM + CAS + PoA + Unison-as-fragments CLI demos | parity (Scala shape) | parity | L0–L6 engines; Unison pack; CLI/transcript |
| marblego | Grammar VM + artifact/branch/store crates | parity (absorbed) | parity | Same L0–L1 story in Scala |
| MetaLego / Kiwi | Composable fragments + STLC | parity | parity | STLC fragments + pushout |
| Eurisko / HVM | Lattice meta / IC lineage | N/A deferred | **envelope** | `docs/agreement.md` + `HvmSurface` + `AgreementSuite` (IC goldens; live `hvm` when on PATH) |

### What closed this pass

- L5 `Encryption` (GRANITE hybrid envelope).
- PKI demo hierarchy + end-to-end tutorial + encryption cert wiring.
- SDS multilingual fallback, shadow rebase/conflict, composition sealing, acetone tutorial.
- Thin Law pack with citation judgment.
- Rosetta `QuickSortApp` (`Peano`, `sortNatWithTrace`, `runSample`).
- Exemplars elevated to `.cairn` languages; `PackLoader` closes `PKI → Law → SDS`
  via fragment `requires`/`provides` (compose without deps fails).

### Trust / authority / repository (2026-07-21)

| Area | Reality |
|---|---|
| Sync | `Sync.pull` / `HttpSync.pull` abort on authorized CAS failure; chain not advanced |
| Delegation | Root grant expiry/nonce/resource justified before hop validation |
| Tips / ΔL | Opaque `ValidatedTip` + `ValidatedChangeSet`; Branches accepts only checked tips; loads replay |
| Capabilities | `EffectContext.withCapabilities` takes `VerifiedCapability` (fromProof only); issuer-scoped `ReplayStore` (memory / durable FS; CAS `replay-snapshot` publish/merge) |
| BranchManifest | Causal digests + `changeHistory`; sidecars write-through caches; causal-LCA merge; journaled accept; `reclaimOrphanBlobs` + conflict `.conflict` root |
| Agreement | Certificate carries `envelopeDigest` + `nativeEvidence`; Lean `natRec` ι + live stdout digests |
| Effect interfaces | `ActionKey` digest-bound; CAS-pinned `effect-interface` via `PinnedInterface` / `ActionKey.fromPinned`; host Meta remains bootstrap |

### Remaining honest gaps

- GRANITE SDS depth beyond the spine: chemical instances load from
  `languages/sds/chemicals/*.cairn`; EU-CLP titles from `eu-clp` profile v1;
  section report is the `sds-report` surface pack (`default` + `json`);
  `sectionFieldRef` cites corpus phrases with `sectionFieldShadow` overrides.
  Typed identification/hazards (#3 partial; thin s1/s2); sections 3–16 still
  generic `euSection` maps. XML/PDF report surfaces, Studio UI, and full
  phrase-staleness-as-ΔL remain open. Causal workflow
  (`SdsCausalWorkflow`) covers author/shadow/rebase/conflict/approve/sign/
  publish without Studio.
- ROSETTA Lean proof *bodies* (Cairn emits obligations; does not re-host Lean
  proofs — §4.10). LeanCore has an **agreement envelope** vs native Lean
  `#check` (refl/subst/`natRec` ι corpus; live stdout digests when `lean` on
  PATH — [docs/agreement.md](docs/agreement.md)), not full kernel compatibility.
- HVM live differential: `HvmSurface` exports HVM2 CON/DUP/ERA books for the
  envelope corpus; live `hvm run` when on PATH. Still not full HVM ABI / Bend /
  HVM5 / labelled-konst isomorphism outside the corpus.
- BFT / gossip daemon / public ledger (explicitly deferred).
- Full granit-rust MetaLego catalog of host languages (Unison/ASN.1/JVM/…) as
  separate packs — absorbed as platform capability, not forked catalogs.
- Crash may leave unreferenced CAS blobs until `reclaimOrphanBlobs`;
  multi-node replay is digest-merge only (not consensus / BFT).
- GRANITE SDS depth / Lean proof bodies / BFT — see above.
