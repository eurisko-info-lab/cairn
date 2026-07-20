# Cairn — 50-Story Implementation Plan (Scala 3 host)

Host decision (per §6 Phase 0 / §11): **Scala 3** for all engines L0–L3 (and L4–L6).
Rosetta *targets* remain multi-host. Build tool: **sbt**, one module per layer,
import DAG enforced by sbt module dependencies:
`kernel ← workbench ← {proof, compute} ← rosetta ← ledger ← surface`, with
`examples` and `tests` on top (never imported by lower layers).

Each story has an acceptance criterion (AC). Stories land in order; a phase's
stories complete before the next phase starts (§4.13).

---

## Phase 0 — Skeleton (S1–S6)

- **S1. Repo + build skeleton.** Git repo; sbt multi-module layout matching L0–L6
  (`kernel/ workbench/ proof/ compute/ rosetta/ ledger/ surface/ examples/ tests/ transcripts/ docs/`).
  AC: `sbt compile` green; module DAG matches §3.
- **S2. Canonical bytes codec.** Deterministic `Canon` value ADT (ints, strings,
  bytes, lists, sorted maps, tags) with documented normalization (endianness,
  map-key order, UTF-8). AC: encode∘decode = id; same value ⇒ same bytes (golden fixtures).
- **S3. Digests + dual identity.** SHA-256 content `Digest`; `TypedKey`
  (valueHash + typeHash + kind). Never bare hashes as the only reference. AC:
  golden digests for fixtures; typed-key mismatch is a structured error.
- **S4. Local CAS.** Disk-backed content-addressed store of canonical bytes,
  plus in-memory impl behind one interface. AC: put/get round-trip; digest
  verified on read; corruption detected.
- **S5. Artifact vocabulary.** `ArtifactKind` (language, grammar, source, IR,
  claim, theorem, proofTerm, testSuite, certificate, branchManifest, changeSet,
  block, …) and typed `Artifact` envelope with canonical serialization. AC:
  round-trip store/load of every kind's fixture.
- **S6. CLI v0.** `cairn hash | put | get | canon` on files. AC: shell
  round-trip of a fixture file through the disk CAS.

## Phase 1 — MVP Language Workbench (S7–S18)

- **S7. Fragment IR.** Sorts (tree/graph mode), constructors, grammar rules,
  reduction rules, judgments, `provides`/`requires`/`excludes`; canonical form +
  digest identity. AC: fixture fragments round-trip via CAS.
- **S8. Grammar-as-data vocabulary.** `TokenSpec`, `Cst` (leaf/node), `Elem`
  (tok/tokField/cat/opt/star/sepBy1/nameLeaf/numLeaf/strLeaf), `ConstructorSpec`,
  `CategorySpec` (ordered PEG choice), `PrecCategory` (infix table),
  `PrintSeg`/`PrintRule`, `GrammarSpec`. No per-language parser code anywhere.
  AC: grammar values serialize canonically (grammars are artifacts).
- **S9. Generic lexer.** One lexer driven by `TokenSpec` (idents, keywords,
  longest-match punct, numbers, strings, comments). AC: token golden tests.
- **S10. Generic parser.** One recursive-descent interpreter over `GrammarSpec`
  with backtracking ordered choice + precedence climbing; static left-recursion
  checker run on every grammar. AC: parses STLC fixtures; left-recursive grammar
  rejected statically with a cited category path.
- **S11. Generic printer + round-trip law.** One printer interpreting the print
  table; law test `parse(print(t)) = t` and `print(parse(s)) = canon(s)` wired
  as a reusable suite every shipped grammar must pass. AC: law suite green on STLC.
- **S12. Pushout composition.** Fragment amalgamation along shared interfaces;
  conflicts (same name, different definition) are structured errors citing
  fragment names + interface paths + digests. AC: base+STLC compose; crafted
  conflict yields the specified error; import order irrelevant.
- **S13. LanguageDef.** Closed composition of fragments as a first-class value
  with content-hash identity of its canonical form. AC: same fragment set (any
  order) ⇒ same language digest.
- **S14. Terms + binders.** Terms are `Cst`; a binder-spec table (which
  constructor positions bind) drives *generic* alpha-renaming and
  capture-avoiding substitution. AC: substitution property tests (capture cases).
- **S15. Tree reduction engine.** Generic rewriter interpreting reduction rules
  as data (pattern with metavariables → template, `subst` as engine primitive).
  Normal-order strategy; fuel-bounded. AC: engine has zero STLC-specific code.
- **S16. STLC example pack.** Sorts/constructors/grammar/β-rule as fragment
  data under `examples/stlc`. Church booleans evaluate; parse/print/eval identity.
  AC: Phase 1 acceptance — compose, round-trip, `(λx:B.x) true ↦ true`, Church bools.
- **S17. ΔL generation.** For any language `L`, derive free changes language
  `ΔL` (AddDef / ReplaceDef / RemoveDef / RenameWithFootprint), itself a
  `LanguageDef` (so `Δ(ΔL)` exists — forced closure §2b). Edits are validated
  terms, application yields a new digest. AC: ΔL(STLC) edit produces new digest;
  Δ(ΔL) constructible; invalid edit rejected.
- **S18. Branches + history.** `BranchManifest` artifacts; append-only local
  history of validated change-sets; head refs are stable typed keys. AC:
  branch/edit/history walk; heads survive process restart.

## Phase 2 — Proof slot + certificates (S19–S24)

- **S19. Proof artifact kinds.** `Claim`, `Theorem`, `ProofTerm`, `TestSuite`,
  `Certificate` as artifacts with canonical forms. AC: CAS round-trips.
- **S20. Tiny logic.** Formula language (atoms over term judgments, implication,
  conjunction, universal over a finite sort) as data; natural-deduction proof
  terms. AC: formulas/proofs are artifacts with digests.
- **S21. Independent check driver.** Small kernel checker: `check(proof, formula)`
  decidable, no tactic engine required. AC: valid ND proofs accepted; ill-typed
  proof rejected with structured error.
- **S22. STLC typing derivations.** STLC typing rules as judgments; a typing
  derivation for a golden term encoded as a proof term and checked. AC:
  derivation checks; wrong-type derivation fails.
- **S23. Tamper tests.** Forged/altered proof bytes rejected (digest mismatch
  and semantic check both). AC: Phase 2 acceptance tests green.
- **S24. Claims pack.** `examples/claims`: proof-free claims over STLC programs
  with `TestSuite` + `Certificate` (certificate = kernel-signed record of a
  passing suite). AC: claim+tests path works with no proof term.

## Phase 3 — Graph / Δ-net computation (S25–S29)

- **S25. Graph-mode sorts.** Agents with typed principal/aux ports; nets as
  artifacts (agents, wires). AC: canonical net serialization + digests.
- **S26. Net reduction engine.** Generic engine applying interaction rules
  (principal-pair rewrites: annihilate/commute/erase) as data. AC: engine has
  no AffineNet-specific code; deterministic reduction of fixtures.
- **S27. AffineNet pack.** `examples/affine-net`: Fan/Eraser agents only —
  no replicator constructible in the ADT/spec. AC: annihilation + erasure suite.
- **S28. Net well-formedness judgments.** Port arity/linearity checks as
  judgments; violations are structured errors. AC: malformed nets rejected.
- **S29. Lowering note + subset lowering.** `docs/lowering.md`: which tree terms
  lower to nets; implement lowering for the affine λ-subset. AC: lowered term
  reduces to same value as tree evaluator on the subset.

## Phase 4 — Rosetta ports (S30–S34)

- **S30. Rosetta vocabulary.** Declarations (module, data, def, rel, theorem,
  target) as a typed artifact graph — thin but real. AC: Rosetta decls are
  artifacts; canonical round-trip.
- **S31. Port framework.** Ports = generated views + obligations/tests, driven
  by per-host grammar-as-data *print tables* (round-trip-checked emitters), not
  new compilers. AC: emitted text reparses under the host round-trip grammar.
- **S32. Scala port.** Emit an ordinary Scala file + munit test obligations from
  a Rosetta artifact. AC: generated project compiles & tests via `scala-cli`.
- **S33. Lean port.** Emit Lean 4 skeleton + theorem statements (`sorry`-free
  statements, proofs may be stubs/obligations). AC: golden-file check (lake
  build optional if Lean present).
- **S34. RosettaQuickSort.** `examples/rosetta-quicksort`: sorted/permutation
  claims projected to Scala (tested) + Lean (skeleton). AC: Phase 4 acceptance
  transcript: artifact → 2 ports → host tests.

## Phase 5 — Ledger publication, single-node PoA (S35–S40)

- **S35. Transaction language.** Tx ADT (publish artifact digest, set branch
  head, register identity, policy cert) with canonical bytes. AC: tx digests stable.
- **S36. Identities + signatures.** Ed25519 (JDK) dev keys; signed txs; sig
  verification in kernel-pure code. AC: bad signature rejected.
- **S37. Ledger transition relation.** Pure `apply(state, block) → Either[Err, state']`
  in kernel (no I/O); content-addressed state root. AC: property — same txs ⇒ same root.
- **S38. PoA blocks.** Hash-linked blocks sealed by an authority key; genesis;
  chain validation. AC: tampered block/chain rejected.
- **S39. Publication.** Publish branch head + artifact digests + certificates;
  ledger stores hashes/heads, bodies stay in CAS (§4.9). AC: publish STLC pack;
  ledger records digests only.
- **S40. Two-process verification.** Transcript: node A builds chain + publishes;
  separate process B validates blocks and materializes artifacts from CAS by
  digest. AC: Phase 5 acceptance green.

## Phase 6–7 — Distribution hooks + hardening (S41–S45)

- **S41. Blob sync.** Pull-based fetch of missing blobs by hash between two
  local node stores; `docs/distribution.md` covers gossip/fork-choice/BFT as
  future. AC: two nodes converge on a published head.
- **S42. Divergence surfacing.** Competing heads detected and reported as such —
  no silent corruption. AC: forked publish yields explicit competing-heads state.
- **S43. Agreement tests.** Property tests: kernel digests/state roots agree
  with independent app-side recomputation (§ kernel gate). AC: suite green.
- **S44. Self-description bootstrap.** Cairn's fragment IR expressed as a Cairn
  language pack: a grammar (in the grammar engine) for fragment declarations
  that parses/prints fragment definitions — meta-language + grammar-language
  pair staged per §2b. AC: a fragment defined in surface syntax round-trips and
  composes identically to the host-constructed one.
- **S45. Bootstrap doc.** `docs/bootstrap.md`: empty CAS → published STLC in one
  sitting; `docs/assumptions.md` records all assumptions taken. AC: docs exist
  and match the transcripts.

## Phase 8 + Surfaces (S46–S50)

- **S46. Transcript DSL.** Transcript language defined *with the grammar engine*
  (dogfooding): put/compose/eval/assert/publish/fetch steps; runner in surface
  layer; transcripts are CI. AC: `transcripts/mvp.cairn` runs from fresh checkout.
- **S47. PKI pack (minimal).** `examples/pki`: certificate `Registry` object
  language + ΔPKI (Issue/Revoke) + chain-validation judgment over Ed25519 +
  ledger trust-anchor publish. AC: chain validation accepts good chain, rejects
  revoked/forged.
- **S48. Docs set.** `docs/vocabulary.md`, `docs/ledger.md`, `docs/rosetta.md`,
  `docs/exemplars.md` (SDS/Bend/Unison as documented deferred targets — no fake
  stubs, §5b/§9.10). AC: docs exist, honest about deferrals.
- **S49. README + elevator.** README with §12 elevator, host decision, layout,
  how to run tests + transcripts. AC: fresh-clone instructions verified.
- **S50. STATUS.md + full acceptance.** Golden digests of shipped artifacts,
  transcript results, success-criteria checklist (§9) with honest status.
  AC: `sbt test` green across all modules; STATUS accurate.

---

## Story → success-criteria map (§9)

| §9 criterion | Stories |
|---|---|
| 1 Composition | S12, S13, S16 |
| 2 Bidirectional surface | S8–S11, S16, S46 |
| 3 Semantic CAS | S2–S5 |
| 4 Certified path | S19–S23 |
| 5 Δ-net path | S25–S28 |
| 6 Polyglot projection | S30–S34 |
| 7 Ledger publication | S35–S40 |
| 8 Layering | S1 (sbt DAG) |
| 9 Reproducibility | S46, S49 |
| 10 Exemplars | S47, S48 |
| 11 Universal closure | S17 (recursive ΔL), S44 (bootstrap) |
