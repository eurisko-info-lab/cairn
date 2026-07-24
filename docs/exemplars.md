# Exemplars (§5b)

Current status of exemplar case studies, including the top-level parity pass
against summarized sources (see [docs/architecture.md](architecture.md) for
the parity matrix). Packs are real code + tests, not name-drops.

**Expression:** PKI, Law, and SDS object languages live in checked-in `.cairn`
files under [content/languages/](../content/languages/) (same meta+grammar surface as
`stlc.cairn` / `meta.cairn`).

**Purity invariant:** Domain apps (PKI, Law, SDS, and future packs such as
loaner) are Cairn. Any domain-specific Scala is a failure of the platform —
not an app feature. Statute/chemical fixtures load from disk via
[[cairn.runtime.ModuleSource]] (e.g. `languages/law/acts/`). Forever-host
concerns (Ed25519, CAS, FS, ledger I/O) must be generic effects, never
domain classes. Remaining Scala under `examples/` / `user/` is **platform debt**
(citation gate, `Sds.validate`, causal handlers, …) to be paid down — see
“Scala orchestration residual” below.

**Changes:** free ΔL only — `Delta.deltaOf(L)` derives `add` / `remove` / …
on demand. It is **never materialized** as checked-in `.cairn` (no `dpki` /
`dlaw` / `dsds` packs, no `docs/delta/`).

**Dependency DAG:** `PKI → Law → SDS`, encoded as fragment
`provides`/`requires` and closed by `PackLoader`:

| Pack | File | Provides | Requires |
|---|---|---|---|
| PKI | `languages/pki.cairn` | `cert` | — |
| Law | `languages/law.cairn` | `law` | `cert` |
| SDS | `languages/sds.cairn` | `sds` | `law` (⇒ PKI) |

Compose of Law without PKI, or SDS without Law/PKI, fails. Closed SDS
amalgamates demoted Law+PKI fragments (certs + statutes + SDS objects).

**Ledger domain trunk** (same DAG as branch ancestry): `SdsDomainTree.plant`
forks `PKI` and `CHEMISTRY` off the trunk, `LAW` with primary `PKI`, and
`SDS` with primary `LAW` + soft reference `CHEMISTRY`. The causal workflow
plants that tree first; work branches (`sds-author`, `sds-approved`, …) hang
under `SDS` via `forkFrom` / `underSds`.

## PKI — on par with GRANITE top-level

`languages/pki.cairn` + `user/pki` (pack façade) + `examples/pki` (Ed25519/chain glue + `PkiMax` + `DemoPki` + `PkiTutorial`):

- Registry object language: `cert` and optional soft `revocation` entries.
- ΔPKI = generic free ΔL (`add` issues, `remove` hard-revokes; soft revoke =
  `add` of a `revocation` term).
- Chain validation + Ed25519; tutorial: issue → validate → revoke → tamper → publish.
- X25519 encryption certificates for SDS composition sealing.

## Law — middle link (PKI → Law → SDS)

`languages/law.cairn` + `user/law` (pack façade) + `examples/law` (`LawTutorial`):

- Statute sections + `enactedBy` citing a PKI cert name as authority.
- Citation judgment (host residual); repeal via free ΔL `remove`; closed language includes PKI `cert`.
- Model Chemical Safety Act: `languages/law/acts/model-chemical-safety.cairn` via `ModuleSource` — not Scala.
- Full statute corpora / jurisdiction profiles still deferred (loaner will add `law-*-credit` packs).

## SDS — on par with GRANITE flagship *spine* (not Studio)

`languages/sds.cairn` + `examples/sds` (`Sds` glue + `CompositionSealing` +
`PhraseStaleness` + `SectionNumbering` / `EuClp` + `Chemicals` /
`ChemicalSource` + `SectionReport` + `SectionFieldStaleness` + `SdsTutorial` +
`SdsCausalWorkflow`):

- Typed objects (substance / mixture / phrase / `corpusPhrase` /
  `translationState` / product / shadow /
  `sectionFieldShadow` / `basis` / `euSection` / typed sections 1–16
  with `fieldLocale` overlays / `outline` / lang-tagged
  `sectionField` / `sectionFieldRef`); rendered document is a compiled
  bidirectional view.
- `basis` cites a Law section number (SDS → Law at the object level).
- ΔSDS = generic free ΔL + domain gate; multilingual phrase + section-field
  fallback; domain-aware shadow rebase with semantic conflict on overridden
  phrases and shadowed euSections.
- Phrase-staleness (`PhraseStaleness`): official `corpusPhrase` never stales;
  free-text EN rewrite is a **derived ΔSDS** changeset
  (`deriveEnRewrite` / `applyEnRewrite`) that materializes `translationState`
  marks; `translationStateTag` judgment gates tags; [[project]] prefers marks
  over pure projection.
- Section-field staleness (`SectionFieldStaleness`): same restale machine;
  EN rewrite derives ΔSDS with `sectionFieldState` marks. Industrial overrides
  remain `sectionFieldShadow` (not Studio).
- Regulatory profile: versioned `languages/eu-clp.cairn` + annex-II v1;
  `sectionNumberOk` / `sectionTitleOk` / `profileVersionOk`;
  `EuClp.conform` host orchestration over those Cairn judgments;
  `SectionNumbering` prefers the profile.
- Chemical instances: `.cairn` under `languages/sds/chemicals/` (acetone /
  ethanol full euSection + thin typed 1–16 subset) via `ChemicalSource`.
- **Report projection (not SDS language):** `sds-report` pack under
  `languages/sds-report*` — surfaces `default` / `json` / `xml` / `csv` / `pdf`
  *consume* SDS modules/outlines. `PdfMinimal` emits one-page PDF bytes.
  RoundTrip trust gate for text surfaces.
- Causal workflow: `languages/sds-workflow.cairn` + `causal.cairn` (checked
  sequence); host `SdsCausalWorkflow` plants `SdsDomainTree` then runs
  effectful Branches/ledger steps under SDS and attaches judgment-checked
  certificates (`sds-certificate` / `certificateKindOk`).
- Composition sealing via L5 `Encryption` (X25519 hybrid) to PKI encryption
  certs — confidential ingredients recoverable only with matching private key.
- Acetone tutorial publishes industrial shadow to the ledger; H-phrases are
  `corpusPhrase`; free-text `prodName` demonstrates restale.
- **Corpus tutorial (capability-constrained):** `SdsCorpusTutorial` walks
  corpus phrases → section-field restale → industrial shadow → CAS put
  authorized only via a narrow `VerifiedCapability` grant bundle (empty
  grant bundle denies). Run:

```bash
sbt "examples/runMain cairn.examples.Main sds-tutorial"
```

Remaining gaps vs GRANITE (Studio still deferred — no Studio UI in this slice):
- Studio-persisted phrase corpus / authoring UI.
- Effect-interface: `effect-interface` language + `iface.cairn` decls are
  runtime SoT (`EffectBootstrap`); ActionKeys from packs (**no Action enum**);
  `Family` thin routing + Fragment/`packDecls` cold-start seeds remain.
- **Scala orchestration residual (platform debt):** `Law.citationCheck`,
  `Sds.validate` / `EuClp.conform` outline walks, `SectionReport.toCst`,
  effectful causal/cert minting, PKI `$sig-ok` / chain rules still host-injected.
  Facts and encodings are Cairn; drivers remain host until module judgments +
  effect-bound `WorkflowRunner` land. Paying this down is a Cairn platform
  task — domain apps must not grow new Scala.
- Production BFT / peer discovery (replay sync is digest-merge; `BftQuorum` is
  research/sim only).
- Multilingual: FR deepened + DE on ethanol + corpus `fieldLocaleRef`;
  lang-scoped shadows still absent (shadows are lang-blind).

## Bend — on par with GRANITE computation *intent* (surface profile)

`examples/bend` + `examples/icnet`: Bend-flavored surface lowered to full
interaction combinators. GRANITE itself defers Bend as a pack (spec-only);
Cairn ships a real thin surface because nets are real.

Remaining: HVM strict/lazy modes; native NUM/OPR (Church numerals +
honest self-ref boundary land in WaveESuite).

**Agreement envelope** (not full HVM compatibility): AffineNet/IcNet claim the
classical IC rule table + corpus NFs vs recorded goldens; `HvmSurface` exports
HVM2 books; live `hvm run` when on PATH. See [agreement.md](agreement.md).

## Unison Core — general-purpose hosted language, peer to STLC/MiniTT (§2c)

`languages/unisoncore.cairn` + `examples/unison` (`UnisonCore` glue +
`Unison` store/codebase). Was an "ideas pack" (α-invariant digests + name
aliases + patch-as-ΔL over *borrowed* STLC terms); §2c/§5b's amendment
sanctioned a real term language for that store, not a Unison fork — Unison
still informs L0/L1/L5 more than any one domain ADT (§5b). Not a domain
pack in the PKI/SDS sense: it sits at the "general-purpose hosted
language" layer of §2c's stack diagram, alongside STLC and MiniTT — an
earlier revision of this doc and one table row in `CAIRN-PROMPT.md` §5b
called it a "domain language pack (peer to PKI/SDS/Search)," which
contradicted §2c's own stack diagram and has been corrected.

- Closed built-in ADTs — `List`/`cons`/`nil`, `Option`/`some`/`none` — plus
  general application/lambda (STLC's own shape) and pattern matching as a
  small fixed family of match forms (`matchList`/`matchOption`, two- and
  one-binder respectively), not a general user-declarable-ADT mechanism —
  same honest, closed-set limitation as MiniTT's hardcoded `Nat`.
- One minimal ability, `Abort` (`abort`/`handle ... with ...`),
  non-resumptive (REffectV2's "unit-typed ops" simplicity, not general
  algebraic effects). `handle`'s reduction is one rule per closed value
  shape rather than one generic passthrough rule: `compute.TreeEngine`'s
  normal-order rewriting tries root rules before recursing into children,
  so a generic `handle($v,$h) => $v` would fire on an unreduced body
  immediately, discarding it before it ever gets the chance to reduce to
  `abort()`.
- Simply typed (`arrow`/`List`/`Option`/`Unit`); types are never reduced,
  so unlike MiniTT there is no `$defeq`/`t-conv` — `hasType` needs zero
  `CheckerCfg` extensions.
- `Unison.Codebase`/`applyPatch` (M48) are unchanged in shape — `names:
  Module` already used the same generic type PKI/Law's registries do, so
  there was nothing to fix there. `Store`'s `BinderSpec` now reads from
  `UnisonCore.language` directly instead of a hand-hardcoded `lam`-only
  map, so `Alpha.digest`/`normalize` see `matchList`/`matchOption`'s
  pattern binders too. `Store` itself was rewired to hold bodies in
  `workbench.Cas` (the same store PKI/Search already use directly, no
  wrapper) instead of a hand-rolled `Map[String,Cst]` — that map was a
  second, redundant copy of exactly what `Cas` already provides; `Store`
  now keeps only a `Set[Digest]` index of which of its own definitions
  have been added, not the term bytes themselves.
- Not Unison-surface-compatible (prefix `(f a)` application, not `f a`
  juxtaposition) — a Cairn-native calculus in Unison's lineage, not a port.

Remaining: automated refactor sessions across dependents; user-declarable
ADTs/abilities. Thin call graph via `call($h)` + host `$call-type` is in;
type-preserving edit propagation is **detection** (recheck), not auto-patch.
user-declarable ADTs/abilities.

## MiniTT — the formal-methods IR ladder's dependent-types rung (§8b)

`languages/minitt.cairn` + `languages/minitt/surfaces/default.cairn` + `examples/minitt` (`MiniTT` glue). A minimal,
closed dependent type core — Π types, a 2-level non-cumulative universe
hierarchy (`Type : Type1`, `Type1` itself untyped), and one hardcoded `Nat`
inductive with a dependent-motive recursor — checked by the *same* generic
kernel `Checker`/`Search` as STLC/PKI/Search's judgments. NOT full CIC (§8
anti-goal): no universe polymorphism, no user-declarable inductives, no
tactics/elaboration. NOT Lean-surface-compatible — a genuinely different,
Cairn-native calculus merely inspired by the same lineage. LeanCore (below)
climbs the next rung on top of this one (identity types, an environment).

- The one judgment shape STLC never needed: type **conversion**. `t-app`
  and the recursor's result type are true substitutions/reductions, not
  values `Checker.matchPat`'s syntactic matching can produce on its own —
  checked via a `$defeq` side condition (PKI's `$sig-ok`/`$anchor` extension
  shape), normalizing both sides (`compute.TreeEngine`, unmodified/generic)
  and comparing up to alpha (`kernel.Alpha.normalize`, unmodified/generic,
  already existed from M2).
- `t-lam` vs `t-lam-conv`: checking a lambda against an expected Π type
  needs its own domain annotation and the Π's domain slot to unify — fine
  when the expected domain is unbound (synthesis) or written identically,
  too strict when it's merely defeq (an unreduced `app(motive, k)` standing
  for `Nat`, e.g. inside the `Nat` recursor's step-function premise).
  `Search.infer` commits to the first fully-successful rule per goal with
  no cross-premise backtracking, so the bridge is a second rule — tried
  only once the stricter `t-lam` fails — rather than a retry inside it.
- Checking mode only (documented limitation, matching MiniTT's own doc
  comment): `$defeq` needs both sides already ground, so synthesizing a
  type through it isn't supported — every `MiniTT.check` call site
  supplies a concrete target type.

Remaining: universe polymorphism; user-declarable inductives; a tactic
layer (M22's `Tactics.replay` machinery already exists generically —
MiniTT doesn't yet use it). Identity types are LeanCore's, not this pack's.

## LeanCore — identity types and a checked-declaration environment (§2c amendment)

`languages/leancore.cairn` + `examples/leancore` (`LeanCore` glue). The next
rung: everything MiniTT has, plus `Eq`/`refl`/`subst` (identity types) and a
minimal environment of checked declarations — §2c's "executable reference vs.
optimized backend" amendment's Lean-kernel *fragment*, named as future work
when that amendment landed and built in this pack. Same generic
`Checker`/`Search`/`TreeEngine`, same `$defeq` extension shape as MiniTT —
literally the same rule file with three more constructors and three more
judgment rules added.

- `subst` (transport), not full path-induction `J`: `J`'s motive depends on
  the equality proof itself (`(x y : A) (p : Eq(A,x,y)) -> Type`), not just
  the endpoint (`A -> Type`) — a substantially harder rule to search over.
  `subst`'s one reduction rule (`subst($P, refl($a), $px) => $px`) and one
  typing rule (`... where $defeq($U, app($P, $b))`) mirror `natRec`'s own
  shape exactly, so this landed without needing a new engine workaround —
  MiniTT's `t-lam`/`t-lam-conv` split already covers the cases that came up,
  including a "symmetry via `subst`" derivation whose motive itself mentions
  the equality type being transported.
- Environment: an ordered, Scala-level list of `(name, type, value)`
  declarations that fold into the same `ctxCons` chain `hasType` already
  walks — no new grammar or engine machinery. `Environment.extend` checks
  the value against the stated type in the current environment's context
  before appending, i.e. "theorem checking." Declarations are opaque once
  checked (no delta-unfolding) — closer to Lean's `axiom`/`theorem` than a
  `def` later terms can unfold through, an explicit, honest limitation.
- `ΔLean` is free: `Delta.deltaOf(LeanCore.language)` works the moment the
  language is composed, same as every other pack — no bespoke change
  language was written or needed.
- Not in scope: user-declarable inductive types, universe polymorphism,
  real Lean 4 surface syntax or import/export.

Remaining: full path-induction `J`; universe polymorphism; user-declarable
inductives; a real Lean import/export surface. Host-side transparent `def`
delta-unfold (`Environment.unfold`) lands; opaque theorems stay unfolded-not.

**Agreement envelope** (not Lean-kernel compatibility): closed LeanCore fragment
(`Eq`/`refl`/`subst` + `natRec` ι) vs native Lean `#check` when `lean` is on
PATH (live stdout digests), else goldens — see [agreement.md](agreement.md).
Rosetta LeanPort obligations stay separate (§4.10).

## Rosetta QuickSort — on par with ROSETTA entrypoints

`QuickSort2` (Ord + effects + four ports) plus `QuickSortApp` (`Peano`,
`sortNatWithTrace`, `runSample`). Full Lean proof bodies from
`QuickSortOrdEffects.rosetta` remain host obligations (`sorry` / tests), not
re-proved in Cairn — by design (§4.10).

## Riemann — an open claim, not a parity item

Not a §13 source — no summarized prior project claims the Riemann Hypothesis,
so this pack is exploratory, outside the parity matrix in
[docs/architecture.md](architecture.md). It exists to demonstrate the
`Claim` vs `Theorem` vocabulary (§2) at its honest limit: RH is unproved (an
open problem in mathematics) and undecidable by Cairn's kernel (a decidable
syntactic term checker, §2b/L2, with no business in continuous complex
analysis).

`languages/riemann.cairn` + `languages/riemann/surfaces/default.cairn` is a small standalone (no `requires`) pack for
analytic propositions (`Term`/`RExpr`/`Prop` sorts). `examples/riemann/Riemann.scala`
builds the standard critical-strip formalization — "∀s, (ζ(s)=0 ∧ 0<Re(s)<1)
→ Re(s)=1/2" — as a `cairn.proof.Claim`, and projects it to Lean, referencing
mathlib's real `riemannZeta`, as `def riemann_hypothesis : Prop := ...`
(never `theorem ... := by sorry`, since a bare `def` asserts nothing — the
honest rendering of a proof-free claim). **No `Theorem` or `Certificate` is
ever constructed for this claim anywhere in the pack**; the obligations
manifest records `kind: "claim"`, `status: "open"`. Lean is the only
projection target (no Scala/Haskell/Rust ports — only Lean/mathlib has real
analytic-continuation machinery); the generated file is not built against
real mathlib in this repo's tests (no network, multi-GB dependency) — only
round-trip verified through Cairn's own grammar engine, same discipline as
the Haskell/Rust ports skipping when `runghc`/`cargo` are unavailable.

## Search — Fact–Intent–Hint board spine

`languages/search.cairn` + `examples/search` (`Search` glue + `SearchTutorial`):

- Standalone pack (`provides search`, no `requires`) — not part of PKI→Law→SDS.
- Sorts Fact / Intent / Hint / Edge / Board; ctors `origin`, `goal`, `fact`,
  `intent`, `hint`, `supports`, `spawns`, `board`.
- `wellFormed` / `goalMet` are real declarative judgments in `search.cairn`,
  checked by the same generic kernel `Checker` as PKI's chain judgment / STLC's
  typing judgment — a board is a `ctxCons`/`ctxNil` context, `wellFormed(ctx,
  term)` checks one term (nonempty text; `supports`/`spawns` endpoints resolve
  via `$ctx-lookup`), `goalMet(ctx, g, f)` checks one candidate witness pair.
  `Search.checkWellFormed` / `Search.checkGoalMet` do the "untrusted search
  proposes, kernel certifies" two-step (`cairn.proof.Search.infer` then
  `Checker.check`). **Honest limitation:** `board(list)`'s variable-arity list
  has no fixed-arity `pat` shape to match against in the rule DSL, so `board`
  membership stays a host-Scala check (`Search.wellFormed`) — not a gap, a
  structural limit of the current rule engine. Host `Search.wellFormed` /
  `Search.goalMet` plus Claim+Certificate gates remain the whole-board checks.
  `Search.certifyEdge` issues a test-suite Certificate for a `supports`/`spawns`
  edge after well-formedness (unchanged — still `"test-suite"`, not upgraded to
  a proof-term certificate, to avoid changing already-asserted behavior), and
  records provenance so `cairn why` walks Intent → Fact → Certificate. ΔL =
  `Delta.deltaOf(search)` only (no `dsearch.cairn`).
- LSP (`surface/Lsp.scala`) exposes format-preserving `add`/`replace`/`remove`/
  `edit` as `workspace/executeCommand` (`cairn.addDef`, `cairn.replaceDef`,
  `cairn.removeDef`, `cairn.editDefAt`) for any registered language, alongside
  the existing `textDocument/rename` — the LSP spec has no standard verb for
  "add a definition," so these are custom commands returning their edit inline
  in the response rather than round-tripping a `workspace/applyEdit` request
  (a deliberate simplification; there's no real bidirectional client in this
  demo server to round-trip that with).
- Tutorial seeds origin+goal, ΔL-adds Intent+Fact+edge into CAS, certifies the
  edge, records Provenance.
- Transcript: `transcripts/search-board.cairn`. Explorer **Board** tab is
  read-only graph over digests ([docs/explorer.md](explorer.md)).

Out of scope this pass: Dispatcher, LLM workers, peer stigmergy RPC, Hint
Law/Policy publish gates (capability `workflows` / `trust` remain deferred).
