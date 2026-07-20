# Exemplars (§5b)

Status of exemplar case studies after PLAN-2 **and** the top-level parity pass
(see [STATUS-2.md](../STATUS-2.md) parity matrix). Packs are real code + tests,
not name-drops.

**Expression:** PKI, Law, and SDS object languages live in checked-in `.cairn`
files under [languages/](../languages/) (same meta+grammar surface as
`stlc.cairn` / `meta.cairn`). Scala under `examples/` is host glue (crypto,
domain gates, tutorials) — not the language definition.

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

## PKI — on par with GRANITE top-level

`languages/pki.cairn` + `examples/pki` (`Pki` glue + `PkiMax` + `DemoPki` + `PkiTutorial`):

- Registry object language: `cert` and optional soft `revocation` entries.
- ΔPKI = generic free ΔL (`add` issues, `remove` hard-revokes; soft revoke =
  `add` of a `revocation` term).
- Chain validation + Ed25519; tutorial: issue → validate → revoke → tamper → publish.
- X25519 encryption certificates for SDS composition sealing.

## Law — middle link (PKI → Law → SDS)

`languages/law.cairn` + `examples/law` (`Law` glue + `LawTutorial`):

- Statute sections + `enactedBy` citing a PKI cert name as authority.
- Citation judgment; repeal via free ΔL `remove`; closed language includes PKI `cert`.
- Model Chemical Safety Act slice only — full statute corpora deferred.

## SDS — on par with GRANITE flagship *spine* (not Studio)

`languages/sds.cairn` + `examples/sds` (`Sds` glue + `CompositionSealing` + `SdsTutorial`):

- Typed objects (substance / mixture / phrase / product / shadow / `basis`);
  rendered document is a compiled bidirectional view.
- `basis` cites a Law section number (SDS → Law at the object level).
- ΔSDS = generic free ΔL + domain gate; multilingual phrase fallback; domain-aware
  shadow rebase with semantic conflict on overridden phrases.
- Composition sealing via L5 `Encryption` (X25519 hybrid) to PKI encryption
  certs — confidential ingredients recoverable only with matching private key.
- Acetone tutorial publishes industrial shadow to the ledger.

Remaining gaps vs GRANITE: full chemicals corpus, regulatory section numbering,
phrase-corpus staleness machine, SDS Studio UI (deferred).

## Bend — on par with GRANITE computation *intent* (surface profile)

`examples/bend` + `examples/icnet`: Bend-flavored surface lowered to full
interaction combinators. GRANITE itself defers Bend as a pack (spec-only);
Cairn ships a real thin surface because nets are real.

Remaining: numbers/recursion primitives; HVM strict/lazy modes.

## Unison — ideas pack (intentional)

α-invariant digests + name aliases + patch-as-ΔL. Not a Unison fork.
Remaining: hash-linked call graphs; type-preserving edit propagation.

## Rosetta QuickSort — on par with ROSETTA entrypoints

`QuickSort2` (Ord + effects + four ports) plus `QuickSortApp` (`Peano`,
`sortNatWithTrace`, `runSample`). Full Lean proof bodies from
`QuickSortOrdEffects.rosetta` remain host obligations (`sorry` / tests), not
re-proved in Cairn — by design (§4.10).

## Riemann — an open claim, not a parity item

Not a §13 source — no summarized prior project claims the Riemann Hypothesis,
so this pack is exploratory, outside the parity matrix, and does not appear
in [STATUS-2.md](STATUS-2.md)'s scorecard. It exists to demonstrate the
`Claim` vs `Theorem` vocabulary (§2) at its honest limit: RH is unproved (an
open problem in mathematics) and undecidable by Cairn's kernel (a decidable
syntactic term checker, §2b/L2, with no business in continuous complex
analysis).

`languages/riemann.cairn` is a small standalone (no `requires`) grammar for
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
