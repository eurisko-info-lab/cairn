# Agreement envelopes (Lean · HVM)

Honest differential boundaries — **not** full Lean-kernel or HVM-runtime
compatibility. LeanCore and AffineNet/IcNet are Cairn-native calculi in those
lineages. Agreement is a closed **envelope** plus certificates, not a claim that
Cairn re-hosts Lean proofs or speaks HVM’s ABI.

## Pipeline

```text
Cairn reference term
  → native representation
  → native execution/check   (live tool | recorded golden | stub)
  → imported result
  → AgreementCertificate
```

`cairn.core.Agreement` defines envelopes, issues certificates, and checks them.
`AgreementSuite` always runs Cairn-side reference checks; native tools on `PATH`
are optional.

## Lean envelope (`lean-core`)

| | |
|---|---|
| **Cairn side** | `LeanCore` — MiniTT + `Eq`/`refl`/`subst` + opaque checked decls |
| **Native side** | Lean 4 `#check` on a *tiny projected fragment* (not Rosetta `LeanPort`) |
| **Claims** | Closed fragment: `sort0`/`sort1`, `Nat`+`zero`/`succ`/`natRec`, Π/λ/app, `Eq`/`refl`/`subst`. Cairn `hasType` agrees with projected `#check`. `subst(P, refl(a), px) ⇝ px` matches Lean `Eq.ndrec` (Type-level ι; `Eq.subst` is Prop-sorted). `natRec` ι-zero / ι-succ for identity-on-`Nat` agree with Lean `N.rec` definitional equalities on the corpus. |
| **Does NOT claim** | Lean 4 kernel/elaborator/tactics; Lean surface / import / mathlib; full CIC (universe polymorphism, user inductives, `J`, delta-unfolding `def`s); Rosetta theorem bodies (`sorry` obligations stay obligations — §4.10). |

### How tests run

1. **Always:** Cairn `check` / `normalize` on the corpus → `cairnResult` digest.
2. **If `lean` on PATH:** write the projected snippet, run `lean`, import ok/fail → `live:lean:exit=…;out=<stdout-digest>`.
3. **Else:** compare against recorded golden outcomes → `golden` (CI stays green).
4. **Never:** pretend Rosetta Lean ports are kernel-checked.

Corpus cases: `refl-zero`, `refl-bad`, `subst-refl`, `natrec-zero`, `natrec-succ`.

## HVM / IC envelope (`hvm-ic`)

| | |
|---|---|
| **Cairn side** | `AffineNet` (γ/ε) and `IcNet` (γ/δ/ε + labelled `konst`) via `NetEngine` |
| **Native side** | Classical IC table outcomes (goldens); `HvmSurface` HVM2 book export; optional live `hvm run` |
| **Claims** | Affine γ/ε annihilation+erasure; full IcNet table (γγ/δδ annihilate, γδ commute, ε erases, δ copies konst); lowered λ-net NFs agree with recorded classical-IC corpus outcomes; `HvmSurface` projects the corpus to HVM2 CON/DUP/ERA books and live `hvm` agrees when present. |
| **Does NOT claim** | Full HVM/HVM5 ABI or memory layout; HVM2 NUM/OPR/SWI; Bend/HVM5/Kind surface syntax; strict/lazy modes; numbers/recursion beyond the corpus; full Bend/Kind/QDIC; opaque labelled `konst` ↔ Church `@True`/`@False` outside the corpus. |

### Surface exporter (`cairn.core.HvmSurface`)

Projects STLC λ-terms (and the era-fan net fixture) into two honest forms:

1. **Classical IC λ-text** — `(λx. body)`, `(f x)`, `true`/`false` (`icLambda`) for golden-stable lineage digests.
2. **HVM2 book IR** — `(a b)` = CON/γ, `{a b}` = DUP/δ, `*` = ERA/ε, `@Name` refs, `& t ~ u` active pairs (`bookFromLambda` / `bookEraFan`). Booleans are Church-affine `@True`/`@False`, not Cairn `konst` labels.

This is a **projection for the envelope corpus**, not a general Cairn↔HVM compiler.

### How tests run

1. **Always:** lower → normalize → readback / kind fingerprint → `cairnResult`; export IC + HVM2 surfaces.
2. **Goldens:** hand-specified classical IC expected readbacks / fingerprints (not “Cairn agrees with itself”).
3. **If `hvm` on PATH:** write the HVM2 book, run `hvm run`, accept coarse result tokens (`@True`, `*`, identity forms) → `live:hvm:…` (export digest in the live detail). Envelope excludes stay narrow.
4. **Else:** classical golden path → `golden`.

## Certificate shape

```text
AgreementCertificate {
  envelopeId     : "lean-core" | "hvm-ic"
  envelopeDigest : Digest(claims + excludes)
  caseName       : corpus case id
  subject        : Digest(Cairn reference term)
  cairnResult    : Digest(Cairn outcome)
  nativeResult   : Digest(imported / golden outcome)
  source         : "golden" | "live:<tool>:<detail>" | "stub:<reason>"
  nativeEvidence : Digest(source detail)
  agreed         : Bool
}
```

Artifact kind: `agreement-certificate`. `Agreement.check` rejects mismatched
digests, inconsistent `agreed` flags, or envelope-digest drift.

## Relation to Rosetta

Rosetta Lean ports remain **projection + obligations** (`docs/rosetta.md`).
Agreement certificates cover the LeanCore *kernel fragment* only. Do not conflate
`#check` on the envelope corpus with verifying Rosetta `theorem … := by sorry`.
