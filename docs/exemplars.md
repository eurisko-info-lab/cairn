# Exemplars (§5b)

Status of the four exemplar case studies. Thin honest slices or documented
deferrals — no hollow stubs (§9.10).

## PKI — implemented (thin slice)

`examples/pki` mirrors GRANITE's first application pack:

- **Registry object language**: `cert NAME key "<hex>" issuer NAME sig "<hex>"`
  terms in a composed `pki` language (grammar round-trips like every other pack).
- **ΔPKI is the generic ΔL**: `Delta.deltaOf(pki)` — issue = `add`, revoke =
  `remove`. No bespoke change machinery; the §2b closure does the work.
- **Chain-validation judgment** over **real Ed25519** signatures: walks
  subject → issuer → … → trust anchor, verifying each link's signature against the
  issuer's registered key; rejects forged signatures, revoked/absent issuers,
  cycles, and self-signed non-anchors.
- **Ledger trust-anchor publish**: the anchor certificate digest is recorded via a
  `RecordCertificate` transaction (`Phase8Suite`).

Not covered (honest gaps vs GRANITE's pack): expiry/validity windows, revocation
lists as artifacts, `ChainValidationJudgment` expressed as declarative `InferRule`
data (it is host code today), SDS-style consumers.

## SDS — deferred, documented target

Safety Data Sheet authoring (chemical regulatory SDS — GRANITE's flagship domain
pack). The intended Cairn shape: typed objects (`Substance`, `Mixture`, `Product`,
shadows, multilingual phrases) as a language pack whose compiled views are documents,
with ΔSDS = `deltaOf(sds)` plus domain-specific validated ops. Deferred because a
thin-but-honest slice needs the phrase/shadow model, which deserves its own story
arc; PKI already proves the language-agnostic kernel (its original purpose).

## Bend — deferred, documented target

In GRANITE, Bend is a **surface profile** (with Kind/HVM) over the QDIC net spine —
spec only, no pack. Cairn's prerequisite exists (`NetEngine` + AffineNet + affine
lowering), but a Bend-shaped surface needs replicators/duplication, which AffineNet
deliberately excludes. When a replicating net language lands, a Bend profile becomes
a grammar + lowering table over it — not before, and no `examples/bend` directory
exists until then.

## Unison — absorbed as ideas

Not a pack. The ideas already absorbed into L0/L1/L5: hash-identified definitions
(`Module` defs are digest-addressed), names as aliases over content (branch heads and
`TypedKey`s), shareable immutable codebase (CAS + publication by digest). An optional
future pack could model Unison-style patches as a ΔL; nothing pretends to today.
