# Exemplars (§5b)

Status of the four exemplar case studies after PLAN-2. All four are now
implemented packs (thin but honest slices); remaining gaps are listed inline.

## PKI — implemented (maximal, M46)

`examples/pki` (`Pki` + `PkiMax`):

- **Registry object language** with a round-tripping surface, plus validity
  windows (`notBefore`/`notAfter`) on certificates.
- **ΔPKI is the generic ΔL**: issue = `add`, revoke = `remove`.
- **Chain validation as declarative inference-rule DATA** (`chainOk`), checked
  by the SAME kernel checker as STLC typing; Ed25519 verification and anchor
  membership are injected side-condition evaluators (M19) — the checker stays
  the sole certifier. Expiry enforced via `$le` side conditions.
- **CRLs as signed artifacts** applied to registries; **ledger trust-anchor
  publish** via `RecordCertificate`.

Remaining gaps: revocation *reason codes*, CRL freshness/next-update windows.

## SDS — implemented (thin slice, M47)

`examples/sds`: typed objects (`substance`, `mixture`, `phrase`, `product`,
`shadow`) as a language pack whose **rendered document is a compiled view**
(and itself a bidirectional surface — renders re-parse). ΔSDS = the generic ΔL
plus a domain gate (mixture percentages ≤ 100, no broken references). The
acetone tutorial with a shadow-overridden hazard phrase and a ledger publish
lives in `WaveH2Suite`.

Remaining gaps vs GRANITE's flagship: multilingual phrase fallback chains,
regulatory section numbering, the SDS studio UI.

## Bend — implemented (surface profile, M29)

`examples/bend`: a Bend/Kind-flavored functional surface (juxtaposition
application via the `Run` layout combinator, `@x (…)` lambdas) lowered to FULL
interaction combinators (`examples/icnet`: γδε with commutation and labelled-
constant duplication), reduced, and read back. It exists only because
replicators became real — the honesty rule held.

Remaining gaps: numbers/recursion primitives, HVM-style strict/lazy modes.

## Unison — implemented (ideas pack, M48)

`examples/unison`: a name-independent definition store over ALPHA-INVARIANT
digests (M2); names are aliases in a tiny `names` language whose ΔL is the
patch language. The "no builds" demo: rename everything, definition digests
unchanged, store untouched.

Remaining gaps: term dependencies (hash-linked call graphs), Unison-style
propagation of type-preserving edits.
