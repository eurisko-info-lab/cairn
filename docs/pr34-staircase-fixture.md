# PR34 Staircase Fixture: G0 -> G1

This document is the permanent conformance walkthrough for the first
load-bearing stair in PR34.

It records the fixture contract for the governed successor transition
`G0 -> G1` and ties that contract to executable parity checks.

## What this fixture proves

The fixture demonstrates one complete successor step where:

1. `G0` reconstructs to `S0`.
2. A governed finalized transition (`delta0`) yields `G1`.
3. `G1` reconstructs to `S1` in independent implementations.
4. The `delta0` transition is independently auditable from ledger publication.

This is the first concrete instance of the staircase claim:

- `Meaning_n => Foundation_{n+1}`

## Authoritative implementation anchors

The fixture is implemented and checked in:

- CKC parity suite: [app/tests/src/test/scala/cairn/tests/CKCParitySuite.scala](../app/tests/src/test/scala/cairn/tests/CKCParitySuite.scala)
- PR34 schema tests: [app/tests/src/test/scala/cairn/tests/Pr34EnvelopeSuite.scala](../app/tests/src/test/scala/cairn/tests/Pr34EnvelopeSuite.scala)
- Rust verifier CLI: [verifier-rust/src/main.rs](../verifier-rust/src/main.rs)
- Lean verifier CLI: [verifier-lean/Main.lean](../verifier-lean/Main.lean)

The governed successor delta captured by the Scala fixture is
`governedDeltaG0ToG1` (second finalized transition digest).

## Frozen fixture digests

The parity suite fails closed if any of these drift:

- federationId:
  `4e9330155c00de9d5122866d30002185726acc4a64aa28953bb6d47f53afdd96`
- genesisState:
  `2572d018e52b0027127b2299fece3bb58390d450f982e8647e604819479dfb28`
- G0 resolve digest:
  `81b66603400140329bd60ad0f8e3d3b815b24068021e0118d1bda3fe8d1c3581`
- G1 resolve digest:
  `6707bb0a84b82cc04f088faecb297435d10d5aff9226c7aff6a27c7de154de5e`
- replica manifest digest:
  `6757960c891274d6fdc025a4eba54d3e9fb711e80048b747700e1ade201c3626`
- cert1 digest:
  `59792e4bb96b2f34ad88e85f09d0fbefd9cb3780d9b3fd1800bdaa021fa713c8`
- cert2 digest:
  `0ec2e8c9342407b83a1837c3d7a9d3b57fa35e6b1f9bd45e2453f697a495065b`

Promoted foundation handoff digest (`pr34-foundation-handoff-v1`):

- `d10ce80d56289df0c2e032aa8362cc137960122da99848cef2b1c94145672242`

This promoted handoff digest includes frozen canonical verdict evidence for:

- resolve(`G0`),
- resolve(`G1`), and
- replay-history(`G0 -> G1`) under the fixture constitution.

It also includes governed artifact choices that were previously implicit host
selection points:

- language digest,
- grammar digest,
- runtime digest,
- machine digest,
- acceptance-constitution digest.

And it now freezes one governed reconstruction policy choice:

- replay max-steps profile used for promoted `G1` replay evidence.

The promoted handoff also freezes the successor world's reconstructed state:

- final `S_{n+1}` digest,
- final epoch,
- verified transition count.

That means the next foundation step can consume the handoff payload directly
instead of re-deriving the successor state from ambient host policy.

## Conformance procedure

Run these checks from repository root:

1. Staircase + replay + cert-binding parity gate:

```bash
sbt -batch "testOnly cairn.tests.CKCParitySuite"
```

2. PR34 envelope/schema contract gate:

```bash
sbt -batch "tests/testOnly cairn.tests.Pr34EnvelopeSuite"
```

3. Rust verifier self-tests:

```bash
cd verifier-rust
cargo test
```

The CKC parity suite includes all required staircase assertions:

- cross-language valid/invalid staircase checks,
- malformed digest rejection parity,
- deterministic G0/G1 fixture digest freeze,
- deterministic cert fixture digest freeze,
- deterministic governed `delta0` digest freeze,
- deterministic promoted-foundation handoff digest freeze,
- explicit Scala/Rust evidence parity for `G1` reconstruction against the
  promoted foundation handoff payload,
- independent audit of the governed `G0 -> G1` transition.

## Failure interpretation

- If a frozen digest assertion fails, fixture construction changed and the
  staircase contract has drifted.
- If staircase-check parity fails, Scala/Rust/Lean disagree on successor-link
  judgment behavior.
- If independent audit fails, `delta0` is not verified as a real
  ledger-published finalized transition.

Any of these failures blocks promotion of this step as a foundation.

## Promotion note

This fixture is the concrete handoff point for post-PR34 work:

1. freeze this step,
2. express one more trusted choice as governed artifacts,
3. construct/finalize the next successor,
4. independently reconstruct again,
5. promote as the next foundation.
