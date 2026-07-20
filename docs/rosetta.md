# Rosetta ports

Projection, not replacement (§4.10): one typed artifact graph
(`rosetta.RosettaModule`) emits ordinary host projects plus obligations/tests.

## Shipped ports

| Host | Emitter | Output | Verified how |
|---|---|---|---|
| Scala 3 | `ScalaPort` | object with prelude + generated defs + `@main` running theorem obligations as sample-based assertions | emitted declaration region is **re-parsed and round-trip-checked** under the port's own grammar before the file is assembled; acceptance test runs the file under `scala-cli` |
| Lean 4 | `LeanPort` | namespace with prelude, `partial def`s, and `theorem … := by sorry` statements | same round-trip check; golden-content assertions (no Lean toolchain assumed in CI) |

## Design

Both emitters are **grammar-as-data print tables** interpreted by the one generic
printer — the same engine that parses/prints object languages. Each port grammar
covers exactly the shapes the emitter produces, so `print → parse → print` is a
byte-level fixpoint, checked on every emission. Host-specific *semantic* mapping
(Rosetta expr → host Cst shape) is a small mechanical transform; text production is
never hand-concatenated per construct.

## Honest limits

- The expression vocabulary is deliberately small (vars, ints, list literals, calls,
  if) — enough for the QuickSort flagship; extend the grammar + transform tables to
  grow it.
- Lean defs are `partial` (no termination proofs) and theorems are stated with
  `sorry` — they are **obligations**, as §4.10 prescribes, not verified results.
- Prelude regions are fixed literal surfaces per host, outside the round-trip region.
