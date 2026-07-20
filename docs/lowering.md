# Lowering: tree terms to interaction nets

Which surface terms become nets (§6 Phase 3), and how.

## Scope

`examples.affinenet.AffineNet.lower` lowers the **affine λ-subset** of STLC terms:

- `var x` — a wire to the binder's port; **at most one occurrence** per binder
  (a second occurrence is a structured error: nets here have no replicator).
- `lam x . b` — a `fan` (γ) agent: principal = the term's port, aux1 = binder wire,
  aux2 = body. Unused binders get an `era` (ε) agent.
- `app f a` — a `fan` agent: principal at the function (so β becomes a γγ active
  pair), aux1 = argument, aux2 = result.
- `true` / `false` / `konst c` — inert `konst` agents (opaque constants).

Types are erased during lowering. `if/then/else` does **not** lower (it would need
either dedicated agent kinds or replicators); it stays on the tree engine.

## Correctness

β-reduction on the tree side corresponds to γγ annihilation on the net side:
`(λx.x) true` normalizes to `true` on both engines (`Phase3Suite`,
"lowered affine term reduces to same value as tree eval"). Erasure of an unused
argument corresponds to ε-propagation.

## Engine

`compute.NetEngine` is generic: agent kinds and interaction rules are data
(`NetLanguage`). Rewrites happen only at principal–principal pairs **that have a
rule**; pairs without rules (e.g. a `free` interface anchor against anything) are
part of the normal form. Rewiring is by port fusion, so wires internal to the
consumed pair (binder-to-body) resolve correctly. Well-formedness (declared kinds,
arity bounds, every port wired exactly once) is a checkable judgment.

## Deferred

Replicators/duplication (non-affine λ), Bend/Kind/HVM surface profiles — see
[exemplars.md](exemplars.md).
