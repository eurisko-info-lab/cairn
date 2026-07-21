# Lowering: tree terms to interaction nets

Which surface terms become nets, and how. Two net languages ship:

## AffineNet (fan/era — the replicator-free fragment)

`examples/affinenet` lowers the **affine λ-subset** (each variable used at most
once; unused binders get ε). Replicators are structurally absent — the kind
table has none — which is itself a checkable property.

## IcNet (full interaction combinators, M25/M26)

`examples/icnet` adds δ (duplicators) with the classic table: γγ/δδ annihilate,
γδ COMMUTE (each copies past the other), ε erases, and δ copies labelled
constants (`@left`/`@right` rule agents inherit kind AND label).

- **General lowering**: full STLC λ-terms (var/lam/app + boolean constants);
  variables used n ≥ 2 times get a δ-tree off the binder port.
  `if/then/else` still does not lower (it stays on the tree engine).
- **Readback** (M26): normal-form nets decode to λ-terms; results are
  alpha-equivalent (M2) to the tree evaluator's on the whole corpus, including
  duplication (`(λd. d d)(λy. y)` and Church-two application).
- **Parallel reduction** (M27): all agent-disjoint active pairs fire per sweep;
  confluence makes results order-independent (asserted in `WaveESuite`).
- **Bend profile** (M29): `examples/bend` parses a Bend-flavored surface,
  inlines defs, lowers to IcNet, reduces, reads back.

## Engine

`compute.NetEngine` is generic: kinds and rules are data (`NetLanguage`);
rewiring is port fusion (internal binder–body wires resolve correctly);
pairs without rules (interface anchors) are part of the normal form.
Well-formedness (declared kinds, arities, exactly-once wiring) is checkable.

## Still deferred

Call-by-need boxes/levels for exponentials; lowering for `if`; net-level
benchmarking against the compiled tree engine beyond the Bench harness;
HVM CLI surface exporter (agreement uses classical-IC goldens — see
[agreement.md](agreement.md)).
