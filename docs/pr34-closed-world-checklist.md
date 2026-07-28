# PR34 Closed-World Checklist

This is the execution checklist for PR34. It translates the roadmap target into
one closed semantic campaign with explicit proof obligations, corpus targets,
and commit slices.

## Completion shape

PR34 closes when all are true:

1. one universal result: replay determinism/uniqueness for fixed canonical
   history and constitution;
2. one substantial inhabited end-to-end world exercising the full path from
   package to reconstruction.
3. one validated successor world produced from a constituted change of that
      first world and independently reconstructed to the same result.
4. the reconstructed successor world is promoted as the constituted
      foundation for constructing the next world.

This does not require uniqueness among all conceivable valid histories.
Authority comes from finality selection plus replay determinism.

Canonical wire schema for the current scaffold:
[docs/pr34-envelope-schema.md](pr34-envelope-schema.md).

## Core obligations

1. Replay determinism on fixed finalized history
   - For fixed `K` and `H`, replay yields at most one `S`.
2. Finality safety per federation position
   - No two finalized successors for the same federation slot under the quorum
     model.
3. Independent canonical verdict identity
   - `Canon(ScalaCKC(B)) == Canon(RustCKC(B))` on shared canonical input `B`.
4. Staircase successor closure
      - `G0` reconstructs to `S0`, a constituted change yields `G1`, and `G1`
        reconstructs independently to `S1` with matching canonical verdicts.
5. Inductive foundation handoff
                  - `Meaning_n` is not just replayed; it is promoted to
                        `Foundation_{n+1}` with explicit compatibility evidence.

## Phase checklist (A-G)

Legend: `[ ]` not started, `[~]` in progress, `[x]` complete.

### A. Fix the semantic object

- [~] Freeze one canonical input object `G` with explicit fields for
      constitution, closure, machine/runtime selection, repository root,
      finalized history, and evidence closure.
- [ ] Ensure authoritative validation APIs invoke one judgment center;
      loaders/projections remain non-authoritative.
- [x] Emit canonical package bytes `B = Canon(G)` with stable schema versioning.

### B. Make change real

- [ ] Implement free-change construction with a witness carrying constructor
      identity and composition/identity semantics, not naming only.
- [ ] Ensure real deterministic change application over selected language
      runtime and state.
- [ ] Add deterministic replay tests proving equal outputs for equal
      `(state, change)` input under one runtime.

### C. Make acceptance real

- [ ] Implement constitution-relative acceptance judgment that checks authority
      scope, claim/evidence object, selected decision procedure, and resource
      bound.
- [ ] Remove/forbid kind-only acceptance shortcuts.
- [ ] Add positive/negative vectors for forged scope, mismatched evidence,
      and bound exhaustion.

### D. Make repository replay ordinary CKC

- [ ] Define repository replay as an ordinary CKC query/result path.
- [ ] Cover causal dependencies, one branch head path, runtime selection,
      acceptance evidence, and deterministic state reconstruction.
- [ ] Keep merge/conflict expansion out of minimum proof unless required by
      the minimal world fixture.

### E. Make federation thin

- [ ] Keep only certificate verification, before/after chain checks,
      transition verification, and finality anchoring required for replay.
- [ ] Prove/check finality safety at slot level.
- [ ] Ensure replay reports include verified transition count, final state,
      final epoch, and canonical evidence.

### F. Make retention a preservation statement

- [ ] Define one explicit archive constitution `rho_archive` with one archived
      replay query as target.
- [ ] Prove/test preservation: retained closure can still replay same result.
- [ ] Add one nontrivial retention corpus case with real reclamation.

### G. Exact independent reconstruction

- [~] Rust/Scala canonical verdict parity for current CKC corpus.
- [ ] Extend corpus to full minimal world (change -> accept -> replay -> retain).
- [~] Bind verdict identity over `(K, G, S, evidence, resourceUse)`.
- [ ] Keep Lean as semantic/proof reference and executable companion; do not
      treat extraction alone as independence substitute.

### H. Staircase successor closure

- [x] Define one constituted upgrade/change from the first minimal world
      (`delta0 : G0 -> G1`) under accepted/finalized governance.
- [x] Require both Scala and Rust to reconstruct `G1` to the same canonical
      verdict/state as a second-generation gate.
- [x] Record `G0 -> G1` as a permanent conformance fixture and tutorial step
      in [docs/pr34-staircase-fixture.md](pr34-staircase-fixture.md).

### I. Foundation handoff discipline

- [x] Freeze `Cairn_n` step artifacts and canonical verdicts before upgrades.
- [x] Represent remaining trusted host choices as constituted artifacts.
- [x] Define/extend change languages for those newly constituted choices.
- [x] Construct and finalize `delta_n : Cairn_n -> Cairn_{n+1}`.
- [x] Reconstruct `G_{n+1}` independently in Scala and Rust.
- [x] Promote `S_{n+1}` as `Foundation_{n+2}` inputs.
- [~] Remove obsolete scaffolding from `Cairn_n` after promotion.

## Minimal world fixture

Create one deliberately small but semantically complete fixture and use it as:

- golden artifact set;
- tutorial walkthrough;
- cross-implementation conformance test;
- Lean example;
- release artifact.

Required contents:

1. one minimal language and real change constructor;
2. initial state plus two real changes;
3. acceptance constitution plus authority/evidence for both changes;
4. one repository branch replaying to final state;
5. two federation transitions with one finality certificate each;
6. one retention constitution and retained closure;
7. final reconstructed state and canonical verdicts.
8. one successor-world delta that yields `G1` and reconstructs to `S1` in both
      independent implementations.

## Commit-slice plan (target: 12-18)

1. Freeze canonical `G`/`B` schema and verdict envelope.
2. Route authoritative APIs through one judgment center.
3. Free-change witness strengthening.
4. Deterministic change application hardening + vectors.
5. Constitution-relative acceptance hardening + vectors.
6. Repository replay as CKC query.
7. Thin federation verification envelope cleanup.
8. Finality safety checks and vectors.
9. Retention constitution and preservation query.
10. Retention preservation corpus case.
11. Rust parity expansion for change/accept/repo replay.
12. Canonical verdict identity checks (`Scala == Rust`).
13. Lean theorem+example closure for replay determinism and finality safety.
14. Minimal world fixture publication and golden integration.
15. CI gates for full corpus.
16. Successor-world gate (`G0 -> G1`) in parity corpus and release checks.
17. Promote first load-bearing step into documented `Foundation_{n+1}` inputs.
18. Remove one trusted host decision by re-expressing it as governed artifacts.

## Non-goals until closure

- no new CKC judgment forms unless needed by minimal world closure;
- no loader-side temporary validation authority;
- no Scala-only semantic shortcut path;
- no protocol freeze before full round-trip identity closure;
- no abstraction expansion that does not shorten byte-to-meaning distance.

## Current status snapshot

- [x] CKC parity suite exists and runs real corpus vectors.
- [x] CI has dedicated CKC parity job provisioning Java, Rust, and Lean.
- [x] Canonical PR34 package/verdict envelope schema exists with codec
      round-trip tests (`Pr34GraphPackage`, `Pr34VerdictEnvelope`).
- [x] CKC-to-PR34 verdict interop mapping exists for all verdict classes,
      including replay-state projection and evidence threading.
- [x] Rust scaffold codecs for package/verdict envelopes are present and
      enforced by `cargo test` in CI.
- [x] Lean scaffold schema module for package/verdict envelopes compiles via
      `lake build` and is imported in `VerifierLean`.
- [~] Loader authority contraction in progress; continue pushing semantics into
      derivation boundary with typed missing/exhausted coverage.
- [ ] Full minimal-world fixture still to be assembled and frozen.
- [x] Staircase scaffold exists in Scala/Rust/Lean via
      `Pr34SuccessorLink` + two-step validator checks.
- [x] Cross-language staircase parity checks run via verifier CLIs from
      `CKCParitySuite`, covering valid links, equal-package rejection, and
      predecessor-link mismatch rejection.
- [x] Staircase parity gate enforces malformed-digest rejection parity across
      Rust and Lean (base args and link-override args).
- [x] CKC parity fixture now asserts independent resolve parity for both
      first-generation (`G0`) and second-generation (`G1`) state digests.
- [x] Staircase CLI parity checks now run against real fixture-derived `G0` and
      `G1` digests (not synthetic placeholders).
- [x] Scala staircase validator now runs on the same real fixture `G0 -> G1`
      pair used by Rust/Lean CLI parity checks.
- [x] Current parity fixture rebuild is deterministic within-suite (`G0`,
      `G1`, federation, genesis), enabling repeatable staircase gating.
- [x] Fixture identity key material is pinned as explicit encoded bytes in
      `CKCParitySuite` (not provider-generated), giving cross-environment
      reproducibility for `G0`/`G1` fixture derivation.
- [x] Fixture outputs (`federationId`, `genesisState`, `G0`, `G1`) are pinned
      to explicit golden digests in `CKCParitySuite` and fail closed on drift.
- [x] Cert-binding fixture outputs (`replica manifest`, `cert1`, `cert2`) are
      also pinned to golden digests in `CKCParitySuite`.
- [x] Successor-world fixture (`G1`) is validated as an independent
      reconstruction gate with governed finalized delta.
- [x] Successor-world tutorial/conformance publication is now documented as a
      permanent `G0 -> G1` fixture walkthrough.
- [x] Foundation handoff (`Meaning_n -> Foundation_{n+1}`) now has a first
      concrete promoted step artifact set in `CKCParitySuite` with a pinned
      canonical handoff digest.
- [x] Promoted handoff slab now directly drives staircase successor validation
      in `CKCParitySuite` (Scala staircase + Rust CLI checks consume promoted
      predecessor/successor/delta fields via execution context).
- [~] Legacy synthetic staircase parity path is being reduced in favor of
      slab-driven checks while preserving Rust/Lean malformed-link parity.
- [~] Standalone successor-world fixture audit path has been consolidated into
      promoted-slab staircase gates to reduce duplicate scaffold assertions.
- [x] Slab-driven staircase assertions are now unified under one authoritative
      cross-implementation gate in `CKCParitySuite`.
- [x] The unified slab staircase gate now decodes governed transition commit
      evidence and asserts constitution-bound change-model identity
      (`capabilities.changeModel == constitution.changeModel == evidence.changeModel`).
- [x] Synthetic cert-fixture parity vectors were removed; cert-binding parity
      now runs against governed fixture cert/proposal digests from the
      promoted slab path.
- [x] Standalone pinned-key fixture test was folded into staircase fixture
      reproducibility so identity pinning and digest stability are enforced by
      one gate.
