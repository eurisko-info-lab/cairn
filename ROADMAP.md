# Roadmap

Cairn's phases are defined in [CAIRN-PROMPT.md §6](CAIRN-PROMPT.md#6-phased-roadmap).
This page maps each phase to its current state in `main`. For the
authoritative, continuously-updated module/trust/effect-boundary picture
(including honest deviations from the constitution), see
[docs/architecture.md](docs/architecture.md) and
[docs/assumptions.md](docs/assumptions.md) — this file is a status summary,
not a second source of truth.

Status legend: ✅ done and exercised by tests/transcripts · 🔶 present but
thinner than the phase's full ambition · ⬜ not started.

| Phase | Scope | Status | Notes |
|---|---|---|---|
| 0 — Skeleton | Repo layout, CAS on disk, digest/key model, empty Fragment IR, round-trip + golden digests | ✅ | Superseded by a much larger live module graph (see [docs/architecture.md](docs/architecture.md)) than the phase originally sketched. |
| 1 — MVP Language Workbench | Fragment IR, pushout composition, bidirectional grammar, tree evaluator, ΔL (add/replace/rename), branch manifests | ✅ | ΔL has grown well past add/replace/rename — see Phase 7 and PR9–12 below. |
| 2 — Proof slot + certificates | Claim/Theorem/ProofTerm/TestSuite/Certificate kinds, independent proof checker, claim+tests path without proofs | ✅ | `content/kernel-rewrite/Checker.scala`; STLC typing derivations check as proof terms; forged proofs rejected. |
| 3 — Graph / Δ-net computation | Graph-mode sorts, agent/port/rule vocabulary, AffineNet (no replicator constructible), lowering story | ✅ | `content/user/affinenet/`, `content/core` interaction-net engine; net reduction + well-formedness suites green. |
| 4 — Rosetta ports | Declaration vocabulary, ≥2 emitted ports (Lean 4 + one more), ports consume artifacts without forking semantics | ✅ | `app/rosetta/`, `docs/rosetta.md`; Lean 4, Scala, Haskell, and Rust ports are emitted and byte-fixpoint tested; Scala is host-run in CI, Haskell/Rust host-run locally when `runghc`/`cargo` are available (skipped in CI, which doesn't install those toolchains). |
| 5 — Ledger-backed publication (single-node PoA) | Tx language, identities/signatures, atomic ledger transition, PoA blocks, branch-head publication | ✅ | `container/system-handler/Node.scala`, `container/ledger-types/`; MVP transcript publishes and a second process fetches by digest. |
| 6 — Distribution hooks | Fetch-by-hash, pull-based sync, gossip/fork-choice/BFT documented as future | ✅ (exceeded) | Went well past "design + thin impl": real `GossipDaemon`/`HttpGossip`, peer registry, and a working BFT view-change protocol (`BftQuorum`, `BftFinality`) with a multi-home replica-set ceremony — see `docs/distribution.md`. |
| 7 — Hardening | Rule compilation (optional perf path), kernel/app digest-agreement checks, self-description bootstrap | ✅ | Meta-language bootstrap fixpoint (`content/languages/meta.cairn`), ΔL operations and validation specs are now pack-declared data rather than Scala switches (PR5–6, PR9, PR12), golden digests as a standing regression gate, 100k fuzz corpus. Threaded bytecode/decision-tree compilation for rules was not pursued (optional in the phase spec). |
| 8 — Exemplar packs | PKI, SDS, Bend, Unison Core, MiniTT, LeanCore | 🔶 | PKI (issue/revoke ΔL + chain validation + ledger trust anchor), SDS (substance/shadow/phrase/ΔSDS override), MiniTT, LeanCore, and Unison Core are real fragment packs with judgment/reduction engines behind them. **Bend remains an envelope** (~100 lines, structural placeholder) — the constitution explicitly gates a full implementation on interaction-net lowering being real for it, which hasn't been prioritized. |

## Beyond the original phases

Recent hardening work (not itself a numbered phase, but load-bearing for
Phase 7's spirit of "kill ambient globals, make domain logic data not code")
has been going pack-by-pack and mechanism-by-mechanism:

- **ChangeModel as data** — ΔL's operation vocabulary (add/replace/remove/
  edit/rename, plus custom operations like `copy`) is fully described by
  `ChangeModel`/`ChangeOpDef` data, not a hand-written interpreter switch.
- **ValidationModel with provider identity** — SDS's structural validation
  rules are pack-declared (`content/languages/sds.cairn`), with judgment
  references bound to the exact provider language's digest, not a bare name.
- **Model-consistent repository algebra** — `SemanticRepository`/`Branches`
  merge and migration paths thread an explicit `ChangeModel` end to end
  (one-sided, two-sided, fast-forward, and cross-language-migration merges),
  closing a gap the original PoA-era code left at an implicit default.
- **Semantic access-trace conflict detection** — `Merge.threeWay` conflict
  detection moved from name-based footprints to a real read/write access
  trace over `SemanticLocation` (binding / whole-definition / subtree),
  so sibling edits to one definition merge cleanly while a read genuinely
  racing a write is still caught.
- **Pack-declared change capability** — the standard ΔL operations
  themselves are now authored as `.cairn` pack data
  (`content/languages/change-standard.cairn`), with semantic identity
  (program/footprint/inverse) split from cosmetic surface identity
  (params/print syntax) so a spelling change can never perturb replay
  identity.

## What's next

See [docs/assumptions.md](docs/assumptions.md) for the fuller list of
documented gaps. The post-migration priority order (roughly
dependency-ordered, chosen deliberately in that order):

1. **Effect interfaces become Meta-defined** — typed rights/resource
   vocabularies generated from a Cairn language, not hardcoded Kernel enums.
2. **Replace ambient globals** (`PackAccess`, `AuthorityGate`) with explicit
   runtime contexts and injected capabilities — no more implicit singletons.
3. **Make all privileged handler paths capability-gated**; remove/quarantine
   raw convenience entry points that bypass the gate.
4. **Complete the authority calculus**: conditions, attenuation proofs,
   delegation, expiry, nonces, replay protection, canonical artifacts.
5. **Unify changes/commutation/merge/conflicts/branches** into one native
   Pijul-like repository path — ΔL, ValidationModel, and ChangeCapability are
   each independently pack-declared now, but not yet consolidated into a
   single `DomainRuntime`-style identity, which was deliberately deferred
   while those pieces were still individually moving.
6. **Define precise agreement relations for Lean and HVM** before calling
   their Cairn calculi "executable reference models" — currently more
   aspirational than checked.
7. **Docs pass last** — rewrite README/constitution sections around the
   settled end state once 1–4 land, rather than describing an intermediate
   one. This file and `docs/architecture.md` are kept current as work lands
   in the meantime.

Bend remaining an envelope (Phase 8) is a separate, lower-priority item: the
constitution itself gates a full implementation on interaction-net lowering
being judged real, which hasn't been prioritized against the list above.
