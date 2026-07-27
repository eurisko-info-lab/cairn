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

### Semantic workspace roadmap

| PR | Capability | Status |
|---|---|---|
| PR11–PR17 | Semantic access traces, pack-declared change/language capabilities, migrations, acceptance constitutions, ΔConflict, and foreign surfaces | ✅ |
| PR18 | SDS Studio v1: generated semantic forms, governed branch workspaces, proposal review, source/report previews, conflict resolution, migration assistance, and evidence inspection | ✅ |
| PR19 | Production SDS JSON/XML/XLSX/report/PDF/image providers, with provenance, canonical round-trip/projection evidence, migration, and foreign-source localization | ✅ |
| PR20 | Durable distributed workspaces: content-addressed drafts, signed review/approval/handoff, restart-safe offline revisions, rebase, and verified replication | ✅ |
| PR21 | Language Studio: atomic language/capability projects edited through ΔMeta and ΔGrammar, with whole-graph revision validation | ✅ |
| PR22 | Proof and Projection Studio: goals, checked derivations/proof terms/certificates, Rosetta output evidence, and Lean/HVM agreement envelopes | ✅ |
| PR23 | Artifact-only application startup: one root digest, recursive dependency installation/audit, language reconstruction, and exact capability/entry checks | ✅ |
| PR24 | Pack and application ecosystem: signed releases, semantic-version discovery, migration routing, ledger publication, recursive replication, and namespace trust policy | ✅ |
| PR25 | Self-hosting, hardening, and TCB contraction: signed two-node successor-release ceremony, artifact-reconstructed Language Studio, classified trusted closure, restart verification, and witnessed optimization equivalence | ✅ |

Studio is the generated interaction surface of a `LanguageCapabilities`
bundle; SDS is its first complete domain inhabitant. It emits ordinary ΔL,
ΔConflict, and migration terms and has no direct module-save path.

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

PR25 closes the foundational architecture programme. New work should now be
driven by operational use and compatibility rather than another universal
abstraction. See [docs/assumptions.md](docs/assumptions.md) for the precise
remaining gaps.

### Revised remaining roadmap

| PR | Capability | Status |
|---|---|---|
| PR26 | Runtime constitution closure | ✅ |
| PR27 | Complete native Pijul-like repository | ✅ |
| PR28 | Contract the remaining host TCB | ⬜ |
| PR29 | Generalized semantic-equivalence evidence | ⬜ |
| PR30 | Distributed transaction and consensus hardening | ⬜ |
| PR31 | Studio productionization | ⬜ |
| PR32 | Eliminate fidelity residuals | ⬜ |

#### PR26 — Runtime constitution closure

Introduce one digest-bound runtime selection:

```scala
final case class DomainRuntime(
  language: Digest,
  capabilities: Digest,
  acceptance: Digest
)
```

Its transitive graph includes language and grammar, change semantics and
surface, validation and providers, migrations, queries, policies, projections,
Studio profiles, and authority/publication requirements. Repository APIs,
validated tips, branch manifests, conflicts, migrations, and acceptance
evidence must identify this constitution. A caller must not be able to combine
a language from one release with another release's change or validation model.

#### PR27 — Complete the native Pijul-like repository

Make causal dependencies native rather than reconstructing them primarily from
branch histories:

- explicit change and semantic-path context dependencies;
- partial application;
- conflicts preserved inside repository state;
- conflict resolutions represented as dependent changes;
- change-centric pull and push;
- branch heads as views rather than history containers;
- garbage collection rooted in the causal change graph.

`Branches` then becomes porcelain over one change graph instead of a second
history mechanism.

Implemented by `NativeRepository`: accepted changes carry explicit causal and
semantic-location context dependencies; unavailable transfers remain pending
until their prerequisites arrive; conflicts and dependent ΔConflict
resolutions live in graph state; pull/push transfers causal artifact closure;
named branches are head-set views; and CAS liveness is derived from graph
roots. `PatchGraph` and ordered histories remain decode/legacy compatibility
adapters, not the authority for runtime-governed branches.

#### PR28 — Contract the remaining host TCB

Reduce the digest-accounted host boundary to a small generic machine:

```text
canonical decoder
grammar interpreter
rule/search interpreter
change-program interpreter
proof checker
effect dispatcher
```

Targets include the Scala Meta/Grammar bootstrap seeds, host `Query.run`,
policy enforcement, `TreeEngine`, `Delta`, and effect-family routing/cold-start
seeds. The goal is not zero native code; it is to move every non-generic choice
into artifacts loaded and checked by those engines.

#### PR29 — Generalized semantic-equivalence evidence

Extend `OptimizationEquivalence` beyond dependency discovery to:

- parser ↔ compiled parser;
- printer ↔ derived printer;
- `TreeEngine` ↔ compiled rewrite machine;
- canonical change interpretation ↔ optimized replay;
- repository merge ↔ incremental merge;
- migration ↔ compiled migration;
- Cairn surface provider ↔ external native provider.

Every agreement binds the semantic model, interpreter implementation, input,
and both result digests. Lean and HVM remain narrow, honestly named agreement
envelopes—not claims of full Lean-kernel or HVM-runtime compatibility.

#### PR30 — Distributed transaction and consensus hardening

Address the current non-atomic and closed-membership boundaries with
transactional publication, authenticated peer discovery, durable consensus
recovery, equivocation evidence, namespace federation, and replicated
garbage-collection safety. The starting point remains explicit: local
acceptance is journaled but not a distributed atomic transaction; discovery is
directory-based; BFT finalizes sealed PoA blocks; replay/revocation state merges
through CAS rather than consensus; and there is no open-membership ledger.

#### PR31 — Studio productionization

Turn the three complete semantic Studio verticals into a daily production
environment:

- incremental document synchronization;
- multi-file and workspace-folder LSP;
- large-document virtualization;
- background validation and indexing;
- an accessible semantic conflict editor;
- user/session identity and permissions;
- deployment, monitoring, and operational observability.

The current LSP remains full-document only.

#### PR32 — Eliminate fidelity residuals

Close the remaining localized fidelity and coverage gaps:

- preserve inserted concrete syntax without parent reprint when no original
  span exists;
- move the remaining `ChemicalDoc` → `Cst` projection out of host glue;
- extend Bend/HVM lowering beyond the narrow agreement corpus.

These are no longer foundational architecture blockers.
