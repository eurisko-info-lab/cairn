# Charb parity gap report (deferred themes)

Source of truth:
- Deferred list from transcripts/charb/dispositions.tsv
- Existing mapping and rationale from docs/porcelain.md and app/surface plumbing

Date: 2026-07-29

## Purpose

The Charb transcript import is a parity harness, not a clone project.
Each imported workflow is forced into one of three states:
- runnable in Cairn
- mapped through Cairn porcelain/plumbing
- deferred (missing concept or engine)

This report ranks only the deferred set and gives the shortest realistic path to promote each item.

## Scope decision: useful features only

Pursue only the deferred themes that strengthen Cairn's current value proposition
(governance, auditability, replay-verifiable operations, and supply-chain evidence):

1. network-mempool-phase3 (completed: promoted to porcelain on 2026-07-29)
2. compliance-registry (completed: promoted to porcelain on 2026-07-29)
3. deps-lock-evidence-registry (completed: promoted to porcelain on 2026-07-29)

All other deferred themes remain explicitly frozen as non-goals for this cycle.

## Priority implementation set

### 1. network-mempool-phase3
- Why this tier: transport already exists (HttpNode, Gossip); missing part is pending transaction pool semantics.
- Minimum engine needed:
  - new mempool state model and admission rules
  - duplicate/replay prevention and eviction policy
  - query/list surface for transcript assertions
- Promotion path:
  - add mempool model in runtime/container path
  - expose plumbing readout (for transcript checks)
  - convert deferred transcript to runnable/porcelain using real pending-tx evidence
- Expected risk: medium, mostly state-model and consistency concerns.
- Acceptance criteria:
  - pending transactions become queryable before append
  - duplicate/replay candidates are rejected deterministically
  - transcript theme `network-mempool-phase3` moves from `deferred` to `porcelain` or `runnable`

### 2. compliance-registry
- Why this tier: mostly data-model plus checks; can reuse existing ledger/cas evidence shapes.
- Minimum engine needed:
  - compliance object kind/schema (policy/check/attestation lineage)
  - mutation and query API for registry entries
  - drift/status computation over current branch or release root
- Promotion path:
  - define canonical registry artifact(s)
  - implement read/write plumbing backed by existing branch/ledger history
  - port transcript from deferred to runnable with concrete registry assertions
- Expected risk: medium, concentrated in schema and lifecycle choices.
- Acceptance criteria:
  - compliance entries are canonical artifacts with lineage
  - drift/status checks run from current branch or release root
  - transcript theme `compliance-registry` moves from `deferred` to `porcelain` or `runnable`

### 3. deps-lock-evidence-registry
- Why this tier: provenance exists, but package-dependency/lockfile model is absent; this is tractable and bounded.
- Minimum engine needed:
  - dependency manifest + lock artifact schema
  - digest binding from build/deploy artifacts to lock evidence
  - verification command for lock conformance
- Promotion path:
  - add dependency/lock evidence artifacts and validators
  - extend provenance/plumbing join to include lock evidence
  - promote transcript once real lock checks are executable
- Expected risk: medium, mostly schema + verification logic.
- Acceptance criteria:
  - dependency/lock evidence is digest-bound to build or release artifacts
  - lock conformance verification is executable and deterministic
  - transcript theme `deps-lock-evidence-registry` moves from `deferred` to `porcelain` or `runnable`

## Frozen deferred themes (not in this implementation cycle)

1. chain-work-scan
2. chain-work-adjudication
3. chain-work-reward
4. stake-registry
5. consensus-economics-phase1

Reason: these introduce incentive-economics/work-market scope that is not required
for current Cairn priorities and would materially expand trusted-state semantics.

## Suggested execution order

1. network-mempool-phase3
2. compliance-registry
3. deps-lock-evidence-registry

## Promotion gate for each deferred theme

A deferred theme should move to porcelain or runnable only when all are true:
1. A real engine/model exists (not text-only stub).
2. A transcript executes that model and asserts outcome evidence.
3. The theme is listed in promoted mappings and removed from deferred disposition.
4. CI enforces the new disposition and transcript behavior.

## Existing references

- Deferred rationale and promoted theme mechanics: docs/porcelain.md
- Imported-source mapping and counts: transcripts/SOURCES.md
- Canonical disposition ledger: transcripts/charb/dispositions.tsv
- Current plumbing mappings: app/surface/src/main/scala/cairn/surface/Plumbing.scala
- Current promoted theme set: app/surface/src/main/scala/cairn/surface/Porcelain.scala

Execution plan for this narrowed scope:
- docs/charb-useful-features-plan.md
