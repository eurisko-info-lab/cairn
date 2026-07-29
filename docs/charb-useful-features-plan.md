# Charb useful-features implementation plan

Date: 2026-07-29
Scope: only deferred themes that are directly useful in Cairn now.

## In-scope

1. network-mempool-phase3 (completed: promoted to porcelain on 2026-07-29)
2. compliance-registry (completed: promoted to porcelain on 2026-07-29)
3. deps-lock-evidence-registry (completed: promoted to porcelain on 2026-07-29)

## Out-of-scope (frozen for this cycle)

1. chain-work-scan
2. chain-work-adjudication
3. chain-work-reward
4. stake-registry
5. consensus-economics-phase1

## Work item A: network-mempool-phase3

Status: completed (promoted from deferred to porcelain)

Objective:
- Add pending-transaction visibility and deterministic admission/rejection before append.

Deliverables:
- Mempool state model and admission policy.
- Duplicate/replay and eviction behavior.
- Plumbing/porcelain exposure for transcript checks.
- Transcript promotion from deferred.

Transcript acceptance:
- `transcripts/charb/network-mempool-phase3.cairn` must execute without `deferred`.
- Transcript includes explicit assertions (for example via expect-summary) showing:
  - pending entries visible
  - duplicate/replay candidate rejected
  - pool reflects post-append state transition

## Work item B: compliance-registry

Status: completed (promoted from deferred to porcelain)

Objective:
- Add compliance registry artifacts and status computation tied to branch or release roots.

Deliverables:
- Canonical compliance artifact schema (policy/check/attestation lineage).
- Registry update/query mechanics.
- Drift/status check output surface.
- Transcript promotion from deferred.

Transcript acceptance:
- `transcripts/charb/compliance-registry.cairn` must execute without `deferred`.
- Transcript asserts registry write/read and drift-status output.

## Work item C: deps-lock-evidence-registry

Status: completed (promoted from deferred to porcelain)

Objective:
- Add dependency/lock evidence artifacts bound to produced build/release artifacts.

Deliverables:
- Dependency manifest + lock evidence artifact schema.
- Digest binding and lock conformance verification.
- Registry-style query surface.
- Transcript promotion from deferred.

Transcript acceptance:
- `transcripts/charb/deps-lock-evidence-registry.cairn` must execute without `deferred`.
- Transcript asserts lock evidence registration and verification result.

## Promotion protocol

For each in-scope theme, promotion is complete only when all hold:
1. Real engine/model exists (no placeholder output).
2. Transcript runs and asserts the model behavior.
3. `transcripts/charb/dispositions.tsv` changes from `deferred` to `porcelain` or `runnable`.
4. CI includes and enforces that promoted disposition.
