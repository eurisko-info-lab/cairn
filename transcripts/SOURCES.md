# Transcript sources

CAIRN-PROMPT §7: *Transcripts are CI.*

## Native / rich adaptations (`transcripts/*.cairn`)

| Cairn transcript | Prior art | Mapping |
| --- | --- | --- |
| `mvp.cairn` / `max.cairn` | CAIRN-PROMPT phases | STLC eval/ΔL/claim/publish/fetch/gossip/ports |
| `search-board.cairn` | Search exemplar | Fact–Intent board on CAS + ledger |
| `repository-workflow.cairn` | granit-rust `repository-workflow.transcript` | repo/commit/publish → STLC ΔL + PoA |
| `chain-sync.cairn` | Charb `chain-sync-workflow.yaml` | attach/sync → two nodes + fetch + gossip |
| `chain-divergence.cairn` | Charb `chain-divergence-workflow.yaml` | fan-out fetch + gossip |
| `e2e-path.cairn` | Charb `e2e-path-workflow.yaml` | lifecycle → eval/ΔL/claim/publish/fetch |
| `patch-conflict.cairn` | Charb `patch-conflict-merge-workflow.yaml` | `expectfail` on ΔL |
| `multi-language.cairn` | Charb language-composition / matrix | load several packs + surfaces |
| `pki` / `law` / `sds-surface.cairn` | GRANITE Phase 8 | domain surfaces + free ΔL |
| `minitt` / `leancore` / `unisoncore-surface.cairn` | CAIRN-PROMPT §2c | hosted-language surfaces |

## Full Charb suite (`transcripts/charb/` — **85**/85)

Every `*-workflow.yaml` under a Charb transcripts checkout (pass `--source`)
has a Cairn port. Counts below are **generated** from
`transcripts/charb/dispositions.tsv` — do not hand-edit.

| Kind | Count | Mechanism |
| --- | --- | --- |
| Rich / thin runnable | 44 | publish/fetch, gossip, PKI, `expectfail` |
| **Porcelain-promoted** | 36 | `porcelain THEME ;` → `Plumbing.charbTheme` ([porcelain.md](../docs/porcelain.md)) |
| Still `deferred` | 5 | §8 out of scope or no plumbing yet |

Deferred-theme prioritization and promotion paths are tracked in
[docs/charb-gap-report.md](../docs/charb-gap-report.md).

Regenerate: `python3 scripts/gen-charb-transcripts.py --source DIR`
(or `--pin-only` to refresh dispositions while preserving source hashes;
`--update-docs` syncs this section). Pinned source revision: `f1c8dbdf387595ae5505e8ef0090c05fd595aa70`.

## granit-rust (`transcripts/granit-rust/`)

| File | Status |
| --- | --- |
| `repository-workflow.cairn` | thin runnable (rich sibling at `../repository-workflow.cairn`) |
| `ledger-settlement.cairn` | `deferred` — tokenomics out of Cairn PoA scope (§8) |

## Still host/Scala

SDS causal workflow (commitTip / merge / certificates) remains partly
Scala-hosted in `SdsCausalWorkflow`, but **domain trunk planting** is now a
transcript: `transcripts/sds-domain-journey.cairn` uses `fork-from` / `refer`.

Charb import identity is pinned in `transcripts/charb/SOURCE.rev` (Git revision)
and per-workflow SHA-256 columns in `dispositions.tsv` / transcript headers.
Exact dispositions are CI-enforced (no mere count thresholds).
