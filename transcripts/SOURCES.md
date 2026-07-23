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

Every `*-workflow.yaml` under
`~/Projects/all-git-repos/pi-forall/charb/transcripts/` has a Cairn port:

| Kind | Count | Mechanism |
| --- | --- | --- |
| Rich / thin runnable | ~44 | publish/fetch, gossip, PKI, `expectfail` |
| **Porcelain-promoted** | ~28 | `porcelain THEME ;` → `Plumbing.charbTheme` ([porcelain.md](../docs/porcelain.md)) |
| Still `deferred` | ~13 | §8 out of scope or no plumbing yet |

Regenerate: `python3 scripts/gen-charb-transcripts.py`

## granit-rust (`transcripts/granit-rust/`)

| File | Status |
| --- | --- |
| `repository-workflow.cairn` | thin runnable (rich sibling at `../repository-workflow.cairn`) |
| `ledger-settlement.cairn` | `deferred` — tokenomics out of Cairn PoA scope (§8) |

## Still host/Scala

Full SDS causal + domain trunk planting in transcripts — `forkFrom` porcelain
step is the next promotion target (`integration-cross-namespace` already shows
domain ancestry via `domain show`).
