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
has a Cairn port:

| Kind | Count | Mechanism |
| --- | --- | --- |
| Rich / thin runnable | ~44 | publish/fetch, gossip, PKI, `expectfail` |
| **Porcelain-promoted** | ~28 | `porcelain THEME ;` → `Plumbing.charbTheme` ([porcelain.md](../docs/porcelain.md)) |
| Still `deferred` | ~13 | §8 out of scope or no plumbing yet |

Regenerate: `python3 scripts/gen-charb-transcripts.py --source DIR`
(or `--pin-only` to refresh `dispositions.tsv` from existing ports).
Pinned dispositions: `transcripts/charb/dispositions.tsv`.

## granit-rust (`transcripts/granit-rust/`)

| File | Status |
| --- | --- |
| `repository-workflow.cairn` | thin runnable (rich sibling at `../repository-workflow.cairn`) |
| `ledger-settlement.cairn` | `deferred` — tokenomics out of Cairn PoA scope (§8) |

## Still host/Scala

SDS causal workflow (commitTip / merge / certificates) remains partly
Scala-hosted in `SdsCausalWorkflow`, but **domain trunk planting** is now a
transcript: `transcripts/sds-domain-journey.cairn` uses `fork-from` / `refer`.
Regenerate Charb ports with an explicit source dir:

```bash
python3 scripts/gen-charb-transcripts.py --source /path/to/charb/transcripts
# or refresh pinned dispositions only:
python3 scripts/gen-charb-transcripts.py --pin-only
```

Exact per-workflow disposition lives in `transcripts/charb/dispositions.tsv`
(CI asserts exact match, not mere count thresholds).
