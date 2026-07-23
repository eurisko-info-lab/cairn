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

- **Runnable thin mapping** (~44): publish/fetch, gossip, PKI surface, or
  `expectfail` ΔL — closest Cairn theme, not a Marble CLI clone.
- **`deferred "…"`** (~41): honest coverage for Marble-only CLI
  (authorization roles, tokenomics, registry SBOM, light-client phases, …).
  Same idea as granit-rust’s `expect rejected` stubs, but succeeds with an
  explicit deferral log line.

Regenerate: `python3 scripts/gen-charb-transcripts.py`

## granit-rust (`transcripts/granit-rust/`)

| File | Status |
| --- | --- |
| `repository-workflow.cairn` | thin runnable (rich sibling at `../repository-workflow.cairn`) |
| `ledger-settlement.cairn` | `deferred` — tokenomics out of Cairn PoA scope (§8) |

## Still host/Scala (not transcript DSL)

Full SDS causal + domain trunk (`SdsDomainTree` / `SdsCausalWorkflow`) — needs
`forkFrom` transcript steps before it can leave Scala.
