# Transcript sources

CAIRN-PROMPT §7: *Transcripts are CI.* Cairn already ships `mvp` / `max` /
`search-board`. Additional fixtures below were **adapted** from prior projects
cited in CAIRN-PROMPT §13 — not byte-copied (different CLIs / DSLs).

| Cairn transcript | Prior art | Mapping |
| --- | --- | --- |
| `repository-workflow.cairn` | `~/Downloads/granit-rust/examples/transcripts/repository-workflow.transcript` | repo/commit/publish → STLC ΔL + claim + PoA publish/fetch |
| `chain-sync.cairn` | Marble/Charb `chain-sync-workflow.yaml` (+ granit-rust charb-import stub) | chain attach/sync → two nodes + fetch + gossip |
| `pki-surface.cairn` | `~/GRANITE/examples/pki/` (Phase 8) | cert/revocation surface + free ΔPKI; crypto validation stays in Scala suites |
| `law-surface.cairn` | `~/GRANITE/examples/law/` | statute surface + free ΔLaw |
| `sds-surface.cairn` | `~/GRANITE/examples/sds/` | thin SDS object surface + free ΔSDS |

**Not ported yet** (wrong shape for today’s transcript DSL, or already covered
by host tutorials / Scala suites):

- Charb’s ~85 YAML workflows (`~/Projects/all-git-repos/pi-forall/charb/transcripts/`)
  — CLI/registry/economics; granit-rust imported many as `expect rejected` stubs
- granit-rust `ledger-settlement` (token balances / task commit-reveal)
- Full SDS causal + domain trunk (`SdsCausalWorkflow`) — needs `forkFrom` steps
  in the transcript language before it can leave Scala
