# Transcript sources

CAIRN-PROMPT §7: *Transcripts are CI.* Cairn already ships `mvp` / `max` /
`search-board`. Additional fixtures below were **adapted** from prior projects
cited in CAIRN-PROMPT §13 — not byte-copied (different CLIs / DSLs).

| Cairn transcript | Prior art | Mapping |
| --- | --- | --- |
| `repository-workflow.cairn` | `~/Downloads/granit-rust/examples/transcripts/repository-workflow.transcript` | repo/commit/publish → STLC ΔL + claim + PoA publish/fetch |
| `chain-sync.cairn` | Marble/Charb `chain-sync-workflow.yaml` (+ granit-rust charb-import stub) | chain attach/sync → two nodes + fetch + gossip |
| `chain-divergence.cairn` | Marble/Charb `chain-divergence-workflow.yaml` | divergence detection → fan-out fetch + gossip |
| `e2e-path.cairn` | Marble/Charb `e2e-path-workflow.yaml` | one-shot lifecycle → STLC eval/ΔL/claim/publish/fetch |
| `patch-conflict.cairn` | Marble/Charb `patch-conflict-merge-workflow.yaml` | dirty merge → structured `expectfail` on ΔL |
| `multi-language.cairn` | Marble/Charb `language-composition` / `multi-language-matrix` | pushout matrix → load several packs + surface checks |
| `pki-surface.cairn` | `~/GRANITE/examples/pki/` (Phase 8) | cert/revocation surface + free ΔPKI; crypto validation stays in Scala suites |
| `law-surface.cairn` | `~/GRANITE/examples/law/` | statute surface + free ΔLaw |
| `sds-surface.cairn` | `~/GRANITE/examples/sds/` | thin SDS object surface + free ΔSDS |
| `minitt-surface.cairn` | CAIRN-PROMPT §2c MiniTT | dependent Nat surface + free Δ |
| `leancore-surface.cairn` | CAIRN-PROMPT §2c LeanCore | Eq/refl surface + free Δ |
| `unisoncore-surface.cairn` | CAIRN-PROMPT §2c Unison Core | ADT/match surface + free Δ |

**Not ported yet** (wrong shape for today’s transcript DSL, or already covered
by host tutorials / Scala suites):

- Most of Charb’s ~85 YAML workflows (`authorization`, tokenomics, registry
  SBOM, …) — CLI/governance economics; granit-rust imported many as
  `expect rejected` stubs
- granit-rust `ledger-settlement` (token balances / task commit-reveal)
- Full SDS causal + domain trunk (`SdsCausalWorkflow`) — needs `forkFrom` steps
  in the transcript language before it can leave Scala
