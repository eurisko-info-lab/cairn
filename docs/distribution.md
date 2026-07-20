# Distribution

What exists (§6 Phase 6) and what is deliberately deferred.

## Implemented

- **Pull-based blob sync** (`ledger.Sync.pull`): a consumer node fetches the
  producer's blocks, replays them through the pure `LedgerKernel` (verifying seals,
  parents, state roots) **before adopting anything**, then materializes missing
  published artifact bodies from the producer's CAS by digest into its own CAS.
- **Divergence surfacing** (`Sync.compare`): two chains are `Same`, `Ahead`,
  `Behind`, or `Diverged(atHeight, headA, headB)`. Competing heads are reported as
  competing heads; there is no auto-merge and no silent overwrite (§8).

## Deferred (design notes, no fake stubs)

- **Gossip**: nodes currently sync point-to-point by explicit pull. A gossip layer
  would flood block digests; bodies still travel by CAS fetch, so the protocol
  stays "hashes first, blobs on demand".
- **Fork choice**: PoA with a single authority makes forks operator errors; with
  multiple authorities, longest-valid-chain-per-authority-set is the intended rule.
  `Sync.compare` already exposes the fork point a fork-choice rule would consume.
- **BFT finality**: out of scope until multi-authority PoA exists (§6 Phase 5/6).
- **Useful-work market hooks**: not designed beyond the observation that
  `RecordCertificate` txs are the natural anchor for work receipts.
