# Distribution

What exists after PLAN-2 (M38–M39) and what remains deferred.

## Implemented

- **Pull-based blob sync** (`ledger.Sync.pull`): consumer replays the producer's
  blocks through the pure `LedgerKernel` (seals, parents, state roots) BEFORE
  adopting anything, then materializes missing published bodies by digest.
  Chain-file adoption writes through `Filesystem` on the consumer's
  `EffectContext.forLedger` (same gate as Node append).
- **HTTP node surface** (`ledger.HttpNode` / `HttpSync`, M38): `/chain`,
  `/blob/{hex}`, `/heads`; pulls negotiate want/have digest sets, verify every
  byte on arrival (content-addressed re-hash + kernel replay), write the chain
  via `Filesystem`, and RESUME safely — interruption loses nothing because
  every step is an idempotent CAS write (second pull fetches zero).
- **Gossip + fork choice** (`ledger.Gossip`, M39): round-based digest gossip
  over real node stores; rule = longest valid chain, ties break on smallest
  head digest; switching chains emits an EXPLICIT `Reorg(node, from, to,
  forkPoint)` event — never a silent merge. Three-node convergence and a
  forked-head reorg are asserted in `WaveGSuite`.
- **Divergence surfacing** (`Sync.compare`): `Same / Ahead / Behind /
  Diverged(atHeight, headA, headB)`.
- **Light clients** (M35): Merkle inclusion proofs verify "published" and
  "head" membership against a state root without the full state.

## Deferred (design notes, no fake stubs)

- **Network gossip daemon**: `Gossip` is an in-process simulation over real
  stores; a daemon would run the same rounds over `HttpSync` on a timer.
- **BFT finality**: multi-authority PoA with quorum governance and rotation
  exists (M36); Byzantine agreement between authorities does not.
- **Peer discovery** and **useful-work market hooks**: unchanged —
  `RecordCertificate` remains the natural anchor for work receipts.
