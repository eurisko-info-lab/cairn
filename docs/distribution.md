# Distribution

What exists for multi-node sync, gossip, discovery, and BFT finality.

## Implemented

- **Pull-based blob sync** (`systemhandler.Sync.pull`): consumer replays the producer's
  blocks through the pure `LedgerKernel` (seals, parents, state roots) BEFORE
  adopting anything, then materializes missing published bodies by digest.
  Chain-file adoption writes through `Filesystem` on the consumer's
  `EffectContext.forLedger` (same gate as Node append).
- **HTTP node surface** (`systemhandler.HttpSync`, M38): `/chain`,
  `/blob/{hex}`, `/heads`; pulls negotiate want/have digest sets, verify every
  byte on arrival (content-addressed re-hash + kernel replay), write the chain
  via `Filesystem`, and RESUME safely — interruption loses nothing because
  every step is an idempotent CAS write (second pull fetches zero).
  **CLI:** `cairn serve [port]` and `cairn pull <baseUrl>` / `cairn fetch-hash
  <baseUrl> <digest>` expose the same transport outside `sbt` (via `bin/cairn`).
- **Peer discovery** (`PeerRegistry` + `GET/POST /peers`): directory-based
  (not DHT). Operators plant peers or nodes announce; `cairn peer discover
  <url>` merges a remote directory. Gossip peers vs BFT replicas are tagged
  (`role=gossip|replica`). Persisted as `$CAIRN_HOME/peers.canon`.
- **HTTP gossip daemon** (`HttpGossip` / `GossipDaemon`): the M39 fork-choice
  rule (longer chain; tip-digest tie-break) over `HttpSync` on a timer.
  CLI: `cairn gossip once`, `cairn gossip run N`. In-process `Gossip.converge`
  remains for transcripts.
- **BFT finality certificates** (`BftFinality`): signed PrePrepare/Prepare/Commit
  over Ed25519, `2f+1` quorum, minting a `FinalityCertificate` for an
  already-sealed PoA block digest. Local agreement is `BftFinality.agreeLocal`;
  HTTP transport is `POST /bft/msg` + `GET /bft/certs` on replica nodes.
  CLI smoke: `cairn bft agree <block-digest-hex>`.
- **Divergence surfacing** (`Sync.compare`): `Same / Ahead / Behind /
  Diverged(atHeight, headA, headB)`.
- **Light clients** (M35): Merkle inclusion proofs verify "published" and
  "head" membership against a state root without the full state.

## Honesty bounds

| Capability | Bound |
| --- | --- |
| Peer discovery | Directory / announce — not open DHT or Sybil-resistant membership |
| Gossip daemon | Pull-based fork choice; no push epidemic or anti-entropy beyond want/have |
| BFT finality | Static authenticated replica set (`f < n/3`); certifies PoA block digests — does not replace M36 round-robin sealing |
| Useful-work market | Still deferred; `RecordCertificate` remains the natural anchor |

## CLI cheat sheet

```bash
./bin/cairn serve 8743
./bin/cairn peer add alice http://127.0.0.1:8743
./bin/cairn peer discover http://127.0.0.1:8743
./bin/cairn gossip once
./bin/cairn gossip run 5
./bin/cairn bft agree <64-hex-block-digest>
./bin/cairn pull http://127.0.0.1:8743
```
