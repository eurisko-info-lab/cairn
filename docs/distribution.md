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
  over Ed25519 with `signer == msg.from`, designated primary per view, and a
  certificate that requires **2f+1 distinct** known replicas. The certificate
  binds `replicaSet` (full sealed-manifest body digest: ids **and** public keys,
  plus transition metadata), block height, and parent, and is only minted for a
  **replay-valid sealed PoA block** on the local chain. Height/parent are
  re-checked via `FinalityCertificate.verifyAgainstChain`.
  Seal collection is keyed by `(view, seq, digest, replicaId)` so peer commits
  do not overwrite each other. HTTP: `POST /bft/msg` + `GET /bft/certs` on
  `cairn serve replica <name> [port]`. Network CLI: `cairn bft agree <hex>`
  against registered replicas; lab: `cairn bft agree local <hex>`.
- **Replica-set governance**: sealed `replica-set.canon` plus durable history in
  `replica-set-history.canon`. Amendments require `replaces`, a strictly higher
  `activationHeight`, and a **predecessor quorum** of approvals from the old set.
  `ReplicaSetManifest.activeAt(history, height)` resolves the live set;
  predecessors deactivate when a successor activates. Pre-activation and
  post-deactivation replicas refuse to operate.
- **Durable vote persistence**: `bft-state.canon` / certs use
  `DurableIo.writeConsensus` (temp + fsync + atomic rename; no non-atomic
  fallback). State is persisted **before** outbound signatures are exposed; any
  write failure enters a permanent fail-closed state.
- **Keystore** (`Keystore` / `Signer`): private keys under
  `$CAIRN_HOME/replicas/<name>.canon` as `keypair-sealed` (AES-GCM via
  `CAIRN_KEYSTORE_SECRET`). Create-only (never overwrite on decrypt/decode
  failure). Plaintext refused unless `CAIRN_KEYSTORE_PLAINTEXT=1` (lab only).
- **Divergence surfacing** (`Sync.compare`): `Same / Ahead / Behind /
  Diverged(atHeight, headA, headB)`.
- **Light clients** (M35): Merkle inclusion proofs verify "published" and
  "head" membership against a state root without the full state.

## Honesty bounds

| Capability | Bound |
| --- | --- |
| Peer discovery | Directory / announce — not open DHT or Sybil-resistant membership |
| Gossip daemon | Pull-based fork choice; no push epidemic or anti-entropy beyond want/have |
| BFT finality | Classic `n=3f+1` only; certifies replay-valid sealed PoA blocks. Authority is a sealed `replica-set.canon` (+ history) of public keys — not unsigned peer gossip. Amendments need predecessor quorum + activation height. Each process holds only its own private key behind `Keystore`; proposers use `POST /bft/propose`. Durable slot/view state in `bft-state.canon` (fail-closed on I/O errors). |
| Domain governance | Owner + grantor seals resolved via `IdentityResolver`. Language digests are mandatory (`PackAccess.loadClosed` or explicit index); `Branches.auditGoverned` walks the sealed agreement/delegation graph. |
| Keystore | Encrypted at rest by default (`CAIRN_KEYSTORE_SECRET`). Create-only paths; wrong secret never replaces identity files. |
| Useful-work market | Still deferred; `RecordCertificate` remains the natural anchor |

## Operational layers

1. **Content distribution** — HTTP blobs + replay-checked chain pull (real)
2. **Convergence** — peer directory + HTTP gossip + deterministic fork choice (functional alpha)
3. **Finality** — quorum certificates over sealed blocks (repaired prototype; still a static replica set)

## CLI cheat sheet

```bash
export CAIRN_KEYSTORE_SECRET='…'          # required for durable replica keys
# or: export CAIRN_KEYSTORE_PLAINTEXT=1   # lab only
./bin/cairn serve 8743
./bin/cairn bft replica-set init r0 r1 r2 r3       # write replica-set.canon + history + local keys
./bin/cairn serve replica r0 8743   # installs /bft/msg + /bft/propose + /bft/certs
./bin/cairn peer add alice http://127.0.0.1:8743
./bin/cairn peer add r1 http://127.0.0.1:8744 replica
./bin/cairn peer discover http://127.0.0.1:8743
./bin/cairn gossip once
./bin/cairn gossip run 5
./bin/cairn bft agree <64-hex-block-digest>        # asks primary /bft/propose (no remote sk)
./bin/cairn bft agree local <64-hex-block-digest>  # in-process lab
./bin/cairn smoke distribution                     # packaged two-node gossip + four-node BFT
./bin/cairn pull http://127.0.0.1:8743
```
