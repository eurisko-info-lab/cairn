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
  `replica-set-history.canon`. Load paths replay-verify genesis + adjacent
  transitions (`ValidatedReplicaSetHistory`). Amendments require `replaces`, a
  strictly higher `activationHeight`, and a **predecessor quorum** of approvals
  from the old set. `activeAt(height)` resolves the live set; predecessors
  deactivate when a successor activates. Every propose/receive/sign and
  certificate verify checks membership at the block height. Packaged
  `serve replica` loads history from `$CAIRN_HOME`.
- **Membership ceremony** (`BftCeremony` / `cairn bft replica-set …`): per-machine
  `keygen`, pubkey export/import, draft assemble, member `seal`, predecessor
  `approve`, `finalize`, and bundle `export`/`install` — private keys never leave
  their home. `init` remains a single-home lab shortcut.
- **Continuous finality**: BFT `seq` is the sealed block height (not a fixed
  `0`). Replicas persist a finalized high-water mark so consecutive blocks
  advance through distinct slots on durable processes.
- **Durable vote persistence**: `bft-state.canon` / certs use
  `DurableIo.writeConsensus` (temp + fsync + atomic rename; no non-atomic
  fallback). Parent-directory fsync is best-effort on filesystems that refuse
  directory opens. State is persisted **before** outbound signatures are
  exposed; any write failure enters a permanent fail-closed state.
- **Keystore** (`Keystore` / `Signer`): private keys under
  `$CAIRN_HOME/replicas/<name>.canon` as `keypair-sealed` (AES-GCM; key =
  SHA-256 of `CAIRN_KEYSTORE_SECRET` — suitable for machine secrets, not a
  salted password KDF). Create-only (never overwrite on decrypt/decode
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
| BFT finality | Classic `n=3f+1` only; certifies replay-valid sealed PoA blocks. Authority is a sealed `replica-set.canon` (+ replay-verified history) of public keys — not unsigned peer gossip. Amendments need predecessor quorum + activation height. Active-set checked on propose/receive/sign, mint, and independent verify. `seq` = block height for continuous finality. Each process holds only its own private key behind `Keystore`; proposers use `POST /bft/propose`. Durable slot/view state in `bft-state.canon` (fail-closed on I/O errors). |
| Domain governance | Owner + grantor seals resolved via `IdentityResolver`. Language digests are mandatory (`PackAccess.loadClosed` or explicit index); `Branches.auditGoverned` walks the sealed agreement/delegation graph and asserts manifest≡agreement ancestry/refs. Governed `referTo` is rejected — amend via `plantGoverned`. |
| Keystore | Encrypted at rest by default (`CAIRN_KEYSTORE_SECRET` → SHA-256 → AES). Create-only paths; wrong secret never replaces identity files. Not a password-stretching KDF. |
| Useful-work market | Still deferred; `RecordCertificate` remains the natural anchor |

## Operational layers

1. **Content distribution** — HTTP blobs + replay-checked chain pull (real)
2. **Convergence** — peer directory + HTTP gossip + deterministic fork choice (functional alpha)
3. **Finality** — quorum certificates over sealed blocks; height-bound seq;
   membership history on the packaged serve path; multi-home CLI ceremony
   (`bft replica-set keygen|…|finalize|install`)

## CLI cheat sheet

```bash
export CAIRN_KEYSTORE_SECRET='…'          # required for durable replica keys
# or: export CAIRN_KEYSTORE_PLAINTEXT=1   # lab only

# --- Multi-home membership ceremony (one private key per machine) ---
# On each host:
./bin/cairn bft replica-set keygen r0
./bin/cairn bft replica-set export-pubkey r0 /tmp/r0.pubkey.canon
# Coordinator gathers pubkeys, assembles draft, distributes draft.canon:
./bin/cairn bft replica-set import-pubkey /tmp/r0.pubkey.canon   # …repeat
./bin/cairn bft replica-set assemble r0 r1 r2 r3
./bin/cairn bft replica-set export-draft /tmp/draft.canon
# Each member:
./bin/cairn bft replica-set import-draft /tmp/draft.canon
./bin/cairn bft replica-set seal r0          # writes ceremony/seals/r0.canon
# Coordinator imports seals and commits tip + history:
./bin/cairn bft replica-set import-seal /tmp/r0.seal.canon       # …repeat
./bin/cairn bft replica-set finalize
./bin/cairn bft replica-set export /tmp/replica-set.bundle.canon
# Every host installs the bundle:
./bin/cairn bft replica-set install /tmp/replica-set.bundle.canon
# Amendments: assemble --activation H … then approve (predecessor quorum) + seal + finalize
./bin/cairn bft replica-set status

# Lab shortcut (all keys under one home — not multi-machine):
./bin/cairn bft replica-set init r0 r1 r2 r3

./bin/cairn serve 8743
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
