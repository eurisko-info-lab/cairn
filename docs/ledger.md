# Ledger

Single-node proof-of-authority publication spine (§6 Phase 5, §4.9).

## Model

- **Transactions** (`kernel.Tx`): `RegisterIdentity`, `PublishArtifact(TypedKey)`,
  `SetBranchHead(branch, TypedKey)`, `RecordCertificate(digest)`. All carry Ed25519
  signatures (`SignedTx`); identity registration may be self-signed (bootstrap).
- **State** (`LedgerState`): identities, published typed keys, branch heads,
  certificate digests. The **state root** is the digest of the state's canonical form.
- **Blocks** (`Block`): height, parent digest, txs, claimed post-state root, authority
  name, Ed25519 seal over the unsealed canonical form. Genesis parent is
  `digest("cairn-genesis")`.
- **Transition** (`LedgerKernel`, L0, pure): `applyTx`, `applyBlock` (atomic — parent,
  height, seal, every tx, and the claimed state root must all validate), `replay`
  (whole chain from genesis). Signature verification is injected as a pure function.

## Invariants

- The ledger records **hashes, heads, identities, certificates** — never artifact
  bodies. Bodies live in CAS and travel by digest (§4.9).
- A branch head may only point at a **published** typed key.
- Re-registering an identity with a different key is rejected.
- Only the kernel certifies: `ledger.Node.append` seals a block and then re-validates
  it through `LedgerKernel.applyBlock` before committing (§4.6).

## Out of scope (design hooks only, §6 Phase 5)

Public tokenomics, open mining, BFT finality, peer discovery. See
[distribution.md](distribution.md) for what exists (pull sync, divergence surfacing)
and what is deferred.
