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

## Domain trunk

The ledger is also the **root of domains** — analogous to DNS TLDs (`.org`,
`.com`). Branch manifests record that tree separately from causal change digests:

| Field | Meaning |
| ----- | ------- |
| `primaryAncestor = None` | Branch hangs on the global trunk (e.g. `LAW`, `CHEMISTRY`) |
| `primaryAncestor = Some(name)` | Strongest binding to one parent domain (e.g. `SDS` → `LAW`) |
| `references` | Soft links to other domains (e.g. `SDS` also refers to `CHEMISTRY`) |

A branch may therefore have many ancestors via references, or a single strongest
ancestor plus optional soft refs. `DomainBranch.wellFormed` (Kernel) and
`Branches.forkFrom` / `referTo` (system-handler) enforce known names,
non-self / non-overlapping primary∩refs, and **transitive primary-cycle**
detection via `primaryOf`. Causal `parents` on the same manifest remain
change-digest edges for merge/LCA — not domain names. Namespace authority,
ownership rules, and ancestry-change policy remain residual work.

## Invariants

- The ledger records **hashes, heads, identities, certificates** — never artifact
  bodies. Bodies live in CAS and travel by digest (§4.9).
- A branch head may only point at a **published** typed key.
- Re-registering an identity with a different key is rejected.
- Only the kernel certifies: `systemhandler.Node.append` seals a block and then re-validates
  it through `LedgerKernel.applyBlock` before committing (§4.6).

## Out of scope (design hooks only, §6 Phase 5)

Public tokenomics, open mining, BFT finality, peer discovery. See
[distribution.md](distribution.md) for what exists (pull sync, divergence surfacing)
and what is deferred.
