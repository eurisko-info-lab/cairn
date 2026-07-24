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
`Branches.forkFrom` / `referTo` (runtime) enforce known names,
non-self / non-overlapping primary∩refs, and **transitive primary-cycle**
detection via `primaryOf`. Causal `parents` on the same manifest remain
change-digest edges for merge/LCA — not domain names.

**Namespace governance** uses `DomainAgreement` (certificate body tagged
`domain-agreement`): owner subject, child/ancestor language digests, dependency
evidence, and an ancestry-change policy (`replaces` must cite the live agreement
digest; owner and child name are sticky). Plants under a primary also require a
separate `DomainAncestorDelegation` certificate (tagged
`domain-ancestor-delegation`) sealed by the **primary's owner**, cited from
`ancestorDelegation` and verified with `grantorSeal` in `plantGoverned`. Soft
`references` stay structural (no per-ref delegation). The primary must itself be
governed before children can `plantGoverned` under it. Ungoverned local
`forkFrom` remains available for bootstrap.

## Invariants

- The ledger records **hashes, heads, identities, certificates** — never artifact
  bodies. Bodies live in CAS and travel by digest (§4.9).
- A branch head may only point at a **published** typed key.
- Re-registering an identity with a different key is rejected.
- Only the kernel certifies: `systemhandler.Node.append` seals a block and then re-validates
  it through `LedgerKernel.applyBlock` before committing (§4.6).

## Out of scope (design hooks only, §6 Phase 5)

Public tokenomics, open mining, and Sybil-resistant open membership remain out
of scope. Peer discovery (directory), HTTP gossip, and BFT finality certificates
are implemented — see [distribution.md](distribution.md).
