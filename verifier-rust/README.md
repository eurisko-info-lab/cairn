# Cairn Verifier (Rust)

`verifier-rust/` is an independent Rust implementation of core PR34 federation checks.

Current capabilities:

- Decode Cairn Canon bytes deterministically (same wire format as Scala/JVM runtime).
- Read artifacts directly from DiskCas layout (`objects/ab/cdef...`) with digest re-check.
- Verify replica-set member seals on `replica-set-manifest` artifacts.
- Verify `FederationFinalityCertificate` quorum signatures over proposal digests.
- Verify cert/proposal projection binding (`transition`, `state`, `previousState`, `epoch`, `replicaSet`, `federationId`).
- Replay federation transitions anchored on ledger blocks and validate:
  - transition hash-link (`before`/`after` chain)
  - finality certificate presence and ledger anchoring (`record-certificate`)
  - cert/proposal/manifest consistency per generation
  - expected federation id and genesis state pin

## CKC architecture (PR34 center)

The verifier now centers on a single executable judgment interface in
[verifier-rust/src/ckc.rs](src/ckc.rs):

- Query: what to derive (`Resolve`, `VerifyCertBinding`, `ReplayHistory`)
- Derive: one deterministic engine `derive(constitution, budget, query)`
- KernelResult: one result algebra (`Valid`, `Invalid`, `Missing`, `Exhausted`)

This maps operational commands to one conceptual relation:

- Resolve digest under immutable closure
- Verify certificate/proposal binding
- Replay federation history to a unique final state

CLI commands are now thin facades over that same derivation engine.

## Build

```bash
cd verifier-rust
cargo check
cargo run -- --help
```

## Commands

### Verify one certificate/proposal pair

```bash
cargo run -- verify-cert \
  --cas /path/to/node-root \
  --cert <certificate-digest> \
  --proposal <proposal-digest> \
  --manifest <replica-set-manifest-digest>
```

### Verify full federation history from a node root

```bash
cargo run -- verify-history \
  --node-root /path/to/node-root \
  --federation-id <federation-id-digest> \
  --genesis-state <genesis-federation-state-digest>
```

`--node-root` is the same directory used by `Node` in Scala (`chain` file + `objects/`).

## Notes

- This verifier is intentionally independent from JVM runtime code and does not call Scala logic.
- Scope is PR34 foundation for federation history and certificate/proposal binding checks; deeper application/runtime replay checks can be added incrementally in this crate.
