# Cairn Verifier (Lean)

`verifier-lean/` is a second independent verifier implementation for PR34,
written in Lean 4. It mirrors the CKC judgment center used by the Rust verifier.

## Architecture

The core lives in `VerifierLean/CKC.lean` and is organized around one relation:

- Query: what to derive (`resolve`, `verifyCertBinding`, `replayHistory`)
- Derive: one executable function `derive`
- KernelResult: one result algebra (`valid`, `invalid`, `missing`, `exhausted`)

This keeps the verifier architecture aligned across implementations while
remaining independent from JVM runtime code.

## Build

```bash
cd verifier-lean
lake build
```

## Run

```bash
cd verifier-lean
lake exe verifier-lean --help
```

Example invocations (against in-memory demo context):

```bash
lake exe verifier-lean resolve /path/to/node-root <digest>
lake exe verifier-lean verify-cert /path/to/node-root <cert> <proposal> <manifest>
lake exe verifier-lean replay-history /path/to/node-root <federation-id> <genesis-state>
```

## Scope

- Current state: executable CKC foundation with deterministic evidence hashing,
  cert-binding checks, and replay checks against real node paths (`chain` and
  `objects/`) using closure existence/anchoring checks.
- Strictness upgrade: `verify-cert` now decodes canonical artifact bytes and
  enforces certificate/proposal projection consistency (`transition`, `state`,
  `previousState`, `epoch`, `replicaSet`, `federationId`) plus manifest-tag
  validation.
- Replay upgrade: `replay-history` now semantically traverses chain blocks,
  extracts `publish-artifact` / `record-certificate` events, replays
  federation transitions in order, checks finality anchoring, validates state
  and proposal/certificate consistency, and enforces epoch monotonicity.
- Quorum/manifest upgrade: verifier now validates replica-set seal coverage
  (authorities vs seals), valid 3f+1 replica cardinality, distinct commit
  replicas, membership of commit replicas in authority ids, and quorum
  threshold on certificate commits. It also checks manifest digest binding
  (`cert.replicaSet` must equal SHA-256 of the manifest body encoding).
- Current limitation: signature verification parity with `verifier-rust/`
  is still pending.
