# Documentation

This page is the front door to Cairn's documentation. Read the first three
documents in order if the project is new to you; use the rest as references.

## First hour

1. [Project overview](../README.md) — what Cairn is and what exists today.
2. [Getting started](getting-started.md) — build, run, inspect, and modify a
   small semantic workflow.
3. [Core concepts](concepts.md) — artifacts, languages, ΔL, Studios,
   workspaces, acceptance, branches, applications, and releases.

## Build and use Cairn

| Document | Use it for |
|---|---|
| [Operations](operations.md) | CLI, CAS home, transcripts, browser, node, ledger, sync, and distribution |
| [Explorer and Studio](explorer.md) | Browser surfaces, semantic editing, and trust views |
| [Exemplars](exemplars.md) | STLC, PKI, Law, SDS, Search, MiniTT, LeanCore, UnisonCore, and Riemann |
| [Porcelain](porcelain.md) | Git-style user commands versus lower-level plumbing |
| [Bootstrap](bootstrap.md) | Detailed empty-CAS-to-publication walkthrough |

## Understand the system

| Document | Use it for |
|---|---|
| [Architecture](architecture.md) | Module DAG, trust boundary, effects, repository model, and parity claims |
| [Vocabulary](vocabulary.md) | Compact term reference |
| [Ecosystem](ecosystem.md) | Signed releases, discovery, migration routes, publication, and replication |
| [Self-hosting and hardening](self-hosting-and-hardening.md) | PR25 closure ceremony, trust accounting, and optimization laws |
| [Ledger](ledger.md) | Transactions, identities, blocks, and publication |
| [Distribution](distribution.md) | HTTP sync, gossip, BFT finality, and replica-set ceremonies |
| [Agreement](agreement.md) | Lean/HVM agreement evidence versus checked proof evidence |
| [Rosetta](rosetta.md) | Host-language projections and their obligations |
| [Lowering](lowering.md) | Tree and interaction-net execution paths |
| [PR34 staircase fixture](pr34-staircase-fixture.md) | Permanent `G0 -> G1` conformance walkthrough and promotion gate |

## Change Cairn

| Document | Use it for |
|---|---|
| [Development](development.md) | Build commands, test strategy, source locations, and common change recipes |
| [Contributing](../CONTRIBUTING.md) | Contribution rules and review expectations |
| [Roadmap](../ROADMAP.md) | Implemented phases and post-foundation direction |
| [Assumptions and honest gaps](assumptions.md) | Deliberate limits, deferrals, and trust assumptions |
| [Project constitution](../CAIRN-PROMPT.md) | Historical design mandate and original phased plan |

## Source-of-truth policy

- The code and executable tests decide behavior.
- [Architecture](architecture.md) decides the current module and trust story.
- [Assumptions](assumptions.md) decides what is deliberately incomplete.
- [Roadmap](../ROADMAP.md) summarizes status; it does not override those files.
- The constitution records intent and history, so older module names or phase
  descriptions in it are not necessarily the live implementation.
