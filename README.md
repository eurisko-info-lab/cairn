# Cairn

[![CI](https://github.com/eurisko-info-lab/cairn/actions/workflows/ci.yml/badge.svg)](https://github.com/eurisko-info-lab/cairn/actions/workflows/ci.yml)
[![Scala](https://img.shields.io/badge/scala-3.3.4-DC322F.svg)](https://www.scala-lang.org/)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)

Cairn is a self-describing semantic environment. Languages, applications,
changes, validation rules, workspaces, proofs, releases, and governance are
portable, content-addressed artifact graphs interpreted by a small generic
kernel.

In an ordinary software stack, a parser, object model, editor, validator,
version-control system, package registry, and deployment policy often disagree
about what a document means. Cairn gives them one semantic source of truth.

## What can Cairn do today?

Cairn can:

- define a language from composable `.cairn` fragments;
- derive parsing, printing, structural editing, and the free change language ΔL;
- stage semantic changes in durable, signed, replicable workspaces;
- validate, review, approve, merge, resolve conflicts, and migrate changes;
- produce checked proof artifacts and separately classified projection or
  external-agreement evidence;
- package an application behind one root digest;
- sign, publish, discover, install, and replicate application releases;
- reconstruct a generated Studio from artifacts rather than host callbacks;
- edit Cairn's own Meta language and grammar through that same Studio path.

SDS is the most complete application vertical: generated forms, keyed
collections, multilingual editing, inline validation, preserved-source and
report previews, governed commits, semantic conflicts, migration, and
production JSON/XML/XLSX/PDF/image surfaces.

Cairn is research software at version 0.1. Its foundational architecture is
implemented and heavily tested; it is not yet a stabilized 1.0 distribution.

## The model in one picture

```text
language + grammar + capabilities + acceptance
              │
              ▼
          DomainRuntime
              │
       ApplicationManifest
              │
        signed ecosystem release
              │
      install from one root digest
              │
              ▼
        generated Studio
              │
      durable ΔL proposal workspace
              │
      validate / prove / review
              │
              ▼
       constitutionally accepted change
              │
       native causal repository
        (branches are head views)
              │
        migrate and publish successor
```

Three histories remain deliberately separate:

| History | What it records |
|---|---|
| Release history | Evolution of available application definitions |
| Causal change graph | Accepted domain state; branches are named head views |
| Workspace history | Proposals, reviews, approvals, handoffs, and rebases |

See [Core concepts](docs/concepts.md) for the vocabulary behind this diagram.

## Try it

Prerequisites:

- JDK 17 or 21;
- sbt 1.x;
- Bash for the `bin/cairn` wrapper.

From a fresh checkout:

```bash
sbt -batch "tests/testOnly cairn.tests.Phase8Suite"
sbt -batch "examples/runMain cairn.examples.Main transcript transcripts/mvp.cairn"
```

The MVP transcript loads STLC, round-trips and evaluates terms, applies ΔL,
certifies a claim, publishes a branch, and fetches it by hash.

Build the standalone CLI:

```bash
sbt -batch examples/assembly
export CAIRN_HOME="$(mktemp -d)"
export CAIRN_KEYSTORE_PLAINTEXT=1   # lab use only
./bin/cairn home
./bin/cairn transcript transcripts/mvp.cairn
./bin/cairn languages
```

For a guided explanation and troubleshooting, continue with
[Getting started](docs/getting-started.md).

## Choose a path

| If you want to… | Start here |
|---|---|
| Understand the project without reading code | [Documentation map](docs/README.md) |
| Run the first workflow | [Getting started](docs/getting-started.md) |
| Learn artifacts, ΔL, Studios, branches, and releases | [Core concepts](docs/concepts.md) |
| Understand trust and module boundaries | [Architecture](docs/architecture.md) |
| Explore shipped languages and applications | [Exemplars](docs/exemplars.md) |
| Operate the CLI, node, ledger, or distribution layer | [Operations guide](docs/operations.md) |
| Contribute code or a language pack | [Development guide](docs/development.md) |
| See implemented scope and remaining gaps | [Roadmap](ROADMAP.md) and [Assumptions](docs/assumptions.md) |

## Repository map

The top-level layout follows trust boundaries:

```text
kernel/      canonical artifacts and independent semantic checks
content/     pure language, change, proof, Studio, and projection machinery
container/   CAS, ledger, crypto, networking, and privileged effects
app/         runtime composition, CLI/browser surfaces, examples, and tests
contracts/   shared effect request/response vocabulary
transcripts/ executable end-to-end scenarios
docs/        guides and reference material
```

The enforced dependency graph and trusted-code boundary are documented in
[Architecture](docs/architecture.md). The short rule is:

```text
domain content cannot import privileged system handlers
```

## Confidence and limitations

The full suite currently covers canonical encoding, language composition,
self-description, semantic changes and merges, proof checking, migrations,
acceptance constitutions, durable workspaces, foreign surfaces, signed
ecosystem releases, two-node self-hosting, fuzzing, and distributed operation.

Run it with:

```bash
sbt -batch test
```

Optional Haskell, Rust, Lean, and HVM checks depend on locally installed tools;
the canonical Scala/JDK paths do not.

Important limitations are maintained in [Assumptions and honest gaps](docs/assumptions.md).
Notably, Cairn is not a public permissionless blockchain, generated Rosetta
files are not automatically proofs, and some exemplar depth remains uneven.

## Project policy

- Canonical-byte changes are compatibility changes and must be intentional.
- Domain semantics belong in artifacts, not operation-name switches.
- Optimizations are replaceable caches and cannot become semantic authorities.
- Every accepted Core proposal has an independent checking or replay path.
- No new runtime dependency is added to the kernel/core/handler substrate
  without a compelling architectural reason.

See [CONTRIBUTING.md](CONTRIBUTING.md), [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md),
and the historical project constitution in [CAIRN-PROMPT.md](CAIRN-PROMPT.md).

Licensed under Apache 2.0.
