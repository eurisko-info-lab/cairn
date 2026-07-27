# Operations guide

This page connects the user-facing commands to Cairn's operational model. For
the first run, use [Getting started](getting-started.md).

## Build and state

```bash
sbt -batch examples/assembly
export CAIRN_HOME=/path/to/cairn-state
export CAIRN_KEYSTORE_SECRET='choose-a-real-secret'
./bin/cairn home
```

`CAIRN_HOME` is an operational directory, not semantic application identity.
Applications start from root digests; workspaces and accepted branches point to
content-addressed artifacts.

Use `CAIRN_KEYSTORE_PLAINTEXT=1` only for disposable local labs.

## Command families

| Family | Purpose |
|---|---|
| `home`, `hash`, `put`, `get`, `canon`, `why` | CAS inspection and provenance |
| `languages`, `capabilities`, `repl`, `lsp`, `ui` | Language and interaction surfaces |
| `transcript` | Reproducible end-to-end scenarios |
| `repo`, `branch`, `domain`, `workflow` | Semantic repository and governance workflows |
| `chain`, `tx`, `auth`, `replay`, `recover` | Ledger, authority, replay, and recovery |
| `serve`, `pull`, `fetch-hash`, `peer`, `gossip` | HTTP node and replication |
| `bft` | Finality and replica-set ceremonies |
| `app install`, `app start` | Artifact-only application installation and startup |
| `porcelain` | Named user workflows over lower-level plumbing |

Running an unknown command prints the current command-family usage.

## Transcripts

Transcripts are executable acceptance scenarios:

```bash
./bin/cairn transcript transcripts/mvp.cairn
./bin/cairn transcript transcripts/max.cairn
./bin/cairn transcript transcripts/repository-workflow.cairn
```

Use MVP for the shortest language/change/proof/publication path. MAX adds
runtime-loaded language text, path edits, queries, three-node gossip, and a host
projection. The other top-level transcripts isolate specific domains.

## Artifact-only applications

Given a source CAS and a root digest:

```bash
./bin/cairn app install <root-digest> <source-cas> <local-cas>
./bin/cairn app start <root-digest> <local-cas>
```

Installation recursively discovers dependencies and verifies every digest.
Startup reconstructs languages, grammars, capabilities, and typed entries from
the manifest. It does not receive a host-built language map.

## Browser and Studio

```bash
./bin/cairn ui
```

The browser combines artifact, chain, branch, provenance, board, Studio, and
trust views. Studio proposes ΔL changes and previews results; acceptance still
flows through validation and the branch constitution. Trust administration is
separate from document editing.

See [Explorer and Studio](explorer.md).

## Node and replication

```bash
./bin/cairn serve 8743
./bin/cairn peer add node-b http://127.0.0.1:8753
./bin/cairn peer list
./bin/cairn gossip once
```

For protocol details, failure behavior, and BFT ceremonies, see
[Distribution](distribution.md). For ledger transactions and publication, see
[Ledger](ledger.md).

## Recovery and audit

```bash
./bin/cairn recover
./bin/cairn why <artifact-digest>
./bin/cairn porcelain chain-quarantine
```

Disk CAS reads reverify digests. Branch acceptance uses a journal so interrupted
local accepts can roll forward. Hardening audits resolve a complete application
or signed-release graph and classify its trusted closure; see
[Self-hosting and hardening](self-hosting-and-hardening.md).

## Security posture

- Prefer encrypted keystores via `CAIRN_KEYSTORE_SECRET`.
- Treat publisher and authority keys as distinct operational roles where the
  deployment requires it.
- A digest proves byte identity, not correctness or publisher authority.
- A signature proves signer identity for bytes, not semantic validity.
- BFT finality applies to authenticated configured replica sets; Cairn is not a
  permissionless public-chain protocol.
