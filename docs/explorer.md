# Cairn Explorer (Web UI)

Local-first browser and semantic cockpit for a PoA node / CAS root. It combines
typed artifact inspection, branch history, generated Studio proposals,
conflict/migration assistance, evidence review, and trust consequences.

## Where is the data? (`CAIRN_HOME`)

| | |
|--|--|
| **Env** | `CAIRN_HOME` (optional) |
| **Default** | `./.cas` under the process cwd (from the repo with sbt: `…/cairn/.cas`) |
| **Publisher node** | `$CAIRN_HOME/nodeA` (MVP `publish` target) |
| **Print paths** | `sbt "examples/runMain cairn.examples.Main home"` |

Transcripts write a **fresh run** under `$CAIRN_HOME/runs/<timestamp>/` and print
absolute node paths plus a copy-paste `ui` command. Bare `ui` follows
`$CAIRN_HOME/LATEST` → that run’s `nodeA`. Home/run/ui path I/O, CLI
hash/put/canon/transcript source reads, load-language, and emit-languages are
authorized via `Filesystem` (`EffectContext.forFilesystem`). Board discovery uses
`CasAdminEffects.artifacts` (CAS `stats` gate).

```bash
sbt "examples/runMain cairn.examples.Main transcript transcripts/mvp.cairn"
# … ends with:
# blockchain nodes:
#   nodeA = /…/cairn/.cas/runs/…/nodeA (has chain)
# explorer: sbt "examples/runMain cairn.examples.Main ui /…/nodeA"

sbt "examples/runMain cairn.examples.Main ui"   # opens LATEST nodeA
```

Override:

```bash
export CAIRN_HOME=/path/to/store
sbt "examples/runMain cairn.examples.Main ui /path/to/store/nodeA 8765"
```

## What it shows

| Area | Behavior |
|------|----------|
| Overview | Chain length, CAS stats by `ArtifactKind` |
| Chain | Blocks → txs (publish / heads / identities / certs) |
| CAS | Kind histogram; open any digest |
| Board | Read-only Fact–Intent–Hint graph from a search IR module |
| Trust | Capability revocation + delegation hops (`RevocationLog` / `DelegationLog`; CAS digest-merge — **not** BFT / Studio) |
| Languages | Loaded packs; scratch editor with parse/print validate |
| Studio | Branch-bound semantic navigation, staged ΔL proposals, validation, previews, conflicts, migrations, submission, and evidence |

## Typed viewers / editors

Surfaces: **text** (grammar printer), **json** (Canon tree), **canon** (debug).
The scratch parser is propose-only. Studio staging produces ordinary ΔL and
cannot mutate a module directly; governed submission flows through validation,
the branch's `AcceptanceConstitution`, an accepted tip, and a branch transaction.

## Search board

After `transcripts/search-board.cairn`, the **Board** tab (or `GET /api/board`)
shows nodes (`origin` / `goal` / `fact` / `intent` / `hint`) and edges
(`supports` / `spawns`) from the published module. Optional
`?digest=<ir-hex>` selects a specific board; otherwise the first
search-shaped IR artifact in CAS is used.

```bash
sbt "examples/runMain cairn.examples.Main transcript transcripts/search-board.cairn"
sbt "examples/runMain cairn.examples.Main ui"   # Board tab
```

## API (JSON)

`GET /api/health|overview|chain|blocks|board|languages|cas/stats|trust`,
`GET /api/trust/revocations`, `POST /api/trust/revoke`,
`GET /api/trust/delegations`, `POST /api/trust/delegate`,
`GET /api/blocks/{height|digest}`, `GET /api/artifacts/{digest}[/view]`,
`POST /api/parse`; `POST /api/studio/open|stage|collection|undo|mode|
resolve-conflict|migrate|submit`; and `GET /api/studio/review/{id}` /
`status/{id}`. Static UI at `/` and `/ui/…`.

### Trust tab

Revocation publishes CAS `capability-revocation` digests via `RevocationLog`;
delegation hops publish `capability-delegation` digests via `DelegationLog`.
Both follow the `ReplayReplication` want/have shape — merge, not consensus.
Trust administration stays outside Studio: Studio displays acceptance and trust
consequences, while the Trust tab governs revocation and delegation.

## Productionization boundary

The semantic Studio vertical is complete, but the current LSP remains
full-document only and the UI is not yet a large-document, multi-user daily
environment. [PR31](../ROADMAP.md#pr31--studio-productionization) owns
incremental synchronization, multi-file/workspace LSP, virtualization,
background validation/indexing, accessibility, identity/permissions, and
deployment observability.
