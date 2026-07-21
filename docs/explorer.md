# Cairn Explorer (Web UI)

Local-first browser for a PoA node / CAS root: walk the chain, open
transactions, and view **typed** artifacts with kind-specific surfaces.

## Where is the data? (`CAIRN_HOME`)

| | |
|--|--|
| **Env** | `CAIRN_HOME` (optional) |
| **Default** | `./.cas` under the process cwd (from the repo with sbt: `…/cairn/.cas`) |
| **Publisher node** | `$CAIRN_HOME/nodeA` (MVP `publish` target) |
| **Print paths** | `sbt "examples/runMain cairn.examples.Main home"` |

Transcripts write a **fresh run** under `$CAIRN_HOME/runs/<timestamp>/` and print
absolute node paths plus a copy-paste `ui` command. Bare `ui` follows
`$CAIRN_HOME/LATEST` → that run’s `nodeA`. Home/run/ui path I/O is authorized
via `Filesystem` (`EffectContext.forFilesystem`).

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
| Languages | Loaded packs; scratch editor with parse/print validate |

## Typed viewers / editors

Surfaces: **text** (grammar printer), **json** (Canon tree), **canon** (debug).
Editor is propose-only (`POST /api/parse`) — no silent CAS/ledger writes.

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

`GET /api/health|overview|chain|blocks|board|languages|cas/stats`,
`GET /api/blocks/{height|digest}`, `GET /api/artifacts/{digest}[/view]`,
`POST /api/parse`. Static UI at `/` and `/ui/…`.
