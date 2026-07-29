# Porcelain and plumbing

Git-style layering for filling the Charb/Marble transcript gap without
pretending Cairn is a Marble CLI clone.

| Layer | Role | Lives in |
| ----- | ---- | -------- |
| **Plumbing** | Named wrappers over existing Kernel / Core / System Handler engines | `surface.Plumbing` |
| **Porcelain** | User-facing CLI verbs + transcript `porcelain THEME ;` | `surface.Porcelain`, `Cli` |
| **Transcripts** | CI scripts that call porcelain (or stay `deferred`) | `transcripts/charb/` |

```
Charb YAML theme
      │
      ├─ has Cairn plumbing? ──yes──► porcelain THEME ;  (promoted)
      │
      └─ §8 / no engine yet ────────► deferred "…" ;   (honest gap)
```

## CLI

```bash
# Build a fat jar once, then use the wrapper (no sbt at runtime):
sbt examples/assembly
./bin/cairn home
./bin/cairn transcript transcripts/mvp.cairn
./bin/cairn serve 8743          # HttpNode on $CAIRN_HOME/nodeA
./bin/cairn pull http://127.0.0.1:8743
./bin/cairn porcelain authorization

# Or via sbt (composition root unchanged):
sbt "examples/runMain cairn.examples.Main chain status"
sbt "examples/runMain cairn.examples.Main auth check alice"
sbt "examples/runMain cairn.examples.Main branch list"
sbt "examples/runMain cairn.examples.Main domain show"
sbt "examples/runMain cairn.examples.Main compose status stlc"
sbt "examples/runMain cairn.examples.Main catalog export"
sbt "examples/runMain cairn.examples.Main workflow list"
sbt "examples/runMain cairn.examples.Main recover"
sbt "examples/runMain cairn.examples.Main replay snapshot"
sbt "examples/runMain cairn.examples.Main tx state"
sbt "examples/runMain cairn.examples.Main light verify"
sbt "examples/runMain cairn.examples.Main porcelain authorization"
```

## Promoted Charb themes (36)

See `Porcelain.promotedThemes` and `scripts/gen-charb-transcripts.py` (`PORCELAIN`).
Each maps to `Plumbing.charbTheme` (auth, chain status/export/compare, branch/domain,
compose/catalog, workflow list, recover, replay snapshot, tx state, light verify,
chain quarantine, federation registry, supply-chain governance, mirror registry,
object/run/commit registry).

Five of these (`chain-quarantine`, `federation-registry`, `governance-supplychain`,
`mirror-registry`, `object-run-commit-registry`) were promoted from deferred by
recognizing they were thin listings over engines that already existed —
`CasAdminEffects.fsck`/`Branches.reclaimOrphanBlobs` (quarantine), the ledger's
own `authorities`/`certificates` maps (federation), `Branches.forkFrom`/`referTo`
(supply-chain governance — the same primitives `fork-from`/`refer` transcript
steps use), `Sync.compare` (mirror), and `Provenance.index` joined with
`Branches.list`/`load` (object/run/commit) — not new engines, just names nobody
had given them yet.

## Removed from Charb transcript suite (5)

These themes are intentionally omitted from the repository's Charb transcript
suite because they are outside the current Cairn product scope: work-market
(`chain-work-scan` / `chain-work-adjudication` / `chain-work-reward`),
`stake-registry`, and `consensus-economics-phase1`.
Regenerate with `python3 scripts/gen-charb-transcripts.py --source DIR` (then `--update-docs`).
plumbing.
