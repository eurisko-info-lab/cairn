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

## Promoted Charb themes (~28)

See `Porcelain.promotedThemes` and `scripts/gen-charb-transcripts.py` (`PORCELAIN`).
Each maps to `Plumbing.charbTheme` (auth, chain status/export/compare, branch/domain,
compose/catalog, workflow list, recover, replay snapshot, tx state, light verify).

## Still deferred (~13)

§8 out of scope or no engine yet: chain quarantine, work-market / stake /
consensus-economics / network-mempool / federation, compliance / mirror /
deps-lock registries, object-run-commit, governance-supplychain (needs SDS
`forkFrom` transcript steps), etc. Regenerate with
`python3 scripts/gen-charb-transcripts.py` after extending plumbing.
