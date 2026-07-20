# STATUS-2 — end of the maximalization plan (PLAN-2)

Date: 2026-07-20. All 50 stories M1–M50 of [PLAN-2.md](PLAN-2.md) landed on top of
the S1–S50 base. Full suite: **199 tests green** (`sbt test`), including a
100 000-term fuzz corpus with zero round-trip failures.

## Story scorecard

| Wave | Stories | Status | Evidence |
|---|---|---|---|
| A identity/CAS | M1–M5 | ✅ | `WaveASuite` — structural fingerprints, α-invariant digests, fsck/GC/stats, sha256+sha512 agility with validated migrations, 100 MiB chunked streaming with dedup |
| B grammar engine | M6–M12 | ✅ | `WaveBSuite` — block/run/adjacent1/restOfLine, byte-exact trivia print + span splice, caret diagnostics + furthest-failure, lints wired into `Compose`, packrat (exponential→linear fixture) + incremental reparse, panic-mode recovery (3 errors → 3 diagnostics), JSON/canon surfaces |
| C ΔL/merge | M13–M18 | ✅ | `WaveCSuite` — rename-morphism pushouts, `List[E]` functors, path edits (`edit f at [2] = …`), compose/invert/commute laws, three-way semantic merge + conflict artifacts, migrations transporting modules *and* change-sets |
| D proofs | M19–M24 | ✅ | `WaveDSuite` — `$neq/$fresh/$lt/$le` + injected extensions (the shadowing exploit now rejected), computational `$ctx-lookup`, unification search (type inference for all goldens), tactic replay through the independent checker, seeded property claims with shrinking, certified evaluation traces (3 tamper modes fail) |
| E computation | M25–M29 | ✅ | `WaveESuite` — full interaction combinators (γδε + labelled-konst copy), general λ lowering with δ-trees + readback (α-equivalent to tree eval), parallel reduction (identical normal forms), compiled dispatch agrees over 300 random terms, Bend surface runs end-to-end |
| F rosetta | M30–M34 | ✅ | `WaveFSuite` — `quicksort : ∀a. Ord a ⇒ …` + counter effect + ADT in ONE artifact; four ports (Scala/Lean/Haskell/Rust) all passing the whole-file byte fixpoint; Scala port RUNS (`ALL TESTS PASS`); scaffolds + obligations.json (8 entries) via the JSON surface |
| G ledger/trust | M35–M40 | ✅ | `WaveGSuite` — Merkle state + light-client inclusion proofs, 2-of-3 quorum authority rotation (round-robin enforced), policy language (with ΔPolicy) gating head updates, localhost-HTTP want/have resumable sync, 3-node gossip with explicit reorg events, 4-hop provenance `why` |
| H bootstrap/packs | M41–M49 | ✅ | `WaveH1Suite`/`WaveH2Suite` — full meta surface (STLC as text = same digest; [languages/stlc.cairn](languages/stlc.cairn) checked in), **bootstrap fixpoint** ([languages/meta.cairn](languages/meta.cairn) describes the meta language, digest-for-digest), runtime language loading (toy language from pure text), capability manifests + lint, LSP (diagnostics/format/hover; rename = ΔL rename emitting a ValidatedChangeSet) + REPL, query language (3 golden queries, ΔQuery), PKI-max (chain validity as declarative rules checked by the same kernel checker; expiry windows; CRLs), SDS (acetone, ΔSDS domain gate, shadow overrides, re-parseable render, ledger publish), Unison pack (α-digest store; renames touch nothing) |
| M50 | CI/fuzz/bench | ✅ | `FuzzSuite` (100k terms, 6 grammars, zero failures), [.github/workflows/ci.yml](.github/workflows/ci.yml) (JDK 17/21, transcripts, language-file sync check), `cairn.examples.Bench` |

## Golden digests (`sbt "examples/runMain cairn.examples.Main digests"`)

```text
language pki    6e5be96d864104f9b04a3a91e106cdcbec0d56d560fae2b46f5b2b81555de8ad
language policy 542f97a889a5081d314248d8ef6c36fe757d5ddb7921c3b085fe1d30871f4e1b
language query  33b0c2e1bc6ac50c372d572e4131f9c0d1bc5a24a922eaae3edcb34c97c3d601
language sds    00893102b8eaaf7df9d3a5c970f3bd589287f3be563d960d05c8c936c1a8f8bd
language stlc   c505555940893bf55221aeef05586479991104c4a5066c197f9b9fc0b2ff9954
rosetta quicksort2 c2de9525e314f240a4dea977e9ad3992e31d1789b03bce8d5e70ce87dc9d04fb
```

(Digests changed vs [STATUS.md](STATUS.md): the fragment canonical form gained
side conditions (M19) and the grammar vocabulary gained layout elems (M6) —
deliberate, versioned breaks.)

## Benchmarks (`sbt "examples/runMain cairn.examples.Bench"`, this machine)

```text
interpretive TreeEngine      3.62 ms   (400 terms, depth 7)
compiled dispatch (M28)      2.01 ms   (~1.8x)
parse 200-lambda chain       2.85 ms   (packrat)
sequential net reduction     0.62 ms
parallel net reduction       0.34 ms   (3 sweeps, pairs 1,2,3)
```

## Honest deviations from PLAN-2 ACs

- **M7**: byte-identical round-trip holds for whole unedited files + span-precise
  splicing; there is no general dirty-subtree re-association (splice is the API).
- **M10**: incremental reparse retains prefix memo entries; suffix entries are
  discarded (index shift), so "O(affected)" is prefix-anchored.
- **M17**: semantic merge operates on module change histories; `Branches` refs are
  not yet merge-aware.
- **M31**: signatures/declarations are fully grammatical; expression BODIES are
  verbatim single-line regions inside the file grammars (rendered by one shared
  fold, not per-host string soup). Whole-file byte fixpoint still enforced.
- **M32/M34**: Haskell/Rust/cargo runs and the Haskell effect projection are
  assume-skipped when toolchains are absent; Scala effect + tests always run
  when scala-cli is present. Lean remains golden-checked.
- **M39**: gossip is an in-process round-based simulation over real node stores
  (validated adoption + reorg events), not a network daemon.
- **M44**: LSP covers initialize/didOpen/didChange/formatting/rename/hover over
  framed JSON-RPC; no workspace folders, no partial edits (full-document sync).
- **M50**: fuzzing covers the six non-layout grammars; layout combinators
  (block/run/adjacent1) are exercised by directed tests instead.

## Success criteria (§9) — re-verified at maximal level

All 11 still hold, now with: α-invariant identity (3), proofs with side
conditions + search + tactics + traces (4), full interaction combinators with
readback (5), four host ports with byte fixpoints (6), Merkle-proof-verifiable
publication with governed authorities and policies (7), and a self-describing
meta language loaded from text at runtime (11 — the §2b bootstrap is no longer
staged: the fixpoint test passes).
