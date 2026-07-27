# Contributing to Cairn

Cairn is developed under a fairly strict architectural discipline — most of
what a contribution needs to respect is already written down. Read these
first:

- [docs/development.md](docs/development.md) — practical build, test, source
  location, canonical-data, and common change recipes.
- [CAIRN-PROMPT.md](CAIRN-PROMPT.md) — the project constitution: vision,
  vocabulary, module boundaries, phased roadmap, non-negotiable requirements.
- [docs/architecture.md](docs/architecture.md) — the current, authoritative
  module/trust/effect-boundary map (source of truth over any diagram elsewhere).
- [docs/assumptions.md](docs/assumptions.md) — documented deviations and
  honest gaps versus the constitution.
- [ROADMAP.md](ROADMAP.md) — phase-by-phase status.

## Ground rules

1. **Respect the trust tiers.** `kernel` is the shared semantic TCB;
   `content/user` may never import `system-handler`
   (`user ↛ system-handler`, enforced by `build.sbt`'s dependency graph, not
   just convention). If you're unsure which module a change belongs in,
   ask in an issue before writing code.
2. **Kernel/Core stays pure.** No I/O in `kernel` or `content/core`. Effects
   are named via the contracts in `contracts/` and only performed by
   `container/system-handler`.
3. **Golden digests are a contract.** Any change that alters a fragment,
   language, or artifact's canonical bytes will change
   `app/tests/golden/digests.txt`. Regenerate deliberately
   (`sbt "examples/runMain cairn.examples.Main digests"`) and explain why in
   your PR description — an unexplained digest diff is treated as a
   regression, not a formality.
4. **Prefer data over code for anything domain-specific.** Adding a ΔL
   operation, a validation rule, or a pack should not require editing an
   interpreter switch — see `ChangeModel`/`ModuleStructural.Spec` for the
   pattern. If your change adds a new "if this domain-specific thing then
   edit this generic file" branch, that's usually the wrong shape.
5. **Transcripts are the acceptance mechanism.** New end-to-end behavior
   should usually come with (or extend) a `.cairn` transcript under
   `transcripts/`, not just a unit test.
6. **No new runtime dependencies beyond the JDK** for `kernel`/`content/core`/
   `container/system-handler`. This is a deliberate, load-bearing constraint,
   not an oversight.

## Working locally

```bash
sbt test                                            # full suite (100k fuzz corpus included)
sbt "examples/runMain cairn.examples.Main digests"  # regenerate golden digests
sbt "examples/runMain cairn.examples.Main emit-languages"  # validate/format-preserve checked-in language sources
```

CI (`.github/workflows/ci.yml`) runs the full suite, a fat-jar smoke test, a
multi-home ceremony, golden digests, MVP/MAX transcripts, and a
language-file sync check on every push/PR — run at least `sbt test` locally
before opening a PR.

## Making a change

1. Open an issue first for anything beyond a small fix — architectural
   discipline here means most non-trivial changes benefit from agreeing on
   the shape before code is written.
2. Keep PRs to one logical change. Large refactors land better as a
   sequence of small, independently-green slices (compile → test → commit)
   than as one large diff.
3. Write commit messages that explain *why*, not just *what* — the codebase
   has a strong precedent for this; look at recent `git log` for the tone.
4. If your change touches `content/languages/*.cairn`, run `emit-languages`
   and inspect the result. Checked-in pack text is runtime source of truth;
   bootstrap seeds exist only for the Meta/STLC/effect fixpoint paths that
   explicitly test digest equality.

## Reporting bugs / requesting features

Use the issue templates. A minimal, reproducible transcript or test case is
worth far more than a description — this is a system built around
transcripts as the source of truth for behavior.

## Code of Conduct

This project follows the [Code of Conduct](CODE_OF_CONDUCT.md).
