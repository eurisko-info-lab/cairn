# Getting started

This guide takes a fresh checkout through a working semantic change and shows
where to look next. It assumes no prior knowledge of Cairn.

## 1. Install prerequisites

Cairn is built with Scala 3 and sbt. CI supports JDK 17 and 21.

Required:

- JDK 17 or 21;
- sbt 1.x;
- Git and Bash for the documented commands.

Optional tools—`scala-cli`, `runghc`, `cargo`, Lean, and HVM—exercise foreign
projection or agreement paths but are not required for the core workflow.

Check the essentials:

```bash
java -version
sbt --version
```

## 2. Run a focused acceptance suite

```bash
sbt -batch "tests/testOnly cairn.tests.Phase8Suite"
```

This compiles the project and exercises the executable transcript layer. The
first build downloads Scala and test dependencies and therefore takes longer.

Run the entire repository later with `sbt -batch test`; it includes the 100k
term fuzz corpus and distributed-system tests.

## 3. Run the MVP transcript

```bash
sbt -batch "examples/runMain cairn.examples.Main transcript transcripts/mvp.cairn"
```

Read [transcripts/mvp.cairn](../transcripts/mvp.cairn) alongside the output.
Its steps are deliberately small:

1. load the STLC language pack;
2. prove parse/print round trips;
3. evaluate several terms;
4. add and rename a definition through ΔSTLC;
5. certify a claim;
6. publish a branch and fetch it by hash.

The transcript does not call an STLC-specific edit API. The parser, evaluator,
change language, and checks are derived or selected from language artifacts.

## 4. Inspect a language definition

Open [content/languages/stlc.cairn](../content/languages/stlc.cairn). It defines
sorts, constructors, binding, rewrite rules, and typing judgments. Its concrete
surface lives in
[content/languages/stlc/surfaces/default.cairn](../content/languages/stlc/surfaces/default.cairn).

List the languages resolved by the CLI:

```bash
sbt -batch "examples/runMain cairn.examples.Main languages"
```

Show their canonical identities:

```bash
sbt -batch "examples/runMain cairn.examples.Main digests"
```

A semantic edit can change a digest. That is expected only when the canonical
meaning changed deliberately.

## 5. Build the standalone CLI

```bash
sbt -batch examples/assembly
export CAIRN_HOME="$(mktemp -d)"
export CAIRN_KEYSTORE_PLAINTEXT=1  # local lab only; use a secret outside a lab
./bin/cairn home
./bin/cairn transcript transcripts/mvp.cairn
```

`CAIRN_HOME` contains local CAS, ledger, branch, peer, and key material. If it
is unset, the CLI uses `./.cas`. The wrapper runs the assembled jar and needs no
sbt after assembly.

For persistent or shared use, set `CAIRN_KEYSTORE_SECRET`; plaintext keys are
an explicitly enabled development convenience.

## 6. Try a richer workflow

Choose one:

```bash
# Three-domain ancestry: PKI → Law → SDS, plus Chemistry reference
./bin/cairn transcript transcripts/sds-domain-journey.cairn

# Repository changes and merge behavior
./bin/cairn transcript transcripts/repository-workflow.cairn

# Web explorer and Studio
./bin/cairn ui
```

The UI command prints its listening address. See [Explorer and Studio](explorer.md)
for the available views and the distinction between proposal editing and trust
administration.

## 7. Know what you just exercised

The shortest useful mental model is:

```text
.cairn language data
  → composed language + capability bundle
  → ordinary ΔL proposal
  → replay + validation + acceptance evidence
  → accepted branch or explicit conflict
  → content-addressed publication
```

Continue with [Core concepts](concepts.md), then use the
[documentation map](README.md) to choose an application, operations, or
development path.

## Common problems

**The wrapper says the jar is missing.**

Run `sbt -batch examples/assembly` from the repository root.

**A command refuses to create a key.**

Set `CAIRN_KEYSTORE_SECRET` or, for an isolated lab only,
`CAIRN_KEYSTORE_PLAINTEXT=1`.

**A test for Haskell, Rust, Lean, or HVM is skipped.**

Those tools are optional. A skip does not mean the canonical Cairn path failed.

**A language-file or golden-digest check changed.**

Do not update it blindly. Read [Development](development.md#canonical-data-and-goldens)
and confirm that the semantic identity change is intended.
