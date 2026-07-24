# Cairn — Master Build Prompt

> Hand this document to an implementer or AI agent with no prior context.
> It defines a new project that consolidates the recurring ideas across a family of related
> research/engineering efforts (composable languages, bidirectional surfaces, proofs,
> interaction-net computation, polyglot projections, and ledger-backed semantic publication).

**Document role.** This is **not** a chronological summary of all conversations. It **is** a
consolidated **implementation constitution** for the main technical programme (GRANITE /
Granit / Marble / MetaLego / Rosetta / interaction nets / semantic repositories). It captures
the **converged architecture**, not every historical branch, rename, abandoned prototype, or
non-programme topic (those are excluded by design). Approximate coverage of the technical
programme: **~85–95%**. Forced universal-language invariants: **§2b**. Editorial compression
of related threads: **§8b**. Sources: **§13**.

---

## 0. Project name

**Cairn**

A cairn is a stack of marked stones used as a landmark. In this system, each stone is a
content-addressed semantic artifact; the stack is a branchable history; the path between
cairns is a sharing ledger. The name is intentionally distinct from Granit / GRANITE /
Marble / Phi / Eurisko / MetaLego while preserving their shared intent.

Suggested repository root: `cairn/`

---

## 1. Vision / mission

Build **Cairn**: a self-describing platform where languages are first-class, composable
data; where parse and print are one bidirectional grammar; where programs, claims,
proofs, VMs, and change-sets are typed content-addressed artifacts; where computation
may lower to interaction-net / Δ-net style rewriting when beneficial; where the same
semantic core projects to multiple host targets (Lean, Coq/Abella-style checkers,
Haskell, Scala, Rust) via a Rosetta interchange layer; and where publication and trust
travel on a local-first, ledger-backed spine (single-node PoA first, distributed
consensus later) — without turning day-one development into a public cryptocurrency.

One sentence of intent:

> *Start from a meta-language and grammar-language; define every other language as
> composable fragments; derive its tools; certify what matters; publish by hash.*

(Primordial bootstrap and recursive language closure are normative — **§2b**.)

---

## 2. Core concepts and vocabulary

Use these terms consistently. Do not collapse them into “just a compiler” or “just git.”

| Term | Meaning |
|------|---------|
| **Fragment** | A composable unit that *provides* / *requires* sorts, constructors, grammar, rules, judgments, services, and preservation claims. Fragments compose by pushout / amalgamation along shared interfaces. Conflicts are errors, not silent overwrites. |
| **Language** | A first-class value: a closed (or partially closed) composition of fragments, with identity by content hash of its canonical form. |
| **Sort / constructor** | Algebraic structure of object-language terms; modes may be `tree` or `graph` (interaction-net agents/ports). |
| **Bidirectional grammar** | One declaration yields both `parse` and `print`. Round-tripping (modulo holes/whitespace policy) is a law, not a hope. No hand-coded per-language parsers/printers in the kernel. |
| **Judgment / rule** | Declarative typing, reduction, elaboration, or search rules compiled to drivers (reduce / search / check). |
| **Artifact** | Typed, immutable, content-addressed object: language, grammar bytecode, source, IR, VM image, claim, axiom, lemma, theorem, proof term, test suite, certificate, branch manifest, change-set, block. |
| **Key / digest** | Dual identity: **content hash** for storage/dedup; **stable semantic key** (valueHash + typeHash, or branch-relative stable ref) for long-lived references. Never use bare untyped hashes as the only reference model. |
| **CAS** | Content-addressed store of canonical artifact bytes. Local-first. |
| **Change / ΔL** | For every language `L` there is a free changes language `ΔL` — including the meta-language and grammar-language. Since `ΔL` is itself a language, `Δ(ΔL)` exists (and so on). Grammars, judgments, queries, migrations, traces, workflows, projections, and policies may themselves be languages and thus carry their own change languages. Edits are terms in `ΔL`, validated, then applied. UI never mutates by side effect. **Forced** invariant, not merely permitted. See §2b; §13 (ΔL / GRANITE). |
| **Branch / selection** | Named views over CAS state; merges prefer semantic commutation (additive/Pijul-like) over textual diffs when footprints commute. |
| **Claim vs proof** | Claims may exist proof-free; theorems eventually carry proof terms. Untrusted machinery proposes; a small kernel certifies. |
| **Kernel gate** | Decidable validation that turns proposals into certified objects (`ValidatedChangeSet`, digests, state roots). Agreement theorems between app recomputation and kernel values are required where both exist. See Granit Lean: §13. |
| **VM / runtime IR** | Executable interpretation of a language: tree evaluator and/or graph/net rewriter. Generic engines only; object languages stay at the meta level. |
| **Δ-net / interaction net** | Optional computation backend: agents, ports, annihilation/commutation rules; affine/linear fragments where sharing is absent by construction when required. See §13 (Δ-nets / computation pack). |
| **Rosetta** | Semantic interchange layer: one typed artifact graph projects to Lean, Abella-style formula/check targets, Haskell, Scala, Rust (and later Coq). Ports are generated views + tests/obligations, not new compilers of those hosts. **Canonical:** `~/granit/ROSETTA/` — not the Phi “Rosetta OS” (§13). |
| **Ledger** | Append-only, hash-linked publication spine: transactions, identities/signatures, content-addressed state root, PoA blocks. Records hashes, branch heads, signatures, and policy certificates — not every artifact body. See §13 (GRANITE sharing-ledger, Marble, Granit Rust). |
| **Transcript** | Executable script driving CAS, branches, builds, ledger append, Rosetta projections, and assertions (for demos and CI). |
| **Meta-language / grammar-language** | Primordial bootstrap pair: a meta-language, and a grammar-language defined using it; every other language (and most tooling) is defined or mechanically derived from those. See §2b; §13 (Phi, GRANITE, Kiwi / MetaLego). |
| **Surface / encoding** | A serialization or projection of a language (UTF-8/UTF-16 text, JSON/XML, XLS/PDF/images, Rosetta ports, …). A “foreign format” is another surface **inside** the language system — not outside it. See §2b. |
| **Language capability bundle** | Uniform associates of a language where applicable: grammar/surfaces, interpreters, ΔL, projections, judgments, proof obligations, traces, migrations, queries, laws, provenance, trust policies, effects, workflows, import/export formats. See §2b. |

---

## 2b. Universal Language Closure and Bootstrap

Architectural invariants. Treat violations as design bugs.

### Primordial bootstrap

1. Start with a **meta-language**.
2. Start with a **grammar-language**, itself defined using the meta-language.
3. From those, define every other language (as composable fragments / closed compositions).
4. Tooling around a language is **language-defined** or **mechanically derived** — not a permanent hand-written privilege of the host.

Self-description (Phase 7) is the staged realization of this invariant; host engines may seed bootstrap once, then migrate definitions into fragments (§7).

**Implementation note:** Cairn ships the strict two-file meta/grammar split —
`languages/meta.cairn` (composition IR) and `languages/grammar.cairn`
(grammar-authoring vocabulary), loaded via `runtime.PackLoader` / Meta
activation in handlers like any other pack, `meta requires grammar` to
compose into the one bootstrap grammar. See
[docs/bootstrap.md](docs/bootstrap.md) §7 and
[docs/assumptions.md](docs/assumptions.md) §11.

### Recursive universality (forced)

- Every language `L` has a free changes language `ΔL`.
- The meta-language and grammar-language each have changes languages.
- `ΔL` is itself a language ⇒ `Δ(ΔL)` (and so on).
- Grammars, judgments, queries, migrations, traces, workflows, projections, and policies can themselves be languages and therefore have their own change languages.

The platform must **force** this closure, not merely allow it. See vocabulary **Change / ΔL**; prior art §13 (ΔL / GRANITE, repository).

### Surfaces / foreign formats

- Text is a binary serialization (e.g. UTF-8 or UTF-16).
- JSON and XML are specialized textual formats.
- XLS, PDF, images, and similar binaries are alternative serializations or projections.
- A “foreign format” is another **surface/encoding inside** the language system — import/export, projection, or grammar surface — not an escape hatch outside it.

Canonical bytes (§4, §7) and Rosetta ports (§4) are instances of this model, not exceptions.

### Uniform language-associated capability model

For every language where applicable, treat the following as one coherent bundle (not scattered one-offs):

| Capability | Role |
|------------|------|
| Grammar / surfaces | Bidirectional parse/print and other encodings |
| Interpreters | Tree and/or graph/net evaluation drivers |
| Change languages | Free `ΔL`, recursively |
| Projections | Rosetta and other host/views |
| Judgments | Typing, well-formedness, validation |
| Proof obligations | Claims, theorems, certificates |
| Traces | Executable or auditable execution records |
| Migrations | Revision morphisms between language versions |
| Queries | Language-level retrieval / search |
| Laws | Invariants and preservation claims |
| Provenance | Origin and derivation of artifacts |
| Trust policies | What the kernel/ledger accepts |
| Effects | Effect vocabulary projected or interpreted |
| Workflows | Multi-step procedures as language terms |
| Import / export formats | Surfaces for foreign encodings (§ above) |

Not every language ships every row on day one; the **model** is uniform. Exemplars (SDS/PKI/Law/Search/…) exercise subsets (§5b).

---

## 2c. Repository Substrate and Hosted Languages

Amended <this revision>. Names the *consequence* of §2b explicitly, since it is
easy to misread the capability model above as "every language brings its own
versioning" rather than what it actually forces.

**The repository layer is not a hosted application.** CAS, content/typed-key
dual identity, branches and selections, `ΔL` and its recursive closure
(`Δ(ΔL)`, …), change dependencies and commutation, semantic merge, conflict
artifacts, migrations, provenance, and ledger-backed publication are **native
platform substrate** — they exist once, generically, and every
language gets them by construction (§2b's "uniform capability model" is the
proof: `Delta.deltaOf` derives `ΔL` for *any* `ComposedLanguage`, no per-
language reimplementation). No language — including the domain packs in §5b —
defines its own bespoke versioning, storage, or merge story. This substrate is
**Pijul-influenced** (changes as primary objects, explicit dependencies,
commutation, conflict preservation) more than Git-influenced (content
addressing, immutable objects, distributed transfer — both present, but as
properties of the substrate, not as "Cairn reimplements Git"). Git/Pijul
themselves, if ever touched, are external import/export/hosting *bridges*
(§2b "surfaces / foreign formats") — they do not define Cairn history.

**Languages and machines are built *on* that substrate, not modeled as
domains that happen to resemble it.** A language like STLC, PKI, SDS, Search,
Riemann, MiniTT, LeanCore (§5b), or Unison Core (§5b) is a fragment composition with its
own grammar/judgments/rules; it automatically inherits CAS storage, `ΔL`,
branches, and provenance from the substrate — it never needs to model "a
repository" itself. Concretely, this is why the Unison pack does not
reimplement Unison's codebase manager (content-addressed defs, name aliases,
patch-as-edit): those properties are just what any Cairn language gets for
free from the Kernel/Core repository substrate, and the pack's job is to
demonstrate that, not rebuild it.

The corrected stack:

```text
┌──────────────────────────────────────────────┐
│ Domain languages and applications             │
│ SDS · Law · PKI · Search · Riemann · …        │
├──────────────────────────────────────────────┤
│ General-purpose hosted languages and machines │
│ Unison Core · MiniTT · LeanCore · STLC        │
│ AffineNet/IcNet                               │
├──────────────────────────────────────────────┤
│ Language workbench (Core + Runtime)           │
│ grammar · judgments · proofs · interpreters   │
│ lowerings · surfaces · ΔL generation          │
├──────────────────────────────────────────────┤
│ Native semantic repository (Kernel/Core/SH)   │
│ CAS · changes · PatchGraph · commutation      │
│ branches · conflicts · migrations · history   │
├──────────────────────────────────────────────┤
│ Certification and distribution                │
│ kernel gates · provenance · policies · PoA    │
└──────────────────────────────────────────────┘
               ↕
     external optimized implementations / bridges
     Lean · HVM · host compilers · Git hosting
```

The repository layer is not another stone on the cairn. It is the geology
beneath the entire cairn.

**Amended <this revision>: executable reference vs. optimized backend.**
The bottom arrow above (`↕ external optimized implementations / bridges`)
is not only "Cairn generates code that targets X" (that's Rosetta, rule
10 below — projection only, never a competing host compiler for X's own
ecosystem). For a system whose *semantics* Cairn wants to own, not just
target, the pattern already proven by `AffineNet`/`IcNet` (§5) is:
Cairn hosts its own executable, kernel-checked reference model of the
relevant fragment — real interaction-net reduction, not prose describing
HVM — while HVM itself remains available as an external, optimized
backend (performance, scheduler, memory layout) that the reference model
can be tested for agreement against. Lean now has its own half of that
pair too: **LeanCore** (§5b, §8b) is a Cairn-native, kernel-checked model
of a Lean-kernel *fragment* — everything MiniTT has (Π types, closed
universes, `Nat`+recursor, definitional equality) plus identity types
(`Eq`/`refl`/`subst`) and a minimal environment of checked declarations
(§8b's "environments, inductive declarations, definitional equality,
theorem checking"), checked by the same generic `Checker`/`Search`
MiniTT already uses. Lean plays both roles at once now, same as HVM: a
Rosetta target for the full native implementation (elaboration, macros,
mathlib), and a Cairn object language (LeanCore) for the kernel fragment
Cairn itself checks and runs. Neither role replaces the other, and this
does not relax rule 10 — LeanCore is still not "a Lean compiler," it's a
language like MiniTT, checked the same way, with its own honestly-scoped
limits (`subst`/transport rather than full path-induction `J`; opaque
declarations, no delta-unfolding; no user-declarable inductives; no
universe polymorphism; no real Lean 4 syntax).

---

## 3. Architecture modules (live)

Enforce an import DAG. Lower layers must not depend on upper layers. Domain
examples must not leak into the kernel. **L0–L6 ownership labels are historical**;
the live boundary is Kernel / Core / System / User / Runtime.

```text
kernel           — digests, canonical forms, artifact types, fragment laws,
                   change validation, certificates, authority models
                   (AuthorizedRequest ≠ AuditedRequest), pure ledger transition

core             — grammar engine, Meta elaboration, ΔL / PatchGraph, search &
                   tactics, tree + net engines, Rosetta projection, policy eval
                   (pure — no I/O)

system-interface — effect contracts (CAS, FS, process, …)

system-handler   — privileged I/O: DiskCas / Branches, handlers, PoA node,
                   AuthorityGate, EffectContext, RuntimeEffectRegistry,
                   RevocationView

user             — language packs / policies / workflows; never imports handlers

runtime          — PackLoader, EffectBootstrap, WorkflowRunner (composition root)

proof / rosetta / surface / examples / tests
                 — aggregation & façades above the DAG (not CAS/Meta owners)
```

**Import rule:** `kernel ← core ← {system-interface} ← system-handler`;
`user ↛ system-handler`; `runtime` ties user + handlers.

**Bootstrap note:** Kernel/Core/Runtime host the primordial meta-language +
grammar-language pair (§2b). Other languages and tooling are defined or
mechanically derived from them; domain packs stay in `examples/` / `languages/`
(§5b).

---

## 4. Non-negotiable requirements

Distilled from the family of prior prompts; treat violations as bugs.

1. **Languages are data.** Object languages, checkers, and examples are fragments/artifacts — not privileged host-language builtins baked into the kernel.
2. **Generic grammar interpreter only.** Parse/print take a grammar artifact as a parameter. No per-object-language hand parsers in Kernel/Core.
3. **Composition by interface.** Fragments declare `provides` / `requires` / `excludes`. Composition is pushout/amalgamation; order of import does not matter when interfaces agree.
4. **Bidirectionality is a law.** Grammar productions define parse and print together; tests include round-trips on every shipped example language.
5. **Dual identity.** Content hashes + stable typed keys / branch-relative refs.
6. **Proposed vs certified.** Parsers, indexers, UIs, and network code propose; only the kernel certifies identity, validated change-sets, and ledger transitions.
7. **Proofs are first-class but staged.** MVP may ship claims + certificates of testing; a proof-term check path must exist as a designed slot and appear by Phase 2.
8. **Recursive ΔL for edits.** Every language `L` — including meta-language, grammar-language, and any `ΔL` — has a free changes language. Mutations are terms in that language, not opaque file overwrites. See §2b.
9. **Ledger is publication, not runtime DB.** Local CAS is the working store; the ledger records publication, trust, and heads.
10. **Rosetta is projection, not replacement.** Do not invent “Haskell++ compilers”; generate ordinary host projects plus obligations/tests. This governs Rosetta *ports* specifically — it does not bar Cairn from hosting its own kernel-checked object language in an external system's lineage (§2c amendment: "executable reference vs. optimized backend"), the way `AffineNet`/`IcNet` already do for HVM's net semantics and MiniTT does for a slice of Lean's.
11. **Domain packages stay out of kernel.** SDS, PKI, Bend, Unison-inspired packs, physics, heuristics, etc. live under `examples/` / `languages/` (§5b) — never in Kernel/Core.
12. **Deterministic canonicalization.** Same semantic artifact ⇒ same bytes ⇒ same digest (documented normalization rules). Surfaces and foreign formats are encodings inside the language system (§2b) — still subject to canonicalization where artifacts are stored.
13. **Tests before features.** Each phase lands with an acceptance suite; do not skip phases.
14. **Primordial bootstrap.** Start from a meta-language and a grammar-language (defined using the meta-language); define every other language from those; derive or language-define tooling. See §2b.
15. **Uniform capability model.** Grammar/surfaces, interpreters, ΔL, projections, judgments, obligations, traces, migrations, queries, laws, provenance, trust, effects, workflows, and import/export are one coherent per-language model (§2b) — not ad-hoc one-offs.
16. **Parity with summarized projects’ top-level.** For each major source Cairn claims to distill (§13), match that project’s README-advertised / flagship surface (packs, CLIs, demos) — not every deep experiment. Thin name-drop stubs are not parity. Track the matrix in [docs/architecture.md](docs/architecture.md).

---

## 5. Flagship vertical slices (examples, not kernel)

Ship these as `examples/` packs to prove the platform. Implement in order:

| Pack | Purpose |
|------|---------|
| **STLC** | Sorts, grammar, β-reduction, simple types, bidirectional surface, ΔL edits |
| **Claims** | Proof-free properties + test certificates on STLC programs |
| **AffineNet** | Small Δ-net / interaction fragment (Fan/Eraser only; no replicator) showing graph-mode sorts |
| **RosettaQuickSort** | One verified-or-claimed sorting example projected to ≥2 ports (e.g. Lean + Haskell or Scala) |
| **Publish** | Transcript that builds STLC artifact, appends a PoA block, and fetches by hash on a second local “node” process |

Do not let example domains drive kernel APIs.

---

## 5b. Exemplar languages (envelop these)

These are **first-class case studies** Cairn should eventually support as language packs or explicitly learn from. They are not kernel features. Characterizations follow GRANITE’s own packs/docs (SDS, PKI, Bend); Unison and Lean/HVM inform Cairn as external systems whose ideas get built as real, honestly-scoped hosted languages (§2c) — Unison Core, MiniTT, and LeanCore — rather than reimplemented or merely name-dropped. Prefer thin, honest slices over hollow stubs. Full path map: **§13 Sources & references**.

| Exemplar | Role in Cairn | Accurate characterization (source-grounded) |
|----------|---------------|-----------------------------------------------|
| **PKI** | Domain pack (early, after STLC+ledger); proves language-agnostic kernel | GRANITE’s **first** application pack: certificate-`Registry` object language with ΔPKI (`IssueCertificate` / `RevokeCertificate`), `ChainValidationJudgment` over real Ed25519 chains, ledger trust-anchor publish. SDS depends on PKI for encryption certs — not the reverse. **See:** `~/GRANITE/docs/pki.md`; impl `~/GRANITE/examples/pki/` (`languages/Pki.scala`, `PkiChanges.scala`, `ChainValidation.scala`). |
| **SDS** | Flagship *domain* pack (after PKI); non-programmer object language + ΔL + studio | **Safety Data Sheet** authoring (chemical regulatory SDS — not “software design something”). An SDS is **not** a flat document: compiled view of typed objects (`Substance`, `Mixture`, `Product`, shadows, multilingual phrases). Acetone tutorial spine; `LanguagePack` with ΔSDS. **See:** flagship prose `~/GRANITE/PROMPT.md` §11–14; studio `~/GRANITE/docs/sds-studio.md`; impl `~/GRANITE/examples/sds/` (`languages/Sds.scala`, `SdsChanges.scala`, `tutorial/SdsTutorial.scala`, `chemicals/Chemicals.scala`). |
| **Bend** | Computation-surface target (after AffineNet / QDIC-shaped nets) | In GRANITE, **Bend** is a deferred **surface profile** (with Kind/HVM) over QDIC — **no** `examples/bend` pack. Spec-only naming + deferral lists. **See (spec only):** `~/GRANITE/examples/computation/PROMPT.md` (Bend/Kind/HVM profiles; SS13/SS16/SS24). Implemented net spine to learn from first: same dir’s languages + `~/GRANITE/examples/computation/`. |
| **Unison Core** | General-purpose hosted language (§2c), peer to STLC/MiniTT — not a domain pack | Amended <this revision>: no longer "inspirational only" — a genuinely new, Cairn-native language (ADTs, pattern matching, a minimal ability/effect system), still **not** a Unison fork/reimplementation, built on Cairn's own substrate rather than modeling Unison's codebase manager (that machinery is just what §2c's substrate already gives any language for free — CAS, alpha-invariant identity, patch-as-ΔL — which the earlier M48 "ideas pack" phase (`examples/unison/`) proved out before this pack existed; `examples/unison/Unison.scala`'s `Store` is itself backed by `system-handler` CAS, not a hand-rolled map, for the same reason). **See:** modeling-as-fragments precedent `~/Downloads/granit-rust/PROMPT.md` §20; Lean IR formalization `~/UnisonAbella/` (`README.md`); impl `languages/unisoncore.cairn`, `examples/unison/`. |
| **MiniTT** | General-purpose hosted language (§2c), the "Formal-methods IR ladder" rung (§8b) climbed honestly | A minimal, closed dependent type core (a 2-level, non-cumulative universe hierarchy, Π types, one hardcoded `Nat` inductive with its recursor) checked by the SAME generic kernel `Checker`/`Search` as STLC/PKI's judgments — not a Lean reimplementation, not full CIC (§8 anti-goal), not claiming Lean-surface *or* Lean-kernel compatibility. **See:** `languages/minitt.cairn`, `examples/minitt/`. |
| **LeanCore** | General-purpose hosted language (§2c), the next "Formal-methods IR ladder" rung (§8b) — §2c's "executable reference vs. optimized backend" amendment's Lean-kernel fragment | Amended <this revision>: everything MiniTT has, plus identity types (`Eq`/`refl`/`subst` — transport, not full path-induction `J`, a deliberately simpler typing rule) and a minimal environment of checked declarations (opaque once checked, no delta-unfolding — closer to `axiom`/`theorem` than `def`). Still not user-declarable inductives, not universe polymorphism, not real Lean 4 syntax — Lean *itself* remains a Rosetta *projection* target for the full native implementation (§4.10) alongside this. **See:** `languages/leancore.cairn`, `examples/leancore/`. |

**Dependency hint (from GRANITE):** `PKI → Law → SDS` — Law pack at `~/GRANITE/examples/law/`; Bend on the **computation** spine (`~/GRANITE/examples/computation/`), not SDS. Unison Core, MiniTT, and LeanCore sit at the "general-purpose hosted language" layer (§2c) — peers to STLC, not domain ADTs — and all three inherit the Kernel/Core/System-handler repository substrate rather than modeling any part of it themselves.

Wire into later phases: after Phase 5, minimal **PKI**; then thin **SDS**; **Bend** only once net lowering is real; **Unison Core**, **MiniTT**, and **LeanCore** as real language packs once the substrate (§2c) and judgment/reduction engines they all build on (`Checker`/`Search`/`TreeEngine`) are proven by STLC/PKI.

---

## 6. Phased roadmap

### Phase 0 — Skeleton (week-scale)

- Repo layout matching Kernel/Core/System/User/Runtime (even if some modules are thin).
- Choose **one** host implementation language for the engines (prefer **Rust** or **Scala 3**; document the choice and stick to it for Kernel/Core). Rosetta *targets* remain multi-host.
- CAS on local disk; digest + typed key model; CLI: `cairn hash`, `cairn put`, `cairn get`.
- Empty `LanguageDef` / `Fragment` IR with serialization to canonical bytes.
- Acceptance: round-trip store/load; golden digests for fixtures.

### Phase 1 — MVP Language Workbench

- Fragment IR: sorts, constructors, grammar, reduction rules, `provides`/`requires`.
- Pushout composition with clear error messages on conflicts.
- Generic bidirectional grammar interpreter.
- Tree-mode evaluator for STLC example.
- ΔL for STLC: at least add/replace definition and rename-with-footprint.
- Branch manifests; append-only local history (no multi-node yet).
- Acceptance: compose base+STLC fragments; parse/print/eval identity and Church bools; ΔL edit produces new digest; round-trip grammar tests green.

### Phase 2 — Proof slot + certificates

- Artifact kinds: `Claim`, `Theorem`, `ProofTerm`, `TestSuite`, `Certificate`.
- Independent **check** driver for proof terms (start small: ND or HOAS-free deep embedding of a tiny logic sufficient for STLC typing derivations).
- Optional tactic/goal engine may remain thin; must not be required to *check* proofs.
- Proof-free claims remain valid with test certificates.
- Acceptance: STLC typing derivation as proof term checks; forged proof rejected; claim+tests path still works without proofs.

### Phase 3 — Graph / Δ-net computation

- Graph-mode sorts: agents, ports, interaction rules.
- AffineNet example: annihilation/decay rules; structural absence of replicators.
- Lowering story documented: which surface terms become nets (even if only a subset).
- Acceptance: net reduction suite; well-formedness judgments; no replicator constructible in AffineNet ADT/spec.

### Phase 4 — Rosetta ports

- Rosetta declaration vocabulary (package/module, data, def, rel, effect, theorem, target, …) sufficient for QuickSort-style example *or* a thinner STLC+sort obligation.
- Emit at least **two** ports: (1) Lean 4 skeleton + statements, (2) Haskell or Scala with generated tests.
- Ports consume Cairn artifacts; they do not fork semantics.
- Acceptance: one transcript builds artifact → emits ports → host tests or `lake build` / `cabal test` / `sbt test` as applicable.

### Phase 5 — Ledger-backed publication (single-node PoA)

- Transaction language; identities and signatures (dev keys OK).
- Atomic ledger transition + content-addressed state root.
- PoA blocks; publication of branch head + artifact digests + policy certs.
- Explicitly **out of scope for this phase:** public tokenomics, open mining, BFT finality, peer discovery. Design hooks only.
- Acceptance: transcript creates local blockchain, publishes STLC pack, second process verifies block and retrieves blobs from CAS by digest.

### Phase 6 — Distribution hooks (design + thin impl)

- Fetch missing blobs by hash; pull-based sync of patches/artifacts.
- Document future: gossip, fork choice, BFT.
- Optional useful-work market hooks remain stubs.
- Acceptance: two local nodes sync a published head; divergence surfaces as competing heads without silent corruption.

### Phase 7 — Hardening

- Threaded bytecode / decision-tree compilation for rules (optional performance path).
- Agreement theorems or property tests between kernel digests and app recomputation.
- Self-description: Cairn’s own fragment IR expressible as a Cairn language pack (bootstrap), even if the host engines remain native — toward the primordial meta-language + grammar-language pair (§2b).

### Phase 8 — Exemplar packs (envelop §5b)

- **PKI (minimal):** mirror `~/GRANITE/examples/pki/` — `Registry` + issue/revoke ΔL + chain-validation + ledger trust-anchor publish. See `~/GRANITE/docs/pki.md`.
- **SDS (thin slice):** mirror `~/GRANITE/examples/sds/` — substance + shadow + one phrase path + ΔSDS override (+ optional render). Disambiguation: Safety Data Sheet — `~/GRANITE/PROMPT.md` §11, `~/GRANITE/docs/sds-studio.md`.
- **Bend (when nets are ready):** gap-analyze against **spec only** `~/GRANITE/examples/computation/PROMPT.md` (Bend/Kind/HVM); implement against real nets in that pack — no empty `examples/bend`.
- **Unison Core (real pack, §2c):** a genuine ADT/pattern-match/ability language on Cairn's substrate, not the CAS-ideas-only stage; fragment modeling precedent in `~/Downloads/granit-rust/PROMPT.md` §20; IR notes `~/UnisonAbella/`.
- **MiniTT (real pack, §2c/§8b ladder):** minimal dependent core (Π types, one hardcoded `Nat` inductive, a closed 2-level universe hierarchy) checked by the existing generic `Checker`/`Search`; not full CIC, not Lean-surface-compatible.
- **LeanCore (real pack, §2c amendment/§8b ladder):** MiniTT plus identity types (`Eq`/`refl`/`subst`) and a minimal checked-declaration environment — the Lean-kernel-fragment rung; not user-declarable inductives, not universe polymorphism, not real Lean 4 syntax.

---

## 7. Implementation constraints

- **Host engines are generic.** Rust/Scala/etc. implement CAS, grammar interpreter, composition, checkers, ledger transition — not STLC-as-handwritten-AST forever. Bootstrap may seed the meta-language / grammar-language (and STLC) in host code *once*, then migrate definitions into fragments (§2b).
- **Canonical bytes.** Specify endianness, map key order, binder representation, and string encoding. Provide a `cairn canon` command.
- **Errors are structured.** Composition, parse, and validation failures cite fragment names, interface paths, and digests.
- **Transcripts are CI.** Every phase adds a transcript under `transcripts/` that a fresh checkout can run.
- **Documentation.** `docs/bootstrap.md` explains how to go from empty CAS to published STLC in one sitting.

---

## 8. Out of scope / anti-goals

Do **not** build:

- A public cryptocurrency, token, or mainnet.
- A monolithic new general-purpose programming language meant to replace Scala/Rust/Haskell for app development.
- Hand-written parsers per object language in the kernel.
- Talend-style “model → generated code becomes source of truth” MDA.
- Baking domain examples (chemistry SDS, particle physics, org ACL demos) into Kernel/Core.
- Requiring full CIC / cubical type theory / self-hosting proof of consistency on day one.
- Replacing Lean/Coq/Abella; Cairn *projects* to them.
- Silent textual merge as the only merge story once semantic footprints exist.
- Using the ledger as the hot path for every local edit.
- Chronological conversation logs, personal/non-programme topics, or every abandoned prototype — this document is an implementation constitution, not an archive (§ preamble).

---

## 8b. Compressed / not preserved in detail

Editorial scope: the following threads are **absorbed as architectural residue** only. Do not expand this prompt into those essays; read the cited sources when implementing the related surface.

| Compressed thread | Pointer |
|-------------------|---------|
| Formal-methods IR ladder (λ → STLC → polymorphism → dependent types / universes / identity / effects / domain rules); bridge vs relation vs formula vs judgment vs presentation. **The dependent-types/universes rung is climbed by MiniTT (§5b, §2c)**, a thin, honest slice (Π types, one hardcoded inductive, a closed 2-level hierarchy) — **and the identity-types rung by LeanCore (§5b, §2c amendment)**, adding `Eq`/`refl`/`subst` and a minimal checked-declaration environment. Not the full ladder: polymorphism, general effects, user-declarable inductives, universe polymorphism remain compressed here. | §13: Aldo, MLTS, Granit Lean, Eurisko |
| Haskell++ / Scala++ as ordinary hosts under testing/verification discipline; laziness, bottoms, effects, host interop nuances | §13: Rosetta (interchange); do **not** invent new host compilers (§4.10) |
| Detailed QuickSort / `Ord` / `Nat` / effects Rosetta example | `~/granit/ROSETTA/examples/QuickSortOrdEffects.rosetta` (§13) |
| Runtime experiments: StackVM, APEX, HVM5, Mogensen-style interpretation, QDIC/Kind/Bend/HVM surface relationships, threaded bytecode / decision trees | §13: HVM / IC, Δ-nets / QDIC, Bend; Phase 3 + Phase 7 |

---

## 9. Success criteria

Cairn is successful when all of the following hold:

1. **Composition:** At least two non-trivial fragments compose via pushout into a working language with tests.
2. **Bidirectional surface:** Every example language round-trips parse ∘ print on its golden suite.
3. **Semantic CAS:** Artifacts are retrieved by digest; dual keys prevent untyped-hash confusion.
4. **Certified path:** At least one proof-term (or typing derivation) is independently checked by the kernel check driver; a tampered term fails.
5. **Δ-net optional path:** AffineNet (or equivalent) reduces correctly under graph-mode rules.
6. **Polyglot projection:** One semantic artifact emits ≥2 Rosetta ports that build/test in their hosts.
7. **Ledger publication:** A single-node PoA ledger publishes a branch head; a second local consumer verifies and materializes artifacts by hash.
8. **Layering:** Dependency graph matches Kernel/Core/System/User/Runtime; examples do not import into the kernel.
9. **Reproducibility:** A clean machine can run `transcripts/mvp.cairn` (name flexible) end-to-end from the README.
10. **Exemplars path:** At least **PKI** or **SDS** exists as a real language pack with ΔL + judgment tests; **Bend** and **Unison** are either thin real slices or documented deferred targets with no fake stubs (§5b).
11. **Universal closure path:** Meta-language + grammar-language bootstrap is designed (and at least stubbed by Phase 7 self-description); ΔL applies recursively where languages exist; at least one non-text surface/encoding is modeled as a projection or import/export of a language (§2b).

---

## 10. Suggested repository layout

```text
cairn/
  PROMPT.md                 # this file (or a short pointer to it)
  README.md
  docs/
    bootstrap.md
    vocabulary.md
    ledger.md
    rosetta.md
    exemplars.md            # SDS / PKI / Bend / Unison notes
  kernel/                   # semantic TCB
  core/                     # pure proposals (grammar, ΔL, nets, …)
  system-interface/         # effect contracts
  system-handler/           # privileged I/O + CAS/branches/PoA
  user/                     # language packs (no handler imports)
  runtime/                  # PackLoader / EffectBootstrap / WorkflowRunner
  proof/                    # certification helpers
  rosetta/                  # scaffold emit façade
  surface/                  # CLI + transcript + LSP + explorer
  examples/
    stlc/
    claims/
    affine-net/
    rosetta-quicksort/
    pki/                    # §5b exemplar
    sds/                    # §5b exemplar
    bend/                   # §5b only when net lowering exists
    unison/                 # §5b/§2c real pack
    minitt/                 # §5b/§2c real pack
    leancore/               # §5b/§2c amendment real pack
  transcripts/
  tests/
```

Adapt crate/module names to the chosen host; keep the layer boundaries.

---

## 11. Working style for the implementer

1. Read this prompt end-to-end; treat it as normative.
2. Pick the host language for engines; record the decision in `README.md`.
3. Implement Phase 0 → 1 fully before starting Phase 2.
4. Prefer small certified interfaces over large unfinished frameworks.
5. When prior projects disagree (Lean-first Granit vs Rust Granit vs Scala GRANITE vs Phi), **prefer**: content-addressed fragments + bidirectional grammar + primordial meta/grammar bootstrap (§2b) + recursive ΔL + kernel gates + local PoA ledger + Rosetta projections — and keep host choice pragmatic.
6. Do not ask clarifying questions that block progress; state assumptions in `docs/assumptions.md` and proceed.
7. Keep golden-artifact digests and transcript results current in `docs/architecture.md` as work lands — one present-tense source of truth, not a separate dated scorecard.

---

## 12. One-paragraph elevator (for README)

Cairn is a landmark for semantic software: you compose language fragments into a typed,
hash-addressed definition; the same definition yields parser and printer, evaluator or
interaction-net machine, claims and checked proofs, and projections into familiar proof
assistants and functional languages; local work stays in a content-addressed store; when
you publish, a small proof-of-authority ledger records what was certified and where to
fetch it — so meaning, not merely text, is what moves between machines.

---

## 13. Sources & references

Lookup map for concepts named in this prompt. Paths are under `/home/patrick/` (shown with a `~/` prefix). Verified present when this section was written. Prefer reading the cited file over guessing from the name alone.

| Concept | Disambiguation | Read first |
|---------|----------------|------------|
| **SDS** | **Safety Data Sheet** (chemical regulatory docs), GRANITE flagship — *not* an unrelated SDS acronym | Spec: `~/GRANITE/PROMPT.md` (§11–14, Acetone). Studio: `~/GRANITE/docs/sds-studio.md`. Arch chain: `~/GRANITE/docs/architecture.md`. **Implemented pack:** `~/GRANITE/examples/sds/` — esp. `~/GRANITE/examples/sds/src/main/scala/granite/examples/sds/languages/Sds.scala`, `SdsChanges.scala`, `tutorial/SdsTutorial.scala`, `chemicals/Chemicals.scala` |
| **PKI** | Certificate registry language pack; first GRANITE app; SDS depends on it | Doc: `~/GRANITE/docs/pki.md`. **Implemented pack:** `~/GRANITE/examples/pki/` — `languages/Pki.scala`, `PkiChanges.scala`, `ChainValidation.scala` |
| **Law** | Middle pack in `PKI → Law → SDS` | **Implemented:** `~/GRANITE/examples/law/` |
| **Bend** | Deferred Bend/Kind/HVM *surface profile* over QDIC — **not implemented** as a pack | **Spec only:** `~/GRANITE/examples/computation/PROMPT.md` (search “Bend/Kind/HVM”). Learn nets from **implemented** computation pack: `~/GRANITE/examples/computation/` |
| **Δ-nets / AffineNet / QDIC** | Interaction-net computation spine in GRANITE | Spec+impl: `~/GRANITE/examples/computation/PROMPT.md` and `~/GRANITE/examples/computation/` |
| **ΔL / free changes** | Per-language change language (e.g. ΔSDS, ΔPKI) | Platform: `~/GRANITE/PROMPT.md`; repo: `~/GRANITE/docs/repository.md`; examples under each pack’s `*Changes.scala` |
| **GRANITE (platform)** | Scala language-oriented artifact platform + sharing ledger | `~/GRANITE/PROMPT.md`; `~/GRANITE/docs/architecture.md`; `~/GRANITE/docs/sharing-ledger.md` |
| **Granit (Lean)** | Lean kernel “blockchain of semantic code/proofs” | Spec: `~/granit/GRANIT/PROMPT.md`; kernel: `~/granit/GRANIT/GRANIT-KERNEL.md`, `~/granit/GRANIT/Kernel.lean` |
| **Granit (Rust)** | Rust MetaLego + PoA ledger + Unison-as-fragments | Spec: `~/Downloads/granit-rust/PROMPT.md` (Unison §20) |
| **Marble** | Blockchain-backed CAS languages/proofs design | Spec: `~/marblego/PROMPT.md`; related: `~/HVM/MARBLE.md`, `~/HVM/marble/` |
| **Rosetta (interchange)** | Typed artifact → Lean/Coq/Abella/Haskell/Scala | Spec: `~/granit/ROSETTA/PROMPT.md`; example: `~/granit/ROSETTA/examples/QuickSortOrdEffects.rosetta` |
| **Rosetta (Phi OS)** | Different “Rosetta”: Phi polyglot memory/OS sketch | Spec: `~/IdeaProjects/phi-autonomous/specs/phi-core/specs/rosetta/PROMPT.md` — do **not** confuse with `~/granit/ROSETTA/` |
| **Kiwi / MetaLego / fragments** | Cubical workbench + STLC plugins / composable fragments | Kiwi: `~/IdeaProjects/lego/toy/synergical/docs/PROMPT.md`; STLC: `~/IdeaProjects/lego/toy/synergical/lego/stlc/PROMPT.md`; universal fragments: `~/Projects/lego/PROMPT.md`, `~/Documents/Notes/PROMPT.md`; MetaLego engines: `~/Downloads/granit-rust/` |
| **Phi** | Specs-as-programs / bootstrap meta-language | `~/IdeaProjects/phi-autonomous/PROMPT.md`; demo: `~/IdeaProjects/phi/PROMPT.md` |
| **Eurisko / Foundry** | Lattice-composable meta-language + projections | `~/Projects/eurisko.ai/PROMPT.md`; app sketch: `~/eurisko/eureka/PROMPT.md` |
| **HVM / IC** | Interaction combinators / rewriting / proof-search | Framework: `~/HVM/PROMPT.md`; visualizer: `~/Projects/hvm/`; SweetCLIPS→HVM: `~/Projects/hvm5/PROMPT.md` |
| **Unison** | Content-addressed defs — external inspiration AND (§2c, amended) a real Cairn pack, Unison Core, not a fork | Spec modeling: `~/Downloads/granit-rust/PROMPT.md` §20; Lean IR: `~/UnisonAbella/README.md`; impl `languages/unisoncore.cairn`, `examples/unison/` |
| **MLTS** | PL-metatheory proof assistant / `.lang` schemas | `~/mlts/mlts-scala/PROMPT.md` |
| **Aldo / delta-nets roadmap** | Pur → CIC → proof assistant design | `~/aldo/PROMPT.md`; SemGuS: `~/aldo/SemGuS/PROMPT.md` |

**Tip:** When two projects share a name (Granit vs GRANITE, Rosetta vs Rosetta), open the path in this table — do not rely on the label alone.
