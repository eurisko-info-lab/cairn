# Bootstrap: empty CAS to published STLC in one sitting

Everything below runs from a fresh checkout with only a JDK (17+) and sbt.

## 0. One command

```bash
sbt "examples/runMain cairn.examples.Main transcript transcripts/mvp.cairn"
```

That transcript is the whole story. The rest of this document explains what it does,
step by step, in terms of the layers.

## 1. Fragments compose into a language (L0/L1)

STLC ships as five fragments — `base`, `types`, `lambda`, `booleans`, `typing` —
each pure data: sorts, constructors (with binder positions), grammar alternatives,
rewrite rules, judgment rules ([Stlc.scala](../examples/src/main/scala/cairn/examples/stlc/Stlc.scala)).

`Compose.compose("stlc", fragments)` amalgamates them by pushout: shared identical
definitions unify, same-name-different-definition is a structured `ComposeError`
citing both fragment names. Order of composition does not change the language digest.

## 2. One grammar engine, both directions (L1)

The composed `GrammarSpec` is interpreted by ONE generic lexer, parser, and printer
(`workbench/Grammar.scala`). There is no STLC-specific parsing code anywhere.
`RoundTrip.check` asserts `parse(print(t)) == t` — a law, enforced per grammar in CI.
`RoundTrip.put` / `Concrete.splice` edit one spanned subtree while preserving
bytes outside the span (format-preserving lens slice). A static left-recursion
checker runs before every parse.

## 3. Evaluation (L3)

`TreeEngine` interprets rewrite rules as data. β-reduction is the *datum*
`app(lam($x,$T,$b), $v) → $subst($b,$x,$v)`; capture-avoiding substitution is a
generic kernel operation driven by the language's binder table.

## 4. Edits are ΔL terms (L1, §2b)

`Delta.deltaOf(stlc)` mechanically derives the free changes language `Δstlc` — a real
`ComposedLanguage` with its own grammar (so `deltaOf(Δstlc) = ΔΔstlc` exists, and so
on: forced recursive closure). The transcript's

```text
delta "{ add id = fun x : Bool . x ; }" ;
delta "{ rename id to ident footprint [] ; }" ;
```

parses each edit in `Δstlc`, validates it (rename demands an exact reference
footprint), and produces a new module digest plus a `ValidatedChangeSet` artifact.

## 5. Claims and proofs (L2)

Claims may be proof-free with test certificates (`claim id_true … ;`). The certified
path exists too: STLC typing derivations are proof terms checked by the independent
`Checker` (see `Phase2Suite`); forged or tampered derivations fail.

## 6. Publication (L5)

`publish main ;` stores fragment/language/module bodies in the local CAS, then appends
one PoA block whose transactions record *digests and heads only*. `fetch main ;` has a
second node pull blocks + blobs by hash, replay the chain through the pure
`LedgerKernel`, and materialize the branch head from its own CAS.

## 7. Self-description (staged, §2b)

`workbench/Meta.scala` defines Cairn's fragment IR as a Cairn surface language: a
fragment written as text parses, elaborates, and composes byte-identically to the
host-constructed value. Grammar productions, rewrite rules and judgments are still
host-seeded — that is the honestly-documented remaining step of the primordial
meta-language/grammar-language bootstrap (see docs/assumptions.md).
