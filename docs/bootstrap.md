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

`Delta.deltaOf(stlc)` mechanically derives the changes language `Δstlc` — a real
`ComposedLanguage` whose ops are module-level (`add`/`replace`/`remove`/`rename`)
plus structural path edits (`edit … at [path] = …`), with forced recursive
closure (`deltaOf(Δstlc) = ΔΔstlc`, and so on). The transcript's

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

## 7. Self-description (fixpoint achieved, §2b)

Self-description fixpoint achieved: `workbench/Meta.scala` is a fused meta
surface (fragment IR + grammar vocabulary) that can describe and reconstruct
itself — `languages/meta.cairn` matches the seed digest-for-digest
([docs/assumptions.md](assumptions.md) §11; STATUS-2 Wave H). A separate
`grammar.cairn` split remains deferred.

What stays host-backed:

- the initial Scala seed (`Meta.fragment`, STLC fragment constructors)
- STLC/meta `.cairn` files as checked-in **canonical mirrors** emitted from Scala
  via `cairn emit-languages` (not yet the runtime source of truth)

Exemplar packs (PKI / Law / SDS / Search) are `.cairn` source of truth, loaded
at runtime by `PackLoader`.
