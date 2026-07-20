# Assumptions (§11.6)

Assumptions taken to keep progress unblocked, stated rather than asked.

1. **Host = Scala 3.3 LTS, sbt, JDK 17+.** Zero runtime deps beyond the JDK
   (SHA-256 via `MessageDigest`, Ed25519 via `java.security`); munit for tests.
2. **Canonical bytes**: bespoke deterministic TLV encoding (big-endian lengths,
   UTF-8 strings, maps sorted by unsigned byte order of keys, duplicate keys
   rejected) rather than CBOR — smaller surface, fully specified in `Canon.scala`.
3. **typeHash** of an artifact = digest of (kind, structural head tag of the body).
   A richer structural type fingerprint can replace it without changing `TypedKey`.
4. **Grammar engine subset**: Elem vocabulary covers tok/tokField/cat/opt/star/
   sepBy1/name/num/str + infix precedence categories. Offside-rule blocks,
   juxtaposition runs, and rest-of-line captures are not yet needed by any shipped
   grammar; the shipped surfaces are designed to be fully token-delimited instead.
5. **Whitespace policy**: round-trip law is `parse(print(t)) == t` (tree-level
   identity) plus print∘parse∘print fixpoints where texts matter; original input
   whitespace is not preserved.
6. **ΔL scope**: the free changes language covers module-level edits (add / replace /
   remove / rename-with-footprint) over named definitions. Finer-grained structural
   edits (inside a term) compose as replace with a new term; footprints are
   name-reference sets computed via the language's variable constructor.
7. **Rename footprint in the MVP transcript** is `[]` because the demo module's
   other definitions do not reference `id` by free variable; the mismatch test in
   `DeltaSuite` covers the non-empty case.
8. **Proof logic**: the independent checker validates derivations against
   declarative inference rules (matching with a shared metavariable environment).
   Side conditions like freshness/disequality are not expressible yet; STLC typing
   as shipped does not need them for the golden derivations. This is the designed
   proof-term slot (§4.7), not a full logic.
9. **PoA**: one authority key, dev-generated per run; multi-authority sets are
   supported by the kernel signature (authorities map) but not exercised.
10. **Ports**: Lean output is golden-checked only (no Lean toolchain assumed);
    Scala output actually runs under `scala-cli` when present (test `assume`s it).
11. **Meta-language staging** (§2b): the self-description surface covers fragment
    interfaces, sorts, constructors, binders, varctor. Grammar productions, rewrite
    rules, and judgments are still host-seeded values — the documented next step,
    not silently skipped.
12. **CLI CAS location**: `$CAIRN_HOME` or `./.cas` (gitignored).
