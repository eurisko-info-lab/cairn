package cairn.tests

import cairn.kernel.*
import cairn.workbench.*
import cairn.core.*
import cairn.examples.stlc.Stlc

/** Wave C acceptance (M13–M18). */
class WaveCSuite extends munit.FunSuite:
  val lang = Stlc.language
  val dl = Delta.deltaOf(lang).toOption.get

  def parseChange(src: String): Cst = Parser.parse(dl.grammar, src).fold(e => fail(e), identity)

  // ---- M13: renaming morphisms ----

  test("M13: fragments compose along a declared rename span"):
    // a fragment defining the same constructor under a different name
    val boolG = Stlc.booleansSurface.grammar
    val altBooleans = Stlc.booleans.copy(name = "alt-booleans",
      constructors = Stlc.booleans.constructors.map(c =>
        if c.name == "true" then c.copy(name = "verum") else c),
      grammar = boolG.copy(
        categories = List(CategorySpec("term", boolG.categories.head.ctors.map(cs =>
          if cs.tag == "true" then cs.copy(tag = "verum") else cs))),
        printRules = boolG.printRules.map(pr =>
          if pr.tag == "true" then pr.copy(tag = "verum") else pr)),
      rewriteRules = Stlc.booleans.rewriteRules.map(r =>
        RewriteRule(r.name, Rename.cst(r.pattern, Map("true" -> "verum")), Rename.cst(r.template, Map("true" -> "verum")))))
    // without a rename: conflict (same grammar alternative tag space, different defs)
    val direct = Compose.compose("clash", List(
      Stlc.base.copy(grammar = Stlc.baseSurface.grammar),
      Stlc.types.copy(grammar = Stlc.typesSurface.grammar),
      Stlc.lambda.copy(grammar = Stlc.lambdaSurface.grammar),
      altBooleans, Stlc.booleans.copy(grammar = boolG)))
    assert(direct.isLeft)
    // with the morphism verum -> true, the two fragments unify (diamond)
    val merged = ComposeImports.compose("stlc", List(
      Import(Stlc.base.copy(grammar = Stlc.baseSurface.grammar)),
      Import(Stlc.types.copy(grammar = Stlc.typesSurface.grammar)),
      Import(Stlc.lambda.copy(grammar = Stlc.lambdaSurface.grammar)),
      Import(Stlc.typing),
      Import(altBooleans, Map("verum" -> "true")),
      Import(Stlc.booleans.copy(grammar = boolG))))
    merged match
      case Left(errs) =>
        // the alt fragment renamed is IDENTICAL to booleans except fragment name;
        // identical definitions unify, so only the fragment-name digest may differ
        fail(errs.map(_.render).mkString("\n"))
      case Right(l) =>
        assert(l.constructors.contains("true"))
        assert(!l.constructors.contains("verum"))

  test("M13: digest independent of rename spelling"):
    val a = ComposeImports.compose("stlc", Stlc.boundFragments.map(Import(_))).toOption.get
    assertEquals(a.digest, Stlc.language.digest)

  // ---- M14: parameterized fragments ----

  test("M14: one List functor instantiated at two sorts in the same language"):
    val listFunctor = FragmentFunctor("list", List("E"), Fragment(
      name = "list",
      provides = List("list"),
      requires = Nil,
      sorts = List(SortDef("ListSort", SortMode.Tree)),
      constructors = List(
        CtorDef("nil", "ListSort", Nil),
        CtorDef("cons", "ListSort", List("E", "ListSort")))))
    val atTerm = listFunctor.instantiate(Map("E" -> "Term")).toOption.get
    val atType = listFunctor.instantiate(Map("E" -> "Type")).toOption.get
    assertEquals(atTerm.name, "list[Term]")
    assert(atTerm.constructors.exists(c => c.name == "cons[Term]" && c.argSorts == List("Term", "ListSort[Term]")))
    // both instances coexist in one composition
    Compose.compose("stlc-lists", Stlc.fragments ++ List(atTerm, atType)) match
      case Right(l) =>
        assert(l.constructors.contains("cons[Term]"))
        assert(l.constructors.contains("cons[Type]"))
      case Left(errs) => fail(errs.map(_.render).mkString("\n"))

  test("M14: instantiation is hash-stable"):
    val f = FragmentFunctor("box", List("E"), Fragment("box", List("box"), Nil,
      constructors = List(CtorDef("mk", "Box", List("E")))))
    val a = f.instantiate(Map("E" -> "Term")).toOption.get
    val b = f.instantiate(Map("E" -> "Term")).toOption.get
    assertEquals(a.digest, b.digest)
    assert(f.instantiate(Map.empty).isLeft)

  // ---- M15: structural ΔL ----

  test("M15: path edit replaces a subtree without retyping the definition"):
    val m0 = Module(List("id" -> Stlc.idBool))
    // lam children: [name, type, body] — edit the body (index 2) to `true`
    val edit = parseChange("{ edit id at [2] = true ; }")
    val Right((m1, vcs)) = Delta.apply(lang, m0, edit): @unchecked
    assertEquals(m1.get("id"), Some(Stlc.lam1("x", Stlc.tBool, Stlc.tru)))
    assert(vcs.result != vcs.base)

  test("M15: bad path is a structured error"):
    val m0 = Module(List("id" -> Stlc.idBool))
    val edit = parseChange("{ edit id at [7] = true ; }")
    assert(Delta.apply(lang, m0, edit).swap.exists(_.contains("out of range")))
    val deep = parseChange("{ edit id at [0, 0] = true ; }")
    assert(Delta.apply(lang, m0, deep).swap.exists(_.contains("leaf")))

  test("M15: Δ(ΔL) includes path edits over change terms (closure holds)"):
    val ddl = Delta.deltaOf(dl).toOption.get
    assert(ddl.constructors.contains(s"edit:${dl.name}"))
    assert(Delta.deltaOf(ddl).isRight)

  // ---- M16: change algebra ----

  val m0 = Module(List("a" -> Stlc.tru, "b" -> Stlc.fls))

  test("M16: apply(compose(c1, c2)) == apply(c2) ∘ apply(c1)"):
    val c1 = parseChange("{ add id = fun x : Bool . x ; }")
    val c2 = parseChange("{ replace a = false ; }")
    val seq = Delta.apply(lang, m0, c1).flatMap((m1, _) => Delta.apply(lang, m1, c2)).map(_._1.digest)
    val composed = Delta.apply(lang, m0, ChangeAlgebra.compose(lang, c1, c2)).map(_._1.digest)
    assertEquals(composed, seq)

  test("M16: apply(invert(c)) ∘ apply(c) == id"):
    val c = parseChange("{ replace a = false ; add id = fun x : Bool . x ; remove b ; edit id at [2] = true ; }")
    val Right((m1, _)) = Delta.apply(lang, m0, c): @unchecked
    val inv = ChangeAlgebra.invert(lang, m0, c).fold(e => fail(e), identity)
    val Right((m2, _)) = Delta.apply(lang, m1, inv): @unchecked
    assertEquals(m2.digest, m0.sorted.digest)

  test("M16: disjoint footprints commute to the same digest"):
    val c1 = parseChange("{ replace a = false ; }")
    val c2 = parseChange("{ replace b = true ; }")
    assert(ChangeAlgebra.commutes(lang, c1, c2))
    val ab = Delta.apply(lang, m0, ChangeAlgebra.compose(lang, c1, c2)).map(_._1.digest)
    val ba = Delta.apply(lang, m0, ChangeAlgebra.compose(lang, c2, c1)).map(_._1.digest)
    assertEquals(ab, ba)

  test("M16: overlapping footprints do not commute"):
    val c1 = parseChange("{ replace a = false ; }")
    val c2 = parseChange("{ remove a ; }")
    assert(!ChangeAlgebra.commutes(lang, c1, c2))

  // ---- M17: semantic merge ----

  test("M17: disjoint branches merge clean"):
    val branchA = parseChange("{ replace a = false ; add fromA = true ; }")
    val branchB = parseChange("{ replace b = true ; add fromB = false ; }")
    Merge.threeWay(lang, m0, branchA, branchB) match
      case Right((merged, vcs)) =>
        assertEquals(merged.get("a"), Some(Stlc.fls))
        assertEquals(merged.get("b"), Some(Stlc.tru))
        assert(merged.get("fromA").isDefined && merged.get("fromB").isDefined)
        assertEquals(vcs.base, m0.digest)
      case Left(c) => fail(c.render)

  test("M17: same-definition edits produce a conflict artifact naming both change-sets"):
    val branchA = parseChange("{ replace a = false ; }")
    val branchB = parseChange("{ edit a at [] = fun x : Bool . x ; }")
    Merge.threeWay(lang, m0, branchA, branchB) match
      case Left(conflict) =>
        assertEquals(conflict.overlap, Set("a"))
        assert(conflict.changeA != conflict.changeB)
        assertEquals(conflict.artifact.kind, ArtifactKind.ChangeSet)
      case Right(_) => fail("conflict not detected")

  test("M17: merge order is canonical (digest-deterministic)"):
    val branchA = parseChange("{ add x1 = true ; }")
    val branchB = parseChange("{ add x2 = false ; }")
    val ab = Merge.threeWay(lang, m0, branchA, branchB).toOption.get._1.digest
    val ba = Merge.threeWay(lang, m0, branchB, branchA).toOption.get._1.digest
    assertEquals(ab, ba)

  // ---- M18: migrations ----

  test("M18: module transports across a ctor rename + arity change"):
    // v2 language: `if` renamed to `cond` with a 4th child (an else-label leaf)
    val boolG = Stlc.booleansSurface.grammar
    val boolsV2 = Stlc.booleans.copy(name = "booleans",
      constructors = Stlc.booleans.constructors.map(c =>
        if c.name == "if" then CtorDef("cond", "Term", List("Term", "Term", "Term", "Name")) else c),
      grammar = boolG.copy(
        categories = List(CategorySpec("term", boolG.categories.head.ctors.map(cs =>
          if cs.tag == "if" then ConstructorSpec("cond", cs.elems :+ Elem.Opt(Elem.NameLeaf)) else cs))),
        printRules = boolG.printRules.map(pr =>
          if pr.tag == "if" then PrintRule("cond", pr.segs :+ PrintSeg.Field(3)) else pr)),
      rewriteRules = Nil)
    val v2 = Compose.compose("stlc2", List(
      Stlc.base.copy(grammar = Stlc.baseSurface.grammar),
      Stlc.types.copy(grammar = Stlc.typesSurface.grammar),
      Stlc.lambda.copy(grammar = Stlc.lambdaSurface.grammar),
      boolsV2)).fold(
      e => fail(e.map(_.render).mkString("\n")), identity)
    val mig = LangMigration(lang.digest, v2.digest,
      ctorRenames = Map("if" -> "cond"),
      arityChanges = Map("cond" -> (4, Cst.Leaf("default"))))
    val m = Module(List("branchy" -> Stlc.node3if))
    Migrate.module(mig, v2, m) match
      case Right(m2) =>
        m2.get("branchy") match
          case Some(Cst.Node("cond", List(_, _, _, Cst.Leaf("default")))) => ()
          case other => fail(s"unexpected transported term: $other")
      case Left(e) => fail(e)
    // migration is a publishable, canonical artifact
    assertEquals(mig.artifact.kind, ArtifactKind.Migration)

  test("M18: stale terms citing unknown ctors fail with reasons"):
    val v2 = Compose.compose("stlc2", List(Stlc.base, Stlc.types, Stlc.lambda)).fold(
      e => fail(e.map(_.render).mkString("\n")), identity) // no booleans in v2!
    val mig = LangMigration(lang.digest, v2.digest, Map.empty, Map.empty)
    val m = Module(List("branchy" -> Stlc.node3if))
    Migrate.module(mig, v2, m) match
      case Left(err) => assert(err.contains("is not a constructor of 'stlc2'"), err)
      case Right(_)  => fail("expected failure")

  test("M18: change-sets transport across migrations"):
    val v2 = Compose.compose("stlc2", Stlc.fragments).toOption.get
    val mig = LangMigration(lang.digest, v2.digest, Map.empty, Map.empty)
    val change = parseChange("{ add id = fun x : Bool . x ; }")
    Migrate.changeset(mig, lang, v2, change) match
      case Right(c2) =>
        c2 match
          case Cst.Node(t, _) => assertEquals(t, "changeset:stlc2")
          case _ => fail("not a node")
        // and the transported change applies in the target language
        assert(Delta.apply(v2, Module(Nil), c2).isRight)
      case Left(e) => fail(e)
