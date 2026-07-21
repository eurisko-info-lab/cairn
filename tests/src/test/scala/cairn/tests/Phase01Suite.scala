package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.systemhandler.{MemCas, DiskCas, Branches}
import cairn.core.TreeEngine
import cairn.examples.stlc.Stlc

/** Phase 0 acceptance (S2–S6): store/load round-trip, golden digests. */
class Phase0Suite extends munit.FunSuite:
  test("CAS put/get round-trip, digest verified (S4)"):
    val cas = MemCas()
    val a = Stlc.base.artifact
    val key = cas.put(a)
    assertEquals(cas.get(key), Right(a))

  test("disk CAS detects corruption (S4)"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-cas")
    val cas = DiskCas(dir)
    val d = cas.putBytes("hello cairn".getBytes)
    // corrupt the object file
    val p = dir.resolve("objects").resolve(d.hex.take(2)).resolve(d.hex.drop(2))
    java.nio.file.Files.write(p, "tampered!!!".getBytes)
    assert(cas.getBytes(d).swap.exists(_.contains("corruption")))

  test("fragment artifacts round-trip through CAS (S7)"):
    val cas = MemCas()
    for f <- Stlc.fragments do
      val key = cas.put(f.artifact)
      val back = cas.get(key).map(a => FragmentCodec.fromCanon(a.body))
      assertEquals(back, Right(f))

  test("grammar spec is a canonical artifact (S8)"):
    val g = Stlc.language.grammar
    val c = GrammarSpec.toCanon(g)
    assertEquals(GrammarSpec.fromCanon(c), g)
    assertEquals(Digest.of(c), Digest.of(GrammarSpec.toCanon(GrammarSpec.fromCanon(c))))

/** Phase 1 acceptance (S9–S18). */
class Phase1Suite extends munit.FunSuite:
  val lang = Stlc.language
  val g = lang.grammar

  test("composition is order-independent (S12/S13)"):
    val l1 = Compose.compose("stlc", Stlc.boundFragments).toOption.get
    val l2 = Compose.compose("stlc", Stlc.boundFragments.reverse).toOption.get
    assertEquals(l1.digest, l2.digest)
    assertEquals(l1.grammar, l2.grammar)

  test("composition conflict is a structured error (S12)"):
    val evil = Stlc.base.copy(name = "evil",
      constructors = List(CtorDef("var", "Term", List("Name", "Name"))))
    Compose.compose("bad", List(Stlc.base, evil)) match
      case Left(errs) =>
        assert(errs.exists(e => e.path == "constructors/var"), errs.map(_.render).mkString("\n"))
        assert(errs.head.render.contains("evil"))
      case Right(_) => fail("conflict not detected")

  test("missing requires is a structured error (S12)"):
    Compose.compose("bad", List(Stlc.lambda)) match
      case Left(errs) => assert(errs.exists(_.path.startsWith("requires/")))
      case Right(_)   => fail("unsatisfied requires not detected")

  test("left-recursion checker accepts stlc grammar (S10)"):
    assertEquals(Parser.leftRecursionCheck(g), Right(()))

  test("left recursion rejected statically (S10)"):
    val bad = GrammarSpec("bad", TokenSpec(Nil, List("+"), None),
      List(CategorySpec("e", List(ConstructorSpec("plus", List(Elem.Cat("e"), Elem.Tok("+"), Elem.Cat("e")))))),
      Nil, Nil, "e")
    assert(Parser.leftRecursionCheck(bad).swap.exists(_.contains("left recursion")))

  test("parse golden terms (S10)"):
    assertEquals(Parser.parse(g, "true"), Right(Stlc.tru))
    assertEquals(Parser.parse(g, "fun x : Bool . x"), Right(Stlc.idBool))
    assertEquals(Parser.parse(g, "((fun x : Bool . x) true)"),
      Right(Stlc.app1(Stlc.idBool, Stlc.tru)))
    assertEquals(Parser.parse(g, "fun f : Bool -> Bool -> Bool . (f true)"),
      Right(Stlc.lam1("f", Stlc.arrow1(Stlc.tBool, Stlc.arrow1(Stlc.tBool, Stlc.tBool)),
        Stlc.app1(Stlc.v("f"), Stlc.tru))))

  test("arrow associativity + parens (S10/S11)"):
    val t = Stlc.arrow1(Stlc.arrow1(Stlc.tBool, Stlc.tBool), Stlc.tBool)
    val printed = Printer.print(g, Stlc.lam1("f", t, Stlc.v("f"))).toOption.get
    assert(printed.contains("(Bool -> Bool) -> Bool"), printed)
    assertEquals(Parser.parse(g, printed), Right(Stlc.lam1("f", t, Stlc.v("f"))))

  val goldenTerms = List(
    Stlc.tru, Stlc.fls, Stlc.idBool, Stlc.churchTrue, Stlc.churchFalse,
    Stlc.app1(Stlc.idBool, Stlc.tru),
    Stlc.node3if,
    Stlc.app1(Stlc.app1(Stlc.churchTrue, Stlc.tru), Stlc.fls),
    Stlc.lam1("f", Stlc.arrow1(Stlc.tBool, Stlc.tBool), Stlc.app1(Stlc.v("f"), Stlc.app1(Stlc.v("f"), Stlc.tru))))

  test("round-trip law: parse(print(t)) == t on golden suite (S11, §4.4)"):
    for t <- goldenTerms do
      RoundTrip.check(g, t) match
        case Right(())  => ()
        case Left(err)  => fail(err)

  test("evaluation: identity and if (S15/S16)"):
    assertEquals(TreeEngine.normalize(lang, Stlc.app1(Stlc.idBool, Stlc.tru)), Right(Stlc.tru))
    assertEquals(TreeEngine.normalize(lang, Stlc.node3if), Right(Stlc.fls))

  test("Church booleans evaluate (S16 acceptance)"):
    // churchTrue a b --> a ; churchFalse a b --> b
    assertEquals(TreeEngine.normalize(lang,
      Stlc.app1(Stlc.app1(Stlc.churchTrue, Stlc.tru), Stlc.fls)), Right(Stlc.tru))
    assertEquals(TreeEngine.normalize(lang,
      Stlc.app1(Stlc.app1(Stlc.churchFalse, Stlc.tru), Stlc.fls)), Right(Stlc.fls))

  test("engine is generic: no rule => stuck term unchanged (S15)"):
    val stuck = Stlc.v("free")
    assertEquals(TreeEngine.normalize(lang, stuck), Right(stuck))

class DeltaSuite extends munit.FunSuite:
  val lang = Stlc.language

  test("ΔL is a real language and parses change syntax (S17)"):
    val dl = Delta.deltaOf(lang).toOption.get
    assertEquals(dl.name, "Δstlc")
    val src = "{ add id = fun x : Bool . x ; }"
    val parsed = Parser.parse(dl.grammar, src)
    assert(parsed.isRight, parsed.toString)

  test("ΔL edit produces new digest; validated change-set recorded (S17)"):
    val dl = Delta.deltaOf(lang).toOption.get
    val m0 = Module(Nil)
    val change = Parser.parse(dl.grammar, "{ add id = fun x : Bool . x ; add both = (id true) ; }").toOption.get
    val Right((m1, vcs)) = Delta.apply(lang, m0, change): @unchecked
    assert(m1.digest != m0.digest)
    assertEquals(vcs.base, m0.digest)
    assertEquals(vcs.result, m1.digest)
    assertEquals(m1.get("id"), Some(Stlc.idBool))

  test("invalid ΔL edit rejected (S17)"):
    val dl = Delta.deltaOf(lang).toOption.get
    val change = Parser.parse(dl.grammar, "{ replace missing = true ; }").toOption.get
    assert(Delta.apply(lang, Module(Nil), change).swap.exists(_.contains("not defined")))

  test("rename requires exact footprint (S17)"):
    val dl = Delta.deltaOf(lang).toOption.get
    val m = Module(List(
      "id" -> Stlc.idBool,
      "use" -> Stlc.app1(Stlc.v("id"), Stlc.tru)))
    val wrong = Parser.parse(dl.grammar, "{ rename id to ident footprint [] ; }").toOption.get
    assert(Delta.apply(lang, m, wrong).swap.exists(_.contains("footprint mismatch")))
    val right = Parser.parse(dl.grammar, "{ rename id to ident footprint [use] ; }").toOption.get
    val Right((m2, _)) = Delta.apply(lang, m, right): @unchecked
    assertEquals(m2.get("ident"), Some(Stlc.idBool))
    assertEquals(m2.get("use"), Some(Stlc.app1(Stlc.v("ident"), Stlc.tru)))

  test("Δ(ΔL) exists — forced recursive closure (S17, §2b)"):
    val dl = Delta.deltaOf(lang).toOption.get
    val ddl = Delta.deltaOf(dl).toOption.get
    assertEquals(ddl.name, "ΔΔstlc")
    assert(ddl.grammar.top == "ΔΔstlc.changeset")
    // and Δ(Δ(ΔL)) for good measure
    assert(Delta.deltaOf(ddl).isRight)

  test("ΔL round-trips its own surface (S17 + S11 law)"):
    val dl = Delta.deltaOf(lang).toOption.get
    val src = "{ add id = fun x : Bool . x ; remove old ; rename a to b footprint [x, y] ; }"
    val t = Parser.parse(dl.grammar, src).toOption.get
    RoundTrip.check(dl.grammar, t).fold(e => fail(e), identity)

class BranchSuite extends munit.FunSuite:
  test("branch manifests: append-only history surviving restart (S18)"):
    val dir = java.nio.file.Files.createTempDirectory("cairn-branches")
    val cas = DiskCas(dir)
    val branches = Branches(cas, dir.resolve("refs"))
    val k1 = cas.put(Stlc.base.artifact)
    val k2 = cas.put(Stlc.types.artifact)
    branches.advance("main", k1)
    branches.advance("main", k2)
    // fresh instances = process restart
    val branches2 = Branches(DiskCas(dir), dir.resolve("refs"))
    val m = branches2.load("main")
    assertEquals(m.head, Some(k2))
    assertEquals(m.history, List(k1))
    assertEquals(branches2.list(), List("main"))
