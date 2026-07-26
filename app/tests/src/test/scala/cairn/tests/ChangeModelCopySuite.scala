package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc

/** The exit-condition proof: a brand-new ΔL operation ("copy"), added as
  * PURE [[ChangeModel]] DATA, gets parseable syntax, executable apply
  * behavior, a footprint, and an inverse — with ZERO changes to
  * `Delta.deltaOf`/`Delta.applyTyped`/`ChangeAlgebra.footprint`/
  * `ChangeAlgebra.invert` themselves. Every assertion below runs through
  * the SAME interpreter code every other test in this suite uses; only the
  * `model` argument differs.
  */
class ChangeModelCopySuite extends munit.FunSuite:
  private val lang = Stlc.language

  private val copyOp = ChangeOpDef(
    name = "copy",
    params = List(
      ChangeParam("from", ChangeParamKind.NameK),
      ChangeParam("to", ChangeParamKind.NameK, precedingToken = Some("to"))),
    program = List(
      ChangeStep.Check(BoolExpr.IsDefined(ValueRef.Param(0)), RejectionSpec.NotDefinedR("copy", ValueRef.Param(0))),
      ChangeStep.Check(BoolExpr.Not(BoolExpr.IsDefined(ValueRef.Param(1))), RejectionSpec.AlreadyDefinedR("copy", ValueRef.Param(1))),
      ChangeStep.Bind("term", ChangeQuery.ReadTerm(ValueRef.Param(0))),
      ChangeStep.Mutate(Mutation.InsertDef(ValueRef.Param(1), ValueRef.Bound("term")))),
    footprint = FootprintExpr.Union(List(FootprintExpr.NameOf(0), FootprintExpr.NameOf(1))),
    // Analogous to add's inverse — copy created `to`, so undoing it removes `to`.
    inverse = InverseExpr("remove", List(InverseArg.Copy(1))),
    printSegs = List(
      PrintSeg.Lit("copy"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
      PrintSeg.Lit("to"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Lit(";")))

  private val model2 = ChangeModel(ChangeModel.default.operations :+ copyOp)

  private val dl2 = Delta.deltaOf(lang, model2).fold(e => fail(e.map(_.render).mkString), identity)
  private def parseChange(src: String): Cst = Parser.parse(dl2.grammar, src).fold(e => fail(e), identity)

  test("copy: parseable syntax falls out of pure ChangeModel data"):
    val parsed = parseChange("{ copy a to b ; }")
    parsed match
      case Cst.Node(_, List(Cst.Node("list", List(Cst.Node(t, List(Cst.Leaf("a"), Cst.Leaf("b"))))))) =>
        assertEquals(t, Delta.tag(lang, "copy"))
      case other => fail(other.toString)

  test("copy: executable apply — inserts `to` with `from`'s current term"):
    val base = Module(List("a" -> Stlc.tru))
    val ch = parseChange("{ copy a to b ; }")
    val (m1, _) = Delta.applyTyped(lang, base, ch, model2).fold(e => fail(e.render), identity)
    assertEquals(m1.get("b"), Some(Stlc.tru))
    assertEquals(m1.get("a"), Some(Stlc.tru)) // `from` is untouched by copy

  test("copy: rejects an undefined source with the right Rejection"):
    val base = Module(Nil)
    val ch = parseChange("{ copy a to b ; }")
    assertEquals(Delta.applyTyped(lang, base, ch, model2), Left(Delta.Rejection.NotDefined("copy", "a")))

  test("copy: rejects an already-defined target with the right Rejection"):
    val base = Module(List("a" -> Stlc.tru, "b" -> Stlc.fls))
    val ch = parseChange("{ copy a to b ; }")
    assertEquals(Delta.applyTyped(lang, base, ch, model2), Left(Delta.Rejection.AlreadyDefined("copy", "b")))

  test("copy: footprint is both names, from pure ChangeModel data"):
    val ch = parseChange("{ copy a to b ; }")
    assertEquals(ChangeAlgebra.footprint(lang, ch, model2), Set("a", "b"))

  test("copy: inverse is remove(to), and forward-then-inverse round-trips the module digest"):
    val base = Module(List("a" -> Stlc.tru))
    val ch = parseChange("{ copy a to b ; }")
    val inv = ChangeAlgebra.invert(lang, base, ch, model2).fold(e => fail(e), identity)
    inv match
      case Cst.Node(_, List(Cst.Node("list", List(Cst.Node(t, List(Cst.Leaf("b"))))))) =>
        assertEquals(t, Delta.tag(lang, "remove"))
      case other => fail(other.toString)
    val (m1, _) = Delta.apply(lang, base, ch, model2).fold(e => fail(e), identity)
    val (m2, _) = Delta.apply(lang, m1, inv, model2).fold(e => fail(e), identity)
    assertEquals(m2.digest, base.digest)

  test("copy: the DEFAULT model (no copy) still rejects `copy` syntax entirely — it's real data, not a hidden builtin"):
    val dlDefault = Delta.deltaOf(lang).fold(e => fail(e.map(_.render).mkString), identity)
    assert(Parser.parse(dlDefault.grammar, "{ copy a to b ; }").isLeft)
