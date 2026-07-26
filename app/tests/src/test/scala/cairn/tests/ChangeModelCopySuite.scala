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

  // -- PR6: ChangeModel identity is now bound into ValidatedChangeSet.Claim --

  test("ChangeModel: canon round-trip for a custom model, at the canon level"):
    assertEquals(ChangeModel.fromCanon(model2.canon).canon, model2.canon)

  test("ChangeModel: byte-level artifact round-trip — a node with only the encoded bytes reconstructs the model"):
    val bytes = Canon.encode(model2.canon)
    val decoded = Canon.decode(bytes).fold(e => fail(e), identity)
    val model2decoded = ChangeModel.fromCanon(decoded)
    assertEquals(model2decoded.canon, model2.canon)
    assertEquals(model2decoded.digest, model2.digest)

  test("ChangeModel: create under model2, replay with a freshly byte-decoded model — 'resolve on another node'"):
    val bytes = Canon.encode(model2.canon)
    val model2decoded = ChangeModel.fromCanon(Canon.decode(bytes).fold(e => fail(e), identity))
    val base = Module(List("a" -> Stlc.tru))
    val ch = parseChange("{ copy a to b ; }")
    val (_, vcs) = Delta.applyTyped(lang, base, ch, model2).fold(e => fail(e.render), identity)
    val claim = vcs.claim
    assertEquals(claim.changeModel, model2.digest)
    val checked = Delta.ValidatedChangeSet.check(lang, model2decoded, base, claim)
    assert(checked.isRight, s"replay under the freshly decoded model should succeed: $checked")

  test("ChangeModel: supplying the WRONG model fails BEFORE apply, with a model-mismatch error"):
    val base = Module(List("a" -> Stlc.tru))
    val ch = parseChange("{ copy a to b ; }")
    val (_, vcs) = Delta.applyTyped(lang, base, ch, model2).fold(e => fail(e.render), identity)
    val claim = vcs.claim
    val checked = Delta.ValidatedChangeSet.check(lang, ChangeModel.default, base, claim)
    checked match
      case Left(msg) => assert(msg.contains("model mismatch"), s"expected a model-mismatch error, got: $msg")
      case Right(_)  => fail("expected the default model (no `copy` op) to reject this claim")

  test("ChangeModel: identical transitions under semantically different models get different VCS identities, even with the same derived ΔL language digest"):
    // Same name/params as `copyOp` (so the derived ΔL grammar/CtorDef — and
    // therefore Delta.deltaOf's language digest — is byte-identical), but a
    // different footprint: this is a pure semantics change, invisible to the
    // grammar, that only ChangeModel.digest (not the derived language's
    // digest) can distinguish.
    val copyOpVariant = copyOp.copy(footprint = FootprintExpr.NameOf(1))
    val model3 = ChangeModel(ChangeModel.default.operations :+ copyOpVariant)
    val dl3 = Delta.deltaOf(lang, model3).fold(e => fail(e.map(_.render).mkString), identity)

    assertEquals(dl3.digest, dl2.digest, "same name/params must derive the same ΔL language, regardless of program/footprint/inverse")
    assert(model3.digest != model2.digest, "different footprint must change the ChangeModel's own digest")

    val base = Module(List("a" -> Stlc.tru))
    val ch = parseChange("{ copy a to b ; }")
    val (_, vcs2) = Delta.applyTyped(lang, base, ch, model2).fold(e => fail(e.render), identity)
    val (_, vcs3) = Delta.applyTyped(lang, base, ch, model3).fold(e => fail(e.render), identity)
    assertEquals(vcs2.result, vcs3.result, "both models apply `copy` identically here — same resulting module")
    assert(vcs2.claim.changeModel != vcs3.claim.changeModel, "but their claims must carry different ChangeModel identities")

  // -- PR10: explicit-model threading through the CREATE-side algebra --

  test("checkRecognized: rejects a `copy` change under the default model, before footprint runs"):
    val ch = parseChange("{ copy a to b ; }")
    ChangeAlgebra.checkRecognized(lang, ch, ChangeModel.default) match
      case Left(msg) => assert(msg.contains("unrecognized operation"), msg)
      case Right(()) => fail("default model has no `copy` op, this must be rejected")

  test("checkRecognized: accepts a `copy` change under model2"):
    val ch = parseChange("{ copy a to b ; }")
    assertEquals(ChangeAlgebra.checkRecognized(lang, ch, model2), Right(()))

  test("Merge.threeWay: two disjoint `copy` changes merge successfully under model2, with no default-model fallback"):
    val base = Module(List("a" -> Stlc.tru, "c" -> Stlc.tru))
    val changeA = parseChange("{ copy a to b ; }")
    val changeB = parseChange("{ copy c to d ; }")
    Merge.threeWay(lang, base, changeA, changeB, model = model2) match
      case Right((merged, vcs)) =>
        assertEquals(merged.get("b"), Some(Stlc.tru))
        assertEquals(merged.get("d"), Some(Stlc.tru))
        assertEquals(vcs.claim.changeModel, model2.digest)
      case Left(c) => fail(s"expected the disjoint copy changes to merge under model2: ${c.render}")

  test("SemanticRepository.commit/tipAfter: a custom-model change works when the model is supplied, and fails before apply when it isn't"):
    val base = Module(List("a" -> Stlc.tru))
    val ch = parseChange("{ copy a to b ; }")
    SemanticRepository.commit(lang, base, ch, model2) match
      case Right((m, vcs)) =>
        assertEquals(m.get("b"), Some(Stlc.tru))
        assertEquals(vcs.claim.changeModel, model2.digest)
      case Left(e) => fail(s"expected commit under model2 to succeed: $e")
    SemanticRepository.commit(lang, base, ch) match
      case Left(e) => assert(e.contains("unrecognized operation"), e)
      case Right(_) => fail("commit under the default model must reject `copy` before apply")
    SemanticRepository.tipAfter(lang, base, ch, model2) match
      case Right(vt) => assertEquals(vt.tip.get("b"), Some(Stlc.tru))
      case Left(e) => fail(s"expected tipAfter under model2 to succeed: $e")
    SemanticRepository.tipAfter(lang, base, ch) match
      case Left(e) => assert(e.contains("unrecognized operation"), e)
      case Right(_) => fail("tipAfter under the default model must reject `copy` before apply")

  test("Merge.threeWay: the same `copy` changes fail under the default model with an unrecognized-operation witness, not a false footprint pass"):
    val base = Module(List("a" -> Stlc.tru, "c" -> Stlc.tru))
    val changeA = parseChange("{ copy a to b ; }")
    val changeB = parseChange("{ copy c to d ; }")
    Merge.threeWay(lang, base, changeA, changeB) match
      case Left(conflict) =>
        assert(conflict.overlap.isEmpty, "must fail via the recognition guard, not a footprint overlap")
        conflict.witness match
          case Some(Merge.ConflictWitness.ApplyFailed(_, rej)) => assert(rej.render.contains("unrecognized operation"), rej.render)
          case other => fail(s"expected an ApplyFailed(unrecognized operation) witness, got: $other")
      case Right(_) => fail("the default model has no `copy` op — this must not silently succeed")

  test("copy: works under applyPreservingFormat with ZERO format-preserving code changes"):
    val mg = ModuleSurface.grammar(lang)
    val src = "-- a's own comment\na = true ;\n-- unrelated comment, untouched\nother = false ;\n"
    val ch = parseChange("{ copy a to b ; }")
    val result = Delta.applyPreservingFormat(lang, mg, src, ch, model2).fold(e => fail(e), identity)
    // The custom op's own comment, and every other def's comment, survive —
    // the format-preserving layer never had to learn what "copy" means; it
    // only understood the InsertedDef mutation copy's program resolves to.
    assert(result.contains("-- a's own comment\na = true ;"), result)
    assert(result.contains("-- unrelated comment, untouched\nother = false ;"), result)
    assert(result.contains("b = true ;"), result)
    assert(Parser.parse(mg, result).isRight, result)
    // Parsing the result back yields the SAME module digest applyTyped produced directly.
    val base = ModuleSurface.toModule(Parser.parse(mg, src).fold(e => fail(e), identity)).fold(e => fail(e), identity)
    val (viaApply, _) = Delta.applyTyped(lang, base, ch, model2).fold(e => fail(e.render), identity)
    val viaPreserved = ModuleSurface.toModule(Parser.parse(mg, result).fold(e => fail(e), identity)).fold(e => fail(e), identity)
    assertEquals(viaPreserved.sorted.digest, viaApply.sorted.digest)
