package cairn.tests

import cairn.kernel.*
import cairn.compute.*
import cairn.proof.*
import cairn.core.*
import cairn.examples.stlc.Stlc
import cairn.examples.claims.Claims

/** Wave D acceptance (M19–M24). */
class WaveDSuite extends munit.FunSuite:
  val lang = Stlc.language
  val cfg = CheckerCfg(lang.judgments.values.toList,
    binderSpec = lang.binderSpec, varCtor = "var")
  def n(tag: String, cs: Cst*): Cst = Cst.node(tag, cs*)
  val ctxNil = n("ctxNil")
  def ctx(entries: (String, Cst)*): Cst =
    entries.foldRight(ctxNil) { case ((x, t), acc) => n("ctxCons", Cst.Leaf(x), t, acc) }

  // ---- M19: side conditions ----

  test("M19: shadowing exploit rejected by $neq side condition"):
    // context: x:Bool (outer), x:Bool->Bool (inner shadows outer).
    // A derivation that looks up the OUTER x via l-there must fail: x == x.
    val shadowed = ctx("x" -> n("arrow", n("tyBool"), n("tyBool")), "x" -> n("tyBool"))
    val exploit = Derivation("l-there",
      n("lookup", shadowed, Cst.Leaf("x"), n("tyBool")),
      List(Derivation("l-here",
        n("lookup", ctx("x" -> n("tyBool")), Cst.Leaf("x"), n("tyBool")), Nil)))
    Checker.check(cfg, exploit) match
      case Left(e)  => assert(e.render.contains("$neq failed"), e.render)
      case Right(_) => fail("shadowing exploit was accepted")

  test("M19: legitimate l-there still checks (distinct names)"):
    val c = ctx("y" -> n("arrow", n("tyBool"), n("tyBool")), "x" -> n("tyBool"))
    val ok = Derivation("l-there",
      n("lookup", c, Cst.Leaf("x"), n("tyBool")),
      List(Derivation("l-here", n("lookup", ctx("x" -> n("tyBool")), Cst.Leaf("x"), n("tyBool")), Nil)))
    assertEquals(Checker.check(cfg, ok), Right(()))

  test("M19: golden derivation still checks after the upgrade"):
    assertEquals(Checker.check(cfg, Claims.idAppliedDerivation), Right(()))

  // ---- M20: computational context lookup ----

  test("M20: t-var needs no lookup sub-derivation; shadowing is respected"):
    val c = ctx("x" -> n("tyBool"))
    val d = Derivation("t-var", n("hasType", c, Stlc.v("x"), n("tyBool")), Nil)
    assertEquals(Checker.check(cfg, d), Right(()))
    // shadowed: inner binding wins — outer type claim must fail
    val c2 = ctx("x" -> n("arrow", n("tyBool"), n("tyBool")), "x" -> n("tyBool"))
    val dInner = Derivation("t-var", n("hasType", c2, Stlc.v("x"), n("arrow", n("tyBool"), n("tyBool"))), Nil)
    assertEquals(Checker.check(cfg, dInner), Right(()))
    val dOuter = Derivation("t-var", n("hasType", c2, Stlc.v("x"), n("tyBool")), Nil)
    assert(Checker.check(cfg, dOuter).isLeft)

  test("M20: missing variable is a structured error"):
    val d = Derivation("t-var", n("hasType", ctxNil, Stlc.v("ghost"), n("tyBool")), Nil)
    assert(Checker.check(cfg, d).swap.exists(_.render.contains("not in context")))

  // ---- M21: derivation search / type inference ----

  test("M21: infer produces checkable derivations for all golden terms"):
    val goldens = List(
      Stlc.tru -> n("tyBool"),
      Stlc.idBool -> n("arrow", n("tyBool"), n("tyBool")),
      Stlc.app1(Stlc.idBool, Stlc.tru) -> n("tyBool"),
      Stlc.churchTrue -> n("arrow", n("tyBool"), n("arrow", n("tyBool"), n("tyBool"))),
      Stlc.node3if -> n("tyBool"))
    for (term, expectedTy) <- goldens do
      val goal = n("hasType", ctxNil, term, Cst.Leaf("$T"))
      Search.infer(cfg, goal) match
        case Right(d) =>
          assertEquals(d.conclusion, n("hasType", ctxNil, term, expectedTy))
          assertEquals(Checker.check(cfg, d), Right(()), s"inferred derivation fails check for ${term.render}")
        case Left(e) => fail(s"${term.render}: $e")

  test("M21: ill-typed term reports the blocked goal"):
    val bad = Stlc.app1(Stlc.tru, Stlc.fls) // true false
    val goal = n("hasType", ctxNil, bad, Cst.Leaf("$T"))
    Search.infer(cfg, goal) match
      case Left(e)  => assert(e.contains("no derivation found"), e)
      case Right(d) => fail(s"unexpected inference: ${d.conclusion.render}")

  // ---- M22: tactics ----

  test("M22: tactic script replays to a proof term the checker validates"):
    val goal = n("hasType", ctxNil, Stlc.idBool, n("arrow", n("tyBool"), n("tyBool")))
    val script = TacticScript("id-typing", List(Tactic.ApplyRule("t-abs"), Tactic.ApplyRule("t-var")))
    Tactics.replay(cfg, script, goal) match
      case Right(d) =>
        assertEquals(d.rule, "t-abs")
        assertEquals(Checker.check(cfg, d), Right(()))
      case Left(e) => fail(e)
    // scripts are artifacts
    assertEquals(script.artifact.kind, ArtifactKind.TacticScript)

  test("M22: auto tactic delegates to search"):
    val goal = n("hasType", ctxNil, Stlc.app1(Stlc.idBool, Stlc.tru), n("tyBool"))
    val script = TacticScript("auto", List(Tactic.Auto(32)))
    Tactics.replay(cfg, script, goal) match
      case Right(d) => assertEquals(Checker.check(cfg, d), Right(()))
      case Left(e)  => fail(e)

  test("M22: wrong tactic is an error, not a bogus proof"):
    val goal = n("hasType", ctxNil, Stlc.idBool, n("arrow", n("tyBool"), n("tyBool")))
    val script = TacticScript("bad", List(Tactic.ApplyRule("t-true")))
    assert(Tactics.replay(cfg, script, goal).isLeft)

  // ---- M23: quantified property claims ----

  /** Well-typed STLC term generator (domain pack code, not kernel). */
  def genWellTyped(rnd: scala.util.Random, depth: Int): Cst =
    def genTerm(d: Int, env: List[String]): Cst =
      val choices = rnd.nextInt(if d <= 0 then 2 else 5)
      choices match
        case 0 => Stlc.tru
        case 1 => Stlc.fls
        case 2 if env.nonEmpty => Stlc.v(env(rnd.nextInt(env.length)))
        case 2 => Stlc.tru
        case 3 =>
          val x = s"v${env.length}"
          Stlc.app1(Stlc.lam1(x, Stlc.tBool, genTerm(d - 1, x :: env)), genTerm(d - 1, env))
        case _ => n("if", genTerm(d - 1, env), genTerm(d - 1, env), genTerm(d - 1, env))
    genTerm(depth, Nil)

  def inferType(t: Cst): Either[String, Cst] =
    Search.infer(cfg, n("hasType", ctxNil, t, Cst.Leaf("$T"))).map(_.conclusion match
      case Cst.Node("hasType", List(_, _, ty)) => ty
      case other => other)

  test("M23: subject reduction holds over generated well-typed terms"):
    val claim = Claim("subject-reduction", n("claimSubjectReduction"), lang.digest)
    val gen = GenSpec("well-typed-bool-terms", seed = 20260720L, count = 200, maxDepth = 4)
    val result = PropertyCert.forAll(claim, gen,
      generate = genWellTyped,
      admissible = t => inferType(t).isRight,
      property = t =>
        for
          ty1 <- inferType(t)
          v <- TreeEngine.normalize(lang, t)
          ty2 <- inferType(v)
        yield ty1 == ty2)
    result match
      case Right(cert) =>
        assertEquals(cert.method, "property")
        assertEquals(cert.evidence, gen.artifact.digest)
      case Left(f) => fail(f.render)

  test("M23: failing property shrinks and reproduces by seed"):
    val claim = Claim("bogus-all-true", n("claimBogus"), lang.digest)
    val gen = GenSpec("g", seed = 7L, count = 100, maxDepth = 3)
    def run() = PropertyCert.forAll(claim, gen,
      generate = genWellTyped,
      admissible = _ => true,
      property = t => Right(TreeEngine.normalize(lang, t) == Right(Stlc.tru)))
    (run(), run()) match
      case (Left(f1), Left(f2)) =>
        assertEquals(f1.original, f2.original) // seeded: same failure
        // shrunk term is no larger than the original
        assert(f1.shrunk.render.length <= f1.original.render.length)
      case other => fail(s"expected reproducible failure, got $other")

  // ---- M24: certified evaluation traces ----

  test("M24: normalizeTraced yields a trace the checker validates"):
    val term = Stlc.app1(Stlc.app1(Stlc.churchTrue, Stlc.tru), Stlc.fls)
    val Right((result, trace)) = TreeEngine.normalizeTraced(lang, term): @unchecked
    assertEquals(result, Stlc.tru)
    assertEquals(TreeEngine.normalize(lang, term), Right(result)) // agrees with plain path
    assertEquals(TraceChecker.check(lang, trace), Right(()))
    assertEquals(trace.artifact.kind, ArtifactKind.Trace)
    assert(trace.steps.nonEmpty)

  test("M24: tampered traces fail"):
    val term = Stlc.app1(Stlc.idBool, Stlc.tru)
    val Right((_, trace)) = TreeEngine.normalizeTraced(lang, term): @unchecked
    // tamper 1: claim a different rule fired
    val t1 = trace.copy(steps = trace.steps.map(_.copy(rule = "if-true")))
    assert(TraceChecker.check(lang, t1).isLeft)
    // tamper 2: alter the recorded result
    val t2 = trace.copy(result = Stlc.fls)
    assert(TraceChecker.check(lang, t2).isLeft)
    // tamper 3: alter an intermediate state
    val t3 = trace.copy(steps = trace.steps.map(_.copy(after = Stlc.fls)))
    assert(TraceChecker.check(lang, t3).isLeft)
