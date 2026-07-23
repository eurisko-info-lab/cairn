package cairn.tests

import cairn.core.*
import cairn.core.NetLanguageChecker.RuleError
import cairn.user.icnet.IcNet
import cairn.examples.stlc.Stlc

/** Unit + property tests for [[NetLanguageChecker]] — the gate
  * [[CompiledNetEngine]]'s constructor needs before indexing a rule table:
  * unknown kinds, out-of-range `RulePort`s, non-linear rewrites, and
  * (the case `CompiledNetEngine`'s own `getOrElseUpdate` would otherwise
  * silently resolve by declaration order) two different rules claiming the
  * same unordered active-pair kind.
  */
class NetLanguageCheckerSuite extends munit.FunSuite:
  private val ic = IcNet.language

  test("the shipped ic-net language validates clean"):
    assertEquals(NetLanguageChecker.check(ic), Nil)

  test("an unknown left/right kind is rejected"):
    val bad = NetLanguage("bad", List(AgentKind("a", 0)), List(NetRule("r", "a", "ghost", Nil, Nil)))
    NetLanguageChecker.check(bad) match
      case List(RuleError.UnknownKind("r", "right", "ghost")) => ()
      case other => fail(s"expected UnknownKind, got $other")

  test("an unknown newAgents kind is rejected"):
    val bad = NetLanguage("bad", List(AgentKind("a", 0)),
      List(NetRule("r", "a", "a", List("ghost"), Nil)))
    val errs = NetLanguageChecker.check(bad)
    assert(errs.exists {
      case RuleError.UnknownKind("r", side, "ghost") => side.startsWith("newAgents")
      case _ => false
    }, errs)

  test("an out-of-range New port is rejected"):
    // "a" has arity 0 (only port 0 exists) — New(0, 1) is out of range
    val bad = NetLanguage("bad", List(AgentKind("a", 0)),
      List(NetRule("r", "a", "a", List("a"), List((RulePort.New(0, 1), RulePort.New(0, 1))))))
    assert(NetLanguageChecker.check(bad).exists(_.isInstanceOf[RuleError.NewAgentPortOutOfRange]))

  test("an out-of-range Ext port is rejected"):
    // "a" has arity 0 — Ext(0, 0) has no aux port 0 to reference
    val bad = NetLanguage("bad", List(AgentKind("a", 0)),
      List(NetRule("r", "a", "a", Nil, List((RulePort.Ext(0, 0), RulePort.Ext(1, 0))))))
    assert(NetLanguageChecker.check(bad).exists(_.isInstanceOf[RuleError.ExtPortOutOfRange]))

  test("a non-linear rule (a port used twice, another never) is rejected"):
    // "a" has arity 1: aux port ext(0,1) exists but is never connected here,
    // while the same New(0,0) port is used twice.
    val bad = NetLanguage("bad", List(AgentKind("a", 1)),
      List(NetRule("r", "a", "a", List("a"),
        List((RulePort.New(0, 0), RulePort.New(0, 0))))))
    val errs = NetLanguageChecker.check(bad)
    assert(errs.exists(_.isInstanceOf[RuleError.NotLinear]), errs)

  test("two different rules claiming the same unordered kind pair is ambiguous"):
    val bad = NetLanguage("bad", List(AgentKind("a", 0), AgentKind("b", 0)),
      List(NetRule("r1", "a", "b", Nil, Nil), NetRule("r2", "b", "a", Nil, Nil)))
    NetLanguageChecker.check(bad) match
      case List(RuleError.AmbiguousPair("a", "b", rules)) => assertEquals(rules.toSet, Set("r1", "r2"))
      case other => fail(s"expected AmbiguousPair, got $other")

  test("CompiledNetEngine rejects an ambiguous table at construction"):
    val bad = NetLanguage("bad", List(AgentKind("a", 0), AgentKind("b", 0)),
      List(NetRule("r1", "a", "b", Nil, Nil), NetRule("r2", "b", "a", Nil, Nil)))
    intercept[RuntimeException](CompiledNetEngine(bad))

  test("validate returns Left with all errors, Right when clean"):
    assertEquals(NetLanguageChecker.validate(ic), Right(ic))
    val bad = NetLanguage("bad", List(AgentKind("a", 0)), List(NetRule("r", "a", "ghost", Nil, Nil)))
    assert(NetLanguageChecker.validate(bad).isLeft)

  test("property: wellFormed(lang,net) and a successful applyPair implies wellFormed(lang,result)"):
    // Corpus: real STLC terms lowered to ic-net, so the nets exercise every
    // rule in the table (annihilation, commutation, erasure, konst-copying)
    // rather than a hand-picked toy shape.
    val two = Stlc.lam1("f", Stlc.tBool,
      Stlc.lam1("x", Stlc.tBool, Stlc.app1(Stlc.v("f"), Stlc.app1(Stlc.v("f"), Stlc.v("x")))))
    val corpus = List(
      Stlc.app1(Stlc.idBool, Stlc.tru),
      Stlc.app1(Stlc.app1(Stlc.churchTrue, Stlc.tru), Stlc.fls),
      Stlc.app1(Stlc.app1(Stlc.churchFalse, Stlc.tru), Stlc.fls),
      Stlc.app1(Stlc.app1(two, Stlc.idBool), Stlc.fls),
      Stlc.app1(Stlc.app1(two, Stlc.idBool), Stlc.tru))
    for term <- corpus do
      val Right((net0, _)) = IcNet.lower(term): @unchecked
      assertEquals(NetEngine.wellFormed(ic, net0), Right(()))
      for seed <- 1 to 20 do
        val rng = new scala.util.Random(seed * 104729 + term.render.hashCode)
        var net = net0
        var steps = 0
        var continue = true
        while continue && steps < 500 do
          val candidates = NetEngine.activePairs(net).flatMap(NetEngine.ruleFor(ic, net, _))
          if candidates.isEmpty then continue = false
          else
            val (rule, l, r) = candidates(rng.nextInt(candidates.length))
            NetEngine.applyPair(net, rule, l, r) match
              case Right(result) =>
                assertEquals(NetEngine.wellFormed(ic, result), Right(()),
                  s"term=${term.render} seed=$seed step=$steps rule=${rule.name}")
                net = result
                steps += 1
              case Left(e) => fail(s"applyPair failed: $e")
