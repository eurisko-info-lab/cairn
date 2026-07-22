package cairn.tests

import cairn.kernel.*
import cairn.core.{TreeEngine, CompiledTreeEngine, CompiledNetEngine, NetBuilder, NetEngine, Net, PortRef}
import cairn.examples.stlc.Stlc
import cairn.examples.affinenet.AffineNet
import cairn.examples.icnet.IcNet
import cairn.examples.bend.Bend

/** Wave E acceptance (M25–M29). */
class WaveESuite extends munit.FunSuite:
  val ic = IcNet.language

  // ---- M25: full interaction combinators ----

  test("M25: dup-dup annihilation"):
    val b = NetBuilder()
    val d1 = b.agent("dup"); val d2 = b.agent("dup")
    val frees = List.fill(4)(b.agent("free"))
    b.wire(PortRef(d1, 0), PortRef(d2, 0))
    b.wire(PortRef(d1, 1), PortRef(frees(0), 0)); b.wire(PortRef(d1, 2), PortRef(frees(1), 0))
    b.wire(PortRef(d2, 1), PortRef(frees(2), 0)); b.wire(PortRef(d2, 2), PortRef(frees(3), 0))
    val Right(n2) = NetEngine.normalize(ic, b.net): @unchecked
    assertEquals(n2.agents.keySet, frees.toSet)
    assertEquals(n2.peer(PortRef(frees(0), 0)), Some(PortRef(frees(2), 0)))

  test("M25: gamma-delta commutation duplicates past each other"):
    val b = NetBuilder()
    val f = b.agent("fan"); val d = b.agent("dup")
    val frees = List.fill(4)(b.agent("free"))
    b.wire(PortRef(f, 0), PortRef(d, 0))
    b.wire(PortRef(f, 1), PortRef(frees(0), 0)); b.wire(PortRef(f, 2), PortRef(frees(1), 0))
    b.wire(PortRef(d, 1), PortRef(frees(2), 0)); b.wire(PortRef(d, 2), PortRef(frees(3), 0))
    val Right(Some(n2)) = NetEngine.step(ic, b.net): @unchecked
    // 4 new agents (2 fans + 2 dups) + the 4 frees
    assertEquals(n2.agents.size, 8)
    assertEquals(n2.agents.values.count(_.kind == "fan"), 2)
    assertEquals(n2.agents.values.count(_.kind == "dup"), 2)
    assertEquals(NetEngine.wellFormed(ic, n2), Right(()))

  test("M25: dup copies a labelled constant"):
    val b = NetBuilder()
    val d = b.agent("dup"); val k = b.agent("konst", "true")
    val f1 = b.agent("free"); val f2 = b.agent("free")
    b.wire(PortRef(d, 0), PortRef(k, 0))
    b.wire(PortRef(d, 1), PortRef(f1, 0)); b.wire(PortRef(d, 2), PortRef(f2, 0))
    val Right(n2) = NetEngine.normalize(ic, b.net): @unchecked
    val konsts = n2.agents.values.filter(_.kind == "konst").toList
    assertEquals(konsts.length, 2)
    assert(konsts.forall(_.label == "true"))

  test("M25: AffineNet still proves structural absence of replicators"):
    assert(!AffineNet.language.kinds.exists(_.name == "dup"))

  // ---- M26: general lowering + readback ----

  test("M26: duplicated argument evaluates correctly on nets"):
    // (λf. λx. f (f x)) (λy. y) true — f used twice => δ required
    val two = Stlc.lam1("f", Stlc.tBool,
      Stlc.lam1("x", Stlc.tBool, Stlc.app1(Stlc.v("f"), Stlc.app1(Stlc.v("f"), Stlc.v("x")))))
    val term = Stlc.app1(Stlc.app1(two, Stlc.idBool), Stlc.tru)
    assertEquals(TreeEngine.normalize(Stlc.language, term), Right(Stlc.tru))
    val Right((net, root)) = IcNet.lower(term): @unchecked
    assertEquals(NetEngine.wellFormed(ic, net), Right(()))
    val Right(normal) = NetEngine.normalize(ic, net): @unchecked
    assertEquals(IcNet.readback(normal, root), Right(Stlc.tru))

  test("M26: readback of a lambda normal form is alpha-equivalent to tree eval"):
    // (λd. d d) (λy. y)  ~~>  λy. y (self-application NEEDS duplication)
    val selfApp = Stlc.lam1("d", Stlc.tBool, Stlc.app1(Stlc.v("d"), Stlc.v("d")))
    val term = Stlc.app1(selfApp, Stlc.idBool)
    val Right(treeValue) = TreeEngine.normalize(Stlc.language, term): @unchecked
    val Right((net, root)) = IcNet.lower(term): @unchecked
    val Right(normal) = NetEngine.normalize(ic, net): @unchecked
    val Right(netValue) = IcNet.readback(normal, root): @unchecked
    val spec = Stlc.language.binderSpec
    assert(Alpha.equivalent(spec, "var")(treeValue, netValue),
      s"tree: ${treeValue.render} vs net: ${netValue.render}")

  test("M26: readback agrees with tree eval across a corpus"):
    val two = Stlc.lam1("f", Stlc.tBool,
      Stlc.lam1("x", Stlc.tBool, Stlc.app1(Stlc.v("f"), Stlc.app1(Stlc.v("f"), Stlc.v("x")))))
    val corpus = List(
      Stlc.app1(Stlc.idBool, Stlc.tru),
      Stlc.app1(Stlc.app1(Stlc.churchTrue, Stlc.tru), Stlc.fls),
      Stlc.app1(Stlc.app1(Stlc.churchFalse, Stlc.tru), Stlc.fls),
      Stlc.app1(Stlc.app1(two, Stlc.idBool), Stlc.fls))
    val spec = Stlc.language.binderSpec
    for term <- corpus do
      val Right(expected) = TreeEngine.normalize(Stlc.language, term): @unchecked
      val Right((net, root)) = IcNet.lower(term): @unchecked
      val Right(normal) = NetEngine.normalize(ic, net): @unchecked
      val Right(actual) = IcNet.readback(normal, root): @unchecked
      assert(Alpha.equivalent(spec, "var")(expected, actual),
        s"${term.render}: tree ${expected.render} vs net ${actual.render}")

  // ---- M27: parallel reduction ----

  test("M27: parallel and sequential reduction reach identical normal forms"):
    val two = Stlc.lam1("f", Stlc.tBool,
      Stlc.lam1("x", Stlc.tBool, Stlc.app1(Stlc.v("f"), Stlc.app1(Stlc.v("f"), Stlc.v("x")))))
    val term = Stlc.app1(Stlc.app1(two, Stlc.idBool), Stlc.tru)
    val Right((net, root)) = IcNet.lower(term): @unchecked
    val Right(seq) = NetEngine.normalize(ic, net): @unchecked
    val Right((par, stats)) = NetEngine.normalizeParallel(ic, net): @unchecked
    assertEquals(IcNet.readback(par, root), IcNet.readback(seq, root))
    assert(stats.sweeps > 0)
    assert(stats.pairsPerSweep.sum >= stats.sweeps) // some sweep did >1 pair or all single

  test("property: net reduction is confluent — random reduction order reaches the same normal form"):
    // Confluence was previously asserted only by a code comment plus one
    // fixed-term example (the M27 test above). This upgrades it to the same
    // evidentiary standard the rest of the suite uses for its invariants
    // (e.g. the seeded causal-LCA property test): many random reduction
    // SCHEDULES (not just parallel-vs-sequential) over a real corpus, all
    // required to land on the identical readback.
    def stepRandom(rng: scala.util.Random, net: Net): Either[String, Option[Net]] =
      val candidates = NetEngine.activePairs(net).flatMap(NetEngine.ruleFor(ic, net, _))
      if candidates.isEmpty then Right(None)
      else
        val (rule, l, r) = candidates(rng.nextInt(candidates.length))
        NetEngine.applyPair(net, rule, l, r).map(Some(_))
    def normalizeRandom(rng: scala.util.Random, net: Net, fuel: Int = 10_000): Either[String, Net] =
      if fuel <= 0 then Left("out of fuel")
      else stepRandom(rng, net).flatMap {
        case Some(n2) => normalizeRandom(rng, n2, fuel - 1)
        case None     => Right(net)
      }
    val two = Stlc.lam1("f", Stlc.tBool,
      Stlc.lam1("x", Stlc.tBool, Stlc.app1(Stlc.v("f"), Stlc.app1(Stlc.v("f"), Stlc.v("x")))))
    val corpus = List(
      Stlc.app1(Stlc.idBool, Stlc.tru),
      Stlc.app1(Stlc.app1(Stlc.churchTrue, Stlc.tru), Stlc.fls),
      Stlc.app1(Stlc.app1(Stlc.churchFalse, Stlc.tru), Stlc.fls),
      Stlc.app1(Stlc.app1(two, Stlc.idBool), Stlc.fls),
      Stlc.app1(Stlc.app1(two, Stlc.idBool), Stlc.tru))
    for term <- corpus do
      val Right((net, root)) = IcNet.lower(term): @unchecked
      val Right(canonical) = NetEngine.normalize(ic, net): @unchecked
      val canonicalReadback = IcNet.readback(canonical, root)
      for seed <- 1 to 40 do
        val rng = new scala.util.Random(seed * 7919 + term.render.hashCode)
        val Right(shuffled) = normalizeRandom(rng, net): @unchecked
        assertEquals(IcNet.readback(shuffled, root), canonicalReadback, s"seed=$seed term=${term.render}")

  // ---- genuine concurrent reduction (real threads, not just fewer sweeps) ----

  test("normalizeConcurrent: agent-disjoint pairs sharing an aux wire are NOT run concurrently"):
    // The hazard this guards against: two active (principal-principal) pairs
    // that are agent-disjoint (so parallelStep's existing selection would
    // batch them together) but share an AUXILIARY wire between them — a
    // completely ordinary net shape, not a contrived edge case. Computing
    // each independently from the same snapshot would resolve that shared
    // wire two different, inconsistent ways. d1.aux2 <-> d3.aux1 below is
    // exactly that: two separate dup-dup active pairs, cross-wired.
    given scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    val b = NetBuilder()
    val d1 = b.agent("dup"); val d2 = b.agent("dup")
    val d3 = b.agent("dup"); val d4 = b.agent("dup")
    val frees = List.fill(6)(b.agent("free"))
    b.wire(PortRef(d1, 0), PortRef(d2, 0)) // pair 1: active
    b.wire(PortRef(d3, 0), PortRef(d4, 0)) // pair 2: active, agent-disjoint from pair 1
    b.wire(PortRef(d1, 1), PortRef(frees(0), 0))
    b.wire(PortRef(d1, 2), PortRef(d3, 1))          // cross-wire: d1.aux2 <-> d3.aux1
    b.wire(PortRef(d2, 1), PortRef(frees(1), 0)); b.wire(PortRef(d2, 2), PortRef(frees(2), 0))
    b.wire(PortRef(d3, 2), PortRef(frees(3), 0))
    b.wire(PortRef(d4, 1), PortRef(frees(4), 0)); b.wire(PortRef(d4, 2), PortRef(frees(5), 0))
    val net = b.net
    assertEquals(NetEngine.wellFormed(ic, net), Right(()))
    val Right(Some((_, total, threaded))) = NetEngine.parallelStepConcurrent(ic, net): @unchecked
    assertEquals(total, 2, "both pairs are agent-disjoint, so both get selected")
    assertEquals(threaded, 0, "but the aux cross-wire means NEITHER is safe for real concurrency")
    val Right(expected) = NetEngine.normalize(ic, net): @unchecked
    val Right((actual, _, _)) = NetEngine.normalizeConcurrent(ic, net): @unchecked
    assertEquals(actual, expected)

  test("normalizeConcurrent: truly independent pairs DO run on real threads, and agree with the interpretive engine"):
    given scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    // Same shape as above, minus the cross-wire: two dup-dup pairs with no
    // wire at all between them — the case genuine concurrency is safe for.
    val b = NetBuilder()
    val d1 = b.agent("dup"); val d2 = b.agent("dup")
    val d3 = b.agent("dup"); val d4 = b.agent("dup")
    val frees = List.fill(6)(b.agent("free"))
    b.wire(PortRef(d1, 0), PortRef(d2, 0))
    b.wire(PortRef(d3, 0), PortRef(d4, 0))
    b.wire(PortRef(d1, 1), PortRef(frees(0), 0)); b.wire(PortRef(d1, 2), PortRef(frees(1), 0))
    b.wire(PortRef(d2, 1), PortRef(frees(2), 0)); b.wire(PortRef(d2, 2), PortRef(frees(3), 0))
    b.wire(PortRef(d3, 1), PortRef(frees(4), 0)); b.wire(PortRef(d3, 2), PortRef(frees(5), 0))
    val frees2 = List(b.agent("free"), b.agent("free"))
    b.wire(PortRef(d4, 1), PortRef(frees2(0), 0)); b.wire(PortRef(d4, 2), PortRef(frees2(1), 0))
    val net = b.net
    assertEquals(NetEngine.wellFormed(ic, net), Right(()))
    val Right(Some((_, total, threaded))) = NetEngine.parallelStepConcurrent(ic, net): @unchecked
    assertEquals(total, 2)
    assertEquals(threaded, 2, "no wire between the two pairs: both are safe for real concurrency")
    val Right(expected) = NetEngine.normalize(ic, net): @unchecked
    val Right((actual, _, threadedTotal)) = NetEngine.normalizeConcurrent(ic, net): @unchecked
    assertEquals(actual, expected)
    assert(threadedTotal > 0)

  test("property: normalizeConcurrent agrees with the interpretive engine over the STLC-lowered corpus"):
    given scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    val two = Stlc.lam1("f", Stlc.tBool,
      Stlc.lam1("x", Stlc.tBool, Stlc.app1(Stlc.v("f"), Stlc.app1(Stlc.v("f"), Stlc.v("x")))))
    val corpus = List(
      Stlc.app1(Stlc.idBool, Stlc.tru),
      Stlc.app1(Stlc.app1(Stlc.churchTrue, Stlc.tru), Stlc.fls),
      Stlc.app1(Stlc.app1(Stlc.churchFalse, Stlc.tru), Stlc.fls),
      Stlc.app1(Stlc.app1(two, Stlc.idBool), Stlc.fls),
      Stlc.app1(Stlc.app1(two, Stlc.idBool), Stlc.tru))
    for term <- corpus do
      val Right((net, root)) = IcNet.lower(term): @unchecked
      val Right(expected) = NetEngine.normalize(ic, net): @unchecked
      val Right((actual, _, _)) = NetEngine.normalizeConcurrent(ic, net): @unchecked
      assertEquals(IcNet.readback(actual, root), IcNet.readback(expected, root), term.render)

  // ---- compiled net dispatch (mirrors M28's CompiledTreeEngine, for nets) ----

  test("compiled net dispatch agrees with the interpretive NetEngine over the STLC-lowered corpus"):
    val compiled = CompiledNetEngine(ic)
    val two = Stlc.lam1("f", Stlc.tBool,
      Stlc.lam1("x", Stlc.tBool, Stlc.app1(Stlc.v("f"), Stlc.app1(Stlc.v("f"), Stlc.v("x")))))
    val corpus = List(
      Stlc.app1(Stlc.idBool, Stlc.tru),
      Stlc.app1(Stlc.app1(Stlc.churchTrue, Stlc.tru), Stlc.fls),
      Stlc.app1(Stlc.app1(Stlc.churchFalse, Stlc.tru), Stlc.fls),
      Stlc.app1(Stlc.app1(two, Stlc.idBool), Stlc.fls),
      Stlc.app1(Stlc.app1(two, Stlc.idBool), Stlc.tru))
    for term <- corpus do
      val Right((net, root)) = IcNet.lower(term): @unchecked
      val Right(expected) = NetEngine.normalize(ic, net): @unchecked
      val Right(actual) = compiled.normalize(net): @unchecked
      assertEquals(IcNet.readback(actual, root), IcNet.readback(expected, root), term.render)

  test("compiled net dispatch agrees on the M25 fixed-net fixtures and actually dispatches"):
    val compiled = CompiledNetEngine(ic)
    val b1 = NetBuilder()
    val d1 = b1.agent("dup"); val d2 = b1.agent("dup")
    val frees1 = List.fill(4)(b1.agent("free"))
    b1.wire(PortRef(d1, 0), PortRef(d2, 0))
    b1.wire(PortRef(d1, 1), PortRef(frees1(0), 0)); b1.wire(PortRef(d1, 2), PortRef(frees1(1), 0))
    b1.wire(PortRef(d2, 1), PortRef(frees1(2), 0)); b1.wire(PortRef(d2, 2), PortRef(frees1(3), 0))
    assertEquals(compiled.normalize(b1.net), NetEngine.normalize(ic, b1.net))
    compiled.visits = 0
    assert(compiled.normalize(b1.net).isRight)
    assert(compiled.visits > 0)

  // ---- M28: compiled rules ----

  test("M28: compiled engine agrees with interpretive reference over corpus"):
    val compiled = CompiledTreeEngine(Stlc.language)
    val rnd = new scala.util.Random(99)
    def gen(d: Int, env: List[String]): Cst =
      rnd.nextInt(if d <= 0 then 2 else 5) match
        case 0 => Stlc.tru
        case 1 => Stlc.fls
        case 2 if env.nonEmpty => Stlc.v(env(rnd.nextInt(env.length)))
        case 2 => Stlc.fls
        case 3 =>
          val x = s"v${env.length}"
          Stlc.app1(Stlc.lam1(x, Stlc.tBool, gen(d - 1, x :: env)), gen(d - 1, env))
        case _ => Cst.node("if", gen(d - 1, env), gen(d - 1, env), gen(d - 1, env))
    for _ <- 1 to 300 do
      val t = gen(5, Nil)
      assertEquals(compiled.normalize(t), TreeEngine.normalize(Stlc.language, t))

  test("M28: compiled dispatch prunes non-matching visits"):
    val compiled = CompiledTreeEngine(Stlc.language)
    val deep = (1 to 30).foldLeft(Stlc.tru) { (acc, i) =>
      Stlc.app1(Stlc.lam1(s"x$i", Stlc.tBool, Stlc.v(s"x$i")), acc) }
    compiled.visits = 0
    assertEquals(compiled.normalize(deep), TreeEngine.normalize(Stlc.language, deep))
    assert(compiled.visits > 0)

  // ---- M29: Bend-profile surface ----

  val bendProgram = """
    |# bend-profile demo: duplication via interaction combinators
    |def id = @x ( x )
    |def apply2 = @f ( @y ( f ( f y ) ) )
    |def main = apply2 id tru
    |""".stripMargin

  test("M29: bend program parses, lowers, reduces, reads back"):
    assertEquals(Bend.run(bendProgram), Right(Cst.node("true")))

  test("M29: bend surface round-trips"):
    val cst = cairn.core.Parser.parse(Bend.grammar, bendProgram).fold(e => fail(e), identity)
    cairn.core.RoundTrip.check(Bend.grammar, cst).fold(e => fail(e), identity)

  test("M29: bend errors are honest"):
    assert(Bend.run("def main = ghost").swap.exists(_.contains("unbound variable 'ghost'")))
    assert(Bend.run("def notmain = tru").swap.exists(_.contains("no `def main`")))

  test("M29+: Bend Church numeral 0/2 via isZero; self-ref still unbound"):
    // Numerals are Church encodings over existing lam/app nets — not HVM NUM.
    val isZero0 =
      """
        |def isZero = @n ( n (@_ (fls)) tru )
        |def main = isZero 0
        |""".stripMargin
    assertEquals(Bend.run(isZero0), Right(Cst.node("true")))
    val isZero2 =
      """
        |def isZero = @n ( n (@_ (fls)) tru )
        |def main = isZero 2
        |""".stripMargin
    assertEquals(Bend.run(isZero2), Right(Cst.node("false")))
    // Honest boundary: self-referencing def is not a primitive let-rec —
    // inlining skips self-subst, so `loop` stays unbound.
    assert(Bend.run("def loop = loop\ndef main = loop").isLeft)
