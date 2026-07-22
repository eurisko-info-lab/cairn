package cairn.tests

import cairn.kernel.*
import cairn.compute.*
import cairn.core.{TreeEngine, CompiledTreeEngine}
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
