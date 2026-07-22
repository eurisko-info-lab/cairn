package cairn.tests

import cairn.kernel.*
import cairn.core.{TreeEngine, NetEngine, Net, Agent, PortRef, RulePort}
import cairn.examples.affinenet.AffineNet
import cairn.examples.stlc.Stlc

/** Phase 3 acceptance (S25–S29): net reduction suite; well-formedness
  * judgments; no replicator constructible; lowering agrees with tree eval.
  */
class Phase3Suite extends munit.FunSuite:
  val lang = AffineNet.language

  test("nets are canonical artifacts (S25)"):
    val b = AffineNet.Builder()
    val e1 = b.agent("era"); val e2 = b.agent("era")
    b.wire(PortRef(e1, 0), PortRef(e2, 0))
    val net = b.net
    assertEquals(net.artifact.kind, ArtifactKind.Net)
    assertEquals(net.artifact.digest, net.artifact.digest)

  test("era-era annihilation (S26/S27)"):
    val b = AffineNet.Builder()
    val e1 = b.agent("era"); val e2 = b.agent("era")
    b.wire(PortRef(e1, 0), PortRef(e2, 0))
    val r = NetEngine.normalize(lang, b.net)
    assertEquals(r.map(_.agents.size), Right(0))

  test("fan-fan annihilation rewires aux ports pairwise (S26/S27)"):
    val b = AffineNet.Builder()
    val f1 = b.agent("fan"); val f2 = b.agent("fan")
    val frees = List.fill(4)(b.agent("free"))
    b.wire(PortRef(f1, 0), PortRef(f2, 0))
    b.wire(PortRef(f1, 1), PortRef(frees(0), 0))
    b.wire(PortRef(f1, 2), PortRef(frees(1), 0))
    b.wire(PortRef(f2, 1), PortRef(frees(2), 0))
    b.wire(PortRef(f2, 2), PortRef(frees(3), 0))
    val Right(n2) = NetEngine.normalize(lang, b.net): @unchecked
    assertEquals(n2.agents.keySet, frees.toSet)
    assertEquals(n2.peer(PortRef(frees(0), 0)), Some(PortRef(frees(2), 0)))
    assertEquals(n2.peer(PortRef(frees(1), 0)), Some(PortRef(frees(3), 0)))

  test("eraser propagates through fan (S26/S27)"):
    val b = AffineNet.Builder()
    val e = b.agent("era"); val f = b.agent("fan")
    val k1 = b.agent("konst"); val k2 = b.agent("konst")
    b.wire(PortRef(e, 0), PortRef(f, 0))
    b.wire(PortRef(f, 1), PortRef(k1, 0))
    b.wire(PortRef(f, 2), PortRef(k2, 0))
    val Right(n2) = NetEngine.normalize(lang, b.net): @unchecked
    assertEquals(n2.agents.size, 0) // erasers consumed both constants

  test("well-formedness: linearity violations rejected (S28)"):
    val b = AffineNet.Builder()
    val f = b.agent("fan")
    val e = b.agent("era")
    b.wire(PortRef(f, 0), PortRef(e, 0))
    // fan aux ports left unwired:
    NetEngine.wellFormed(lang, b.net) match
      case Left(errs) => assert(errs.exists(_.contains("unwired")), errs.mkString(";"))
      case Right(_)   => fail("expected well-formedness errors")

  test("well-formedness: undeclared kind rejected (S28)"):
    val net = Net(Map(0 -> Agent(0, "replicator")), Set.empty)
    assert(NetEngine.wellFormed(lang, net).swap.exists(_.exists(_.contains("undeclared kind"))))

  test("no replicator constructible (S27 acceptance, §6 Phase 3)"):
    assertEquals(lang.kinds.map(_.name).toSet, Set("fan", "era", "free", "konst"))
    // no rule creates more agents than it consumes while referencing an Ext twice
    for r <- lang.rules do
      val extUses = r.connections.flatMap((a, b) => List(a, b)).collect { case e: RulePort.Ext => e }
      assertEquals(extUses.distinct.size, extUses.size, s"rule ${r.name} duplicates an external port")

  test("lowered affine term reduces to same value as tree eval (S29 acceptance)"):
    // (fun x : Bool . x) true — tree eval gives true
    val term = Stlc.app1(Stlc.idBool, Stlc.tru)
    assertEquals(TreeEngine.normalize(Stlc.language, term), Right(Stlc.tru))
    val Right((net, root)) = AffineNet.lower(term): @unchecked
    assertEquals(NetEngine.wellFormed(AffineNet.language, net), Right(()))
    val Right(n2) = NetEngine.normalize(lang, net): @unchecked
    // the root's peer must now be the constant that `true` lowered to
    val peer = n2.peer(PortRef(root, 0)).get
    assertEquals(n2.agents(peer.agent).kind, "konst")

  test("unused binder gets an eraser; erased argument vanishes (S29)"):
    // (fun x : Bool . fun y : Bool . x) true  ~> fun y . true (net: konst survives, second binder erased)
    val term = Stlc.app1(Stlc.lam1("x", Stlc.tBool, Stlc.lam1("y", Stlc.tBool, Stlc.v("x"))), Stlc.tru)
    val Right((net, root)) = AffineNet.lower(term): @unchecked
    assertEquals(NetEngine.wellFormed(AffineNet.language, net), Right(()))
    val Right(n2) = NetEngine.normalize(lang, net): @unchecked
    // normal form: a fan (the remaining λy) at the root
    val peer = n2.peer(PortRef(root, 0)).get
    assertEquals(n2.agents(peer.agent).kind, "fan")

  test("non-affine term rejected by lowering (S29)"):
    val dup = Stlc.lam1("x", Stlc.tBool, Stlc.app1(Stlc.v("x"), Stlc.v("x")))
    assert(AffineNet.lower(dup).swap.exists(_.contains("not affine")))
