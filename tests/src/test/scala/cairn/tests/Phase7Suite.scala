package cairn.tests

import cairn.kernel.*
import cairn.workbench.*
import cairn.ledger.*
import cairn.examples.stlc.Stlc

/** Phase 7 acceptance (S43–S44): agreement between kernel values and
  * independent app recomputation; self-description bootstrap.
  */
class Phase7Suite extends munit.FunSuite:

  test("agreement: ledger state root — kernel replay vs app recomputation (S43)"):
    val alice = Keypair.dev("alice")
    val auth = Map("alice" -> alice.publicBytes)
    val node = Node(java.nio.file.Files.createTempDirectory("cairn-agree"))
    node.cas.put(Stlc.base.artifact)
    val txs = List(
      alice.signTx(Tx.RegisterIdentity("alice", alice.publicBytes)),
      alice.signTx(Tx.PublishArtifact(Stlc.base.artifact.key)),
      alice.signTx(Tx.SetBranchHead("main", Stlc.base.artifact.key)))
    val block = node.append(alice, auth, txs).toOption.get
    // kernel value
    val kernelRoot = node.state(auth).toOption.get.root
    // independent app-side recomputation: naive fold, separate code path
    val appState = LedgerState(
      identities = Map("alice" -> alice.publicBytes),
      published = Set(Stlc.base.artifact.key.render),
      heads = Map("main" -> Stlc.base.artifact.key),
      certificates = Map.empty)
    assertEquals(kernelRoot, appState.root)
    assertEquals(block.stateRoot, appState.root)

  test("agreement: module digest after ΔL vs direct construction (S43)"):
    val lang = Stlc.language
    val dl = Delta.deltaOf(lang).toOption.get
    val change = Parser.parse(dl.grammar, "{ add id = fun x : Bool . x ; }").toOption.get
    val Right((viaDelta, _)) = Delta.apply(lang, Module(Nil), change): @unchecked
    val direct = Module(List("id" -> Stlc.idBool)).sorted
    assertEquals(viaDelta.digest, direct.digest)

  test("agreement: digest determinism over permuted canonical maps (S43)"):
    val perms = List(
      Canon.cmap("a" -> Canon.CInt(1), "b" -> Canon.CStr("x"), "c" -> Canon.clist(Canon.CInt(2))),
      Canon.cmap("c" -> Canon.clist(Canon.CInt(2)), "a" -> Canon.CInt(1), "b" -> Canon.CStr("x")),
      Canon.cmap("b" -> Canon.CStr("x"), "c" -> Canon.clist(Canon.CInt(2)), "a" -> Canon.CInt(1)))
    assertEquals(perms.map(Digest.of).distinct.size, 1)

  test("self-description: surface-defined fragment == host-defined fragment (S44 acceptance)"):
    // host-side value
    val host = Fragment(
      name = "pair",
      provides = List("pair"),
      requires = List("term"),
      sorts = List(SortDef("Pair", SortMode.Tree)),
      constructors = List(
        CtorDef("mkPair", "Pair", List("Term", "Term")),
        CtorDef("letPair", "Term", List("Name", "Name", "Pair", "Term"), binders = List((0, List(3)), (1, List(3))))),
      varCtor = Some("var"))
    // the same fragment written in Cairn's own meta surface
    val src = """
      fragment pair provides pair requires term {
        sort Pair tree ;
        ctor mkPair : Pair (Term, Term) ;
        ctor letPair : Term (Name, Name, Pair, Term) binds 0 in 3 binds 1 in 3 ;
        varctor var ;
      }"""
    Meta.parseFragment(src) match
      case Left(e) => fail(e)
      case Right(surface) =>
        assertEquals(surface, host)
        assertEquals(surface.digest, host.digest)

  test("self-description: meta surface round-trips (S44 + S11 law)"):
    val src = "fragment tiny provides tiny { sort T tree ; ctor unit : T ; }"
    val g = Meta.grammar.copy(top = "fragmentDecl")
    val cst = Parser.parse(g, src).toOption.get
    RoundTrip.check(g, cst).fold(e => fail(e), identity)

  test("self-description: surface fragment composes with host fragments (S44)"):
    val src = """
      fragment pairs requires term {
        sort Pair tree ;
        ctor mkPair : Pair (Term, Term) ;
      }"""
    val frag = Meta.parseFragment(src).toOption.get
    Compose.compose("stlc-with-pairs", Stlc.fragments :+ frag) match
      case Right(lang) => assert(lang.constructors.contains("mkPair"))
      case Left(errs)  => fail(errs.map(_.render).mkString("\n"))
