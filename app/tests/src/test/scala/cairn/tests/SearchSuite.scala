package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.PackLoader
import cairn.systemhandler.{DiskCas, EffectContext, Provenance}
import cairn.examples.search.SearchTutorial
import java.nio.file.Files

/** Fact–Intent–Hint search pack: object language + CAS provenance spine. */
class SearchSuite extends munit.FunSuite:
  private val packs = PackLoader(EffectContext.forPackLoader())
  private val Search = cairn.examples.search.Search(packs)

  test("search pack loads from languages/*.cairn at runtime"):
    val raw = packs.loadRaw()
    assert(raw.contains("search"), raw.keySet.toString)
    assert(!raw.contains("dsearch"))

  test("search composes standalone — no unmet requires"):
    val unmet = packs.unmetRequires("search", packs.loadRaw())
    assertEquals(unmet, Set.empty[String])
    Search.ownCompose match
      case Right(lang) =>
        assert(lang.fragments.exists(_.provides.contains("search")))
        assert(lang.judgments.contains("wellFormed"))
        assert(lang.judgments.contains("goalMet"))
      case Left(errs) => fail(errs.map(_.render).mkString)

  test("search.cairn text round-trips the meta surface"):
    val fs = packs.requireOwn("search")
    val text = Meta.printLanguage("search", fs).fold(e => fail(e), identity)
    val back = Meta.parseLanguageAst(text).fold(e => fail(e), identity)
    assertEquals(back._1, "search")
    assertEquals(back._2.map(_.digest), fs.map(_.digest))

  test("board terms round-trip under search grammar"):
    val g = Search.language.grammar
    for s <- List(
      """origin "start"""",
      """goal "done"""",
      """fact "confirmed"""",
      """intent "explore"""",
      """hint "look here"""",
      """supports a b""",
      """spawns a b""",
      """board (a, b, c)"""
    ) do
      val t = Parser.parse(g, s).fold(e => fail(e), identity)
      RoundTrip.check(g, t).fold(e => fail(e), identity)

  test("free ΔL is derived only — no dsearch.cairn on disk"):
    val dl = Delta.deltaOf(Search.language).fold(e => fail(e.map(_.render).mkString), identity)
    assert(dl.constructors.keySet.exists(_.startsWith("add:")))
    assert(dl.constructors.keySet.exists(_.startsWith("remove:")))
    assert(!java.nio.file.Files.exists(java.nio.file.Path.of("content/languages/dsearch.cairn")))
    assert(!java.nio.file.Files.exists(java.nio.file.Path.of("docs/delta/Δsearch.cairn")))

  test("wellFormed rejects dangling edge; goalMet after Fact"):
    val seed = cairn.examples.search.Search.seedBoard
    assert(Search.wellFormed(seed).isRight)
    assert(!Search.goalMet(seed))
    val dl = Delta.deltaOf(Search.language).fold(e => fail(e.map(_.render).mkString), identity)
    val bad = Parser.parse(dl.grammar, """{ add e = supports phantom finding ; }""")
      .fold(e => fail(e), identity)
    assert(Search.applySearch(seed, bad).swap.exists(_.contains("unknown")))
    val good = Parser.parse(dl.grammar,
      """{ add explore = intent "work" ; add finding = fact "done" ; add e = supports explore finding ; }"""
    ).fold(e => fail(e), identity)
    val board = Search.applySearch(seed, good).fold(e => fail(e), _._1)
    assert(Search.wellFormed(board).isRight)
    assert(Search.goalMet(board))

  test("SearchTutorial: Fact write records Intent→Fact provenance"):
    val dir = Files.createTempDirectory("search-tutorial")
    val r = SearchTutorial.run(dir)
    assert(r.languageRequiresMet)
    assert(r.wellFormed)
    assert(r.goalMet)
    assert(r.whyHops >= 1, r.toString)
    assert(r.whyTools.contains("explore"), r.whyTools.toString)
    val hops = Provenance.why(dir.resolve("cas"), Digest(r.factDigest), EffectContext.forCas())
      .fold(e => fail(e), identity)
    assert(hops.exists(_.record.inputs.exists(_.hex == r.intentDigest)), hops.toString)
    assert(r.languageProvides.contains("search"))

  test("graphOf extracts nodes and supports/spawns edges"):
    val g = Search.graphOf(cairn.examples.search.Search.seedBoard)
    assertEquals(g.nodes.map(_.kind).toSet, Set("origin", "goal"))
    val dl = Delta.deltaOf(Search.language).fold(e => fail(e.map(_.render).mkString), identity)
    val ch = Parser.parse(dl.grammar,
      """{ add i = intent "x" ; add f = fact "y" ; add e = supports i f ; }"""
    ).fold(e => fail(e), identity)
    val board = Search.applySearch(cairn.examples.search.Search.seedBoard, ch).fold(e => fail(e), _._1)
    val graph = Search.graphOf(board)
    assert(graph.edges.exists(e => e.kind == "supports" && e.from == "i" && e.to == "f"))

  test("certifyEdge: why walks Intent→Fact→Certificate; dangling edge rejected"):
    val dir = Files.createTempDirectory("search-edge-dag")
    val cas = DiskCas(dir.resolve("cas"))
    val dl = Delta.deltaOf(Search.language).fold(e => fail(e.map(_.render).mkString), identity)
    val ch = Parser.parse(dl.grammar,
      """{ add explore = intent "work" ; add finding = fact "done" ; add link = supports explore finding ; }"""
    ).fold(e => fail(e), identity)
    val board = Search.applySearch(cairn.examples.search.Search.seedBoard, ch).fold(e => fail(e), _._1)
    val intentDig = Search.putTerm(cas, board.get("explore").get)
    val factDig = Search.putFact(cas, board.get("finding").get, intentDig)
    val edge = Search.certifyEdge(cas, board, "link", factDig).fold(e => fail(e), identity)
    assertEquals(edge.certificate.method, "test-suite")
    assertEquals(edge.certificate.claim, edge.claim.artifact.digest)
    val hops = Provenance.why(dir.resolve("cas"), edge.certDigest, EffectContext.forCas())
      .fold(e => fail(e), identity)
    assert(hops.exists(_.record.inputs.exists(_ == factDig)), hops.toString)
    assert(hops.exists(_.record.inputs.exists(_ == intentDig)) ||
      hops.exists(h => h.record.output == factDig && h.record.inputs.exists(_ == intentDig)),
      hops.toString)
    // full chain: Certificate ← Fact ← Intent
    val factHop = hops.find(_.record.output == factDig)
    assert(factHop.exists(_.record.inputs.exists(_ == intentDig)), hops.toString)
    val certHop = hops.find(_.record.output == edge.certDigest)
    assert(certHop.exists(_.record.inputs.exists(_ == factDig)), hops.toString)

    val dangling = Module(board.defs :+ ("bad" -> Cst.node("supports", Cst.Leaf("phantom"), Cst.Leaf("finding"))))
    assert(Search.wellFormed(dangling).swap.exists(_.contains("unknown")))
    assert(Search.certifyEdge(cas, dangling, "bad", factDig).swap.exists(_.contains("unknown")))

  private def boardWithEdge: Module =
    val dl = Delta.deltaOf(Search.language).fold(e => fail(e.map(_.render).mkString), identity)
    val ch = Parser.parse(dl.grammar,
      """{ add explore = intent "work" ; add finding = fact "done" ; add link = supports explore finding ; }"""
    ).fold(e => fail(e), identity)
    Search.applySearch(cairn.examples.search.Search.seedBoard, ch).fold(e => fail(e), _._1)

  test("checkWellFormed: kernel-certified for nonempty leaf terms and resolvable edges"):
    val m = boardWithEdge
    assert(Search.checkWellFormed(m, Cst.node("fact", Cst.Leaf("done"))).isRight)
    assert(Search.checkWellFormed(m, Cst.node("goal", Cst.Leaf("reach a confirmed finding"))).isRight)
    assert(Search.checkWellFormed(m, Cst.node("supports", Cst.Leaf("explore"), Cst.Leaf("finding"))).isRight)

  test("checkWellFormed: kernel rejects empty text ($neq gates it, not just the host check)"):
    val m = boardWithEdge
    assert(Search.checkWellFormed(m, Cst.node("origin", Cst.Leaf(""))).isLeft)

  test("checkWellFormed: kernel rejects a dangling edge endpoint ($ctx-lookup gates it)"):
    val m = boardWithEdge
    assert(Search.checkWellFormed(m, Cst.node("supports", Cst.Leaf("explore"), Cst.Leaf("phantom"))).isLeft)

  test("checkGoalMet: kernel-certified for a genuine (goal, fact) witness pair"):
    val m = boardWithEdge
    assert(Search.checkGoalMet(m, "target", "finding").isRight)

  test("checkGoalMet: kernel rejects a witness pair where the 'fact' name isn't a fact"):
    val m = boardWithEdge
    assert(Search.checkGoalMet(m, "target", "explore").isLeft) // explore is an intent, not a fact

  test("Checker.check rejects a forged derivation directly, independent of search"):
    val m = boardWithEdge
    val forgedConclusion = Cst.node("wellFormed", Search.boardCtx(m),
      Cst.node("supports", Cst.Leaf("explore"), Cst.Leaf("phantom")))
    val forged = Derivation("wf-supports", forgedConclusion, Nil)
    assert(Checker.check(Search.checkerCfg, forged).isLeft)
