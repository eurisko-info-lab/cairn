package cairn.tests

import cairn.kernel.*
import cairn.workbench.*
import cairn.core.*
import cairn.systemhandler.EffectContext
import cairn.examples.law.LawTutorial
import cairn.examples.stlc.Stlc

/** Exemplar languages as `.cairn` data + PKI → Law → SDS dependency DAG. */
class ExemplarPackSuite extends munit.FunSuite:
  private val packs = PackLoader(EffectContext.bootstrapped())
  private val Pki = cairn.examples.pki.Pki(packs)
  private val Law = cairn.examples.law.Law(packs)
  private val Sds = cairn.examples.sds.Sds(packs)

  test("packs load from languages/*.cairn at runtime"):
    val raw = packs.loadRaw()
    assert(raw.contains("pki"), raw.keySet.toString)
    assert(raw.contains("law"), raw.keySet.toString)
    assert(raw.contains("sds"), raw.keySet.toString)
    assert(raw.contains("search"), raw.keySet.toString)
    assert(raw.contains("stlc"), raw.keySet.toString)
    assert(!raw.contains("dpki") && !raw.contains("dlaw") && !raw.contains("dsds") && !raw.contains("dsearch"))

  test("PKI closes alone; provides cert"):
    val lang = packs.requireClosed("pki")
    assertEquals(lang.name, "pki")
    assert(lang.fragments.exists(_.provides.contains("cert")))
    assertEquals(lang.digest, Pki.language.digest)

  test("Law own fragment requires cert — compose without PKI fails"):
    val unmet = packs.unmetRequires("law", packs.loadRaw())
    assertEquals(unmet, Set("cert"))
    Law.ownCompose match
      case Left(errs) =>
        assert(errs.exists(_.detail.contains("required interface 'cert'")), errs.map(_.render).mkString)
      case Right(_) => fail("Law must not compose without PKI cert")

  test("SDS own fragment requires law — compose without Law fails"):
    val unmet = packs.unmetRequires("sds", packs.loadRaw())
    assertEquals(unmet, Set("law"))
    Sds.ownCompose match
      case Left(errs) =>
        assert(errs.exists(_.detail.contains("required interface 'law'")), errs.map(_.render).mkString)
      case Right(_) => fail("SDS must not compose without Law")

  test("SDS + Law without PKI still fails (transitive cert)"):
    val raw = packs.loadRaw()
    val lawFs = raw("law")
    val sdsFs = raw("sds")
    Compose.compose("sds-no-pki", sdsFs ++ lawFs.map(PackLoader.demote)) match
      case Left(errs) =>
        assert(errs.exists(_.detail.contains("required interface 'cert'")), errs.map(_.render).mkString)
      case Right(_) => fail("SDS+Law without PKI must fail")

  test("closed Law pulls PKI; closed SDS pulls Law+PKI"):
    val law = packs.requireClosed("law")
    assert(law.constructors.contains("cert"), "Law closed must include PKI cert")
    assert(law.constructors.contains("enactedBy"), "Law cites PKI via enactedBy")
    assert(law.fragments.exists(_.provides.contains("cert")))
    assert(law.fragments.exists(_.provides.contains("law")))

    val sds = packs.requireClosed("sds")
    assert(sds.constructors.contains("cert"), "SDS closed must include PKI cert")
    assert(sds.constructors.contains("section") || sds.constructors.contains("enactedBy"),
      "SDS closed must include Law constructors")
    assert(sds.constructors.contains("basis"), "SDS cites Law via basis")
    assert(sds.fragments.exists(_.provides.contains("sds")))
    assert(sds.fragments.exists(_.provides.contains("law")))
    assert(sds.fragments.exists(_.provides.contains("cert")))

  test("exemplar .cairn text round-trips the meta surface"):
    for name <- List("pki", "law", "sds") do
      val fs = packs.requireOwn(name)
      val text = Meta.printLanguage(name, fs).fold(e => fail(e), identity)
      val back = Meta.parseLanguageAst(text).fold(e => fail(e), identity)
      assertEquals(back._1, name)
      assertEquals(back._2.map(_.digest), fs.map(_.digest))
      val surf = packs.requireSurface(name)
      val sText = Meta.printSurface(surf.name, surf.language, surf.fragments).fold(e => fail(e), identity)
      val sBack = Meta.parseSurfaceAst(sText).fold(e => fail(e), identity)
      assertEquals(sBack._1, surf.name)
      assertEquals(sBack._2, surf.language)
      assertEquals(sBack._3.map(_.digest), surf.fragments.map(_.digest))

  test("language digest ignores surface edits"):
    val lang = packs.requireClosed("search")
    val surf = packs.requireSurface("search")
    // rebinding the same surface is identity on language digest
    val rebound = Compose.compose("search",
      PackLoader.bindSurface(packs.requireOwn("search"), surf)).fold(e => fail(e.map(_.render).mkString), identity)
    assertEquals(rebound.digest, lang.digest)
    assertEquals(rebound.grammar, lang.grammar)

  test("alternate STLC surface changes surface digest, not language digest"):
    val langDefault = packs.requireClosed("stlc")
    val langHs = packs.requireClosed("stlc", "haskell-style")
    assertEquals(langHs.digest, langDefault.digest)
    val dSurf = packs.requireSurface("stlc", "default")
    val hsSurf = packs.requireSurface("stlc", "haskell-style")
    assert(dSurf.digest != hsSurf.digest)
    // haskell-style uses `lam` instead of `fun`
    val term = Parser.parse(langHs.grammar, "lam x : Bool . x").fold(e => fail(e), identity)
    assertEquals(term, Stlc.idBool)

  test("free ΔL is derived only — add/remove via Delta.deltaOf, not on disk"):
    val dl = Delta.deltaOf(Pki.language).fold(e => fail(e.map(_.render).mkString), identity)
    assert(dl.constructors.keySet.exists(_.startsWith("remove:")))
    assert(dl.constructors.keySet.exists(_.startsWith("add:")))
    assert(!java.nio.file.Files.exists(java.nio.file.Path.of("languages/dpki.cairn")))
    assert(!java.nio.file.Files.exists(java.nio.file.Path.of("docs/delta/Δpki.cairn")))

  test("Law tutorial: citations + enactedBy cert + repeal"):
    val r = LawTutorial.run()
    assert(r.languageRequiresMet)
    assert(r.citationOk)
    assertEquals(r.enactedByCert, "root")
    assert(r.repealDangling)
    assert(r.languageProvides.contains("law"))
    assert(r.languageProvides.contains("cert"))

  test("SDS basis cites Law section; invalid basis rejected"):
    val base = cairn.examples.sds.SdsTutorial.acetoneBase
    Sds.validate(base).fold(e => fail(e), identity)
    assert(base.get("regBasis").exists {
      case Cst.Node("basis", List(Cst.Leaf("cleanerProduct"), Cst.Leaf("3"))) => true
      case _ => false
    })
    val dl = Delta.deltaOf(Sds.language).fold(e => fail(e.map(_.render).mkString), identity)
    val bad = Parser.parse(dl.grammar,
      """{ add badBasis = basis phantom section "9" ; }""").fold(e => fail(e), identity)
    assert(Sds.applySds(base, bad).swap.exists(_.contains("unknown product 'phantom'")))
