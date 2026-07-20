package cairn.tests

import cairn.kernel.*
import cairn.workbench.*
import cairn.examples.pki.Pki
import cairn.examples.law.{Law, LawTutorial}
import cairn.examples.sds.Sds

/** Exemplar languages as `.cairn` data + PKI → Law → SDS dependency DAG. */
class ExemplarPackSuite extends munit.FunSuite:

  test("packs load from languages/*.cairn at runtime"):
    val raw = PackLoader.loadRaw()
    assert(raw.contains("pki"), raw.keySet.toString)
    assert(raw.contains("law"), raw.keySet.toString)
    assert(raw.contains("sds"), raw.keySet.toString)
    assert(raw.contains("search"), raw.keySet.toString)
    assert(raw.contains("stlc"), raw.keySet.toString)
    assert(!raw.contains("dpki") && !raw.contains("dlaw") && !raw.contains("dsds") && !raw.contains("dsearch"))

  test("PKI closes alone; provides cert"):
    val lang = PackLoader.requireClosed("pki")
    assertEquals(lang.name, "pki")
    assert(lang.fragments.exists(_.provides.contains("cert")))
    assertEquals(lang.digest, Pki.language.digest)

  test("Law own fragment requires cert — compose without PKI fails"):
    val unmet = PackLoader.unmetRequires("law", PackLoader.loadRaw())
    assertEquals(unmet, Set("cert"))
    Law.ownCompose match
      case Left(errs) =>
        assert(errs.exists(_.detail.contains("required interface 'cert'")), errs.map(_.render).mkString)
      case Right(_) => fail("Law must not compose without PKI cert")

  test("SDS own fragment requires law — compose without Law fails"):
    val unmet = PackLoader.unmetRequires("sds", PackLoader.loadRaw())
    assertEquals(unmet, Set("law"))
    Sds.ownCompose match
      case Left(errs) =>
        assert(errs.exists(_.detail.contains("required interface 'law'")), errs.map(_.render).mkString)
      case Right(_) => fail("SDS must not compose without Law")

  test("SDS + Law without PKI still fails (transitive cert)"):
    val raw = PackLoader.loadRaw()
    val lawFs = raw("law")
    val sdsFs = raw("sds")
    Compose.compose("sds-no-pki", sdsFs ++ lawFs.map(PackLoader.demote)) match
      case Left(errs) =>
        assert(errs.exists(_.detail.contains("required interface 'cert'")), errs.map(_.render).mkString)
      case Right(_) => fail("SDS+Law without PKI must fail")

  test("closed Law pulls PKI; closed SDS pulls Law+PKI"):
    val law = PackLoader.requireClosed("law")
    assert(law.constructors.contains("cert"), "Law closed must include PKI cert")
    assert(law.constructors.contains("enactedBy"), "Law cites PKI via enactedBy")
    assert(law.fragments.exists(_.provides.contains("cert")))
    assert(law.fragments.exists(_.provides.contains("law")))

    val sds = PackLoader.requireClosed("sds")
    assert(sds.constructors.contains("cert"), "SDS closed must include PKI cert")
    assert(sds.constructors.contains("section") || sds.constructors.contains("enactedBy"),
      "SDS closed must include Law constructors")
    assert(sds.constructors.contains("basis"), "SDS cites Law via basis")
    assert(sds.fragments.exists(_.provides.contains("sds")))
    assert(sds.fragments.exists(_.provides.contains("law")))
    assert(sds.fragments.exists(_.provides.contains("cert")))

  test("exemplar .cairn text round-trips the meta surface"):
    for name <- List("pki", "law", "sds") do
      val fs = PackLoader.requireOwn(name)
      val text = Meta.printLanguage(name, fs).fold(e => fail(e), identity)
      val back = Meta.parseLanguageAst(text).fold(e => fail(e), identity)
      assertEquals(back._1, name)
      assertEquals(back._2.map(_.digest), fs.map(_.digest))

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
