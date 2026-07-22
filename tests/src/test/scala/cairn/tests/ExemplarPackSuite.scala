package cairn.tests

import cairn.kernel.*
import cairn.workbench.*
import cairn.core.*
import cairn.systemhandler.EffectContext
import cairn.examples.law.LawTutorial
import cairn.examples.stlc.Stlc

/** Exemplar languages as `.cairn` data + PKI → Law → SDS dependency DAG. */
class ExemplarPackSuite extends munit.FunSuite:
  private val packs = PackLoader(EffectContext.forPackLoader())
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
    assert(sds.constructors.contains("euSection"), "SDS EU-CLP section bodies")
    assert(sds.constructors.contains("outline"), "SDS section outlines")
    assert(sds.constructors.contains("sectionField"), "SDS section fields")
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

  test("SDS phrase-staleness: free-text FR goes stale when EN source hash changes"):
    import cairn.examples.sds.PhraseStaleness
    val enHash1 = PhraseStaleness.textHash("Acetone Cleaner")
    val translations = Map(
      "en" -> PhraseStaleness.TranslatedText("Acetone Cleaner", enHash1.hex, PhraseStaleness.State.HumanReviewed),
      "fr" -> PhraseStaleness.TranslatedText("Nettoyant acetone", enHash1.hex, PhraseStaleness.State.HumanReviewed))
    val enHash2 = PhraseStaleness.textHash("Acetone Cleaner (industrial)")
    val restaled = PhraseStaleness.restale(translations, enHash2)
    assertEquals(restaled("fr").state, PhraseStaleness.State.StaleBecauseSourceChanged)
    assertEquals(restaled("en").state, PhraseStaleness.State.HumanReviewed)

  test("SDS phrase-staleness: corpusPhrase never stales; acetone H-phrases are corpus"):
    import cairn.examples.sds.PhraseStaleness
    val base = cairn.examples.sds.SdsTutorial.acetoneBase
    assert(Sds.language.constructors.contains("corpusPhrase"))
    assert(base.get("h225").exists {
      case Cst.Node("corpusPhrase", _) => true
      case _ => false
    })
    val projected = PhraseStaleness.project(base, "h225")
    assertEquals(projected("en").state, PhraseStaleness.State.OfficialCorpus)
    assertEquals(projected("fr").state, PhraseStaleness.State.OfficialCorpus)
    val restaled = PhraseStaleness.restale(projected, PhraseStaleness.textHash("other"))
    assertEquals(restaled("fr").state, PhraseStaleness.State.OfficialCorpus)
    assertEquals(
      PhraseStaleness.staleLangsAfterEnChange(base, "h225", "rewritten hazard"),
      Set.empty)

  test("SDS phrase-staleness: acetone free-text prodName FR stales on EN rewrite"):
    import cairn.examples.sds.PhraseStaleness
    val base = cairn.examples.sds.SdsTutorial.acetoneBase
    val projected = PhraseStaleness.project(base, "prodName")
    assertEquals(projected("fr").state, PhraseStaleness.State.HumanReviewed)
    assertEquals(
      PhraseStaleness.staleLangsAfterEnChange(base, "prodName", "Acetone Cleaner (new)"),
      Set("fr"))
    // matching hash keeps FR fresh
    val same = PhraseStaleness.restale(projected, PhraseStaleness.textHash("Acetone Cleaner"))
    assertEquals(same("fr").state, PhraseStaleness.State.HumanReviewed)

  test("SDS section numbering: EU-CLP has exactly sections 1..16 in order"):
    import cairn.examples.sds.SectionNumbering
    assertEquals(SectionNumbering.numbers, (1 to 16).toList)
    assertEquals(SectionNumbering.euClp.map(_.number), (1 to 16).toList)
    assertEquals(SectionNumbering.titleOf(1), Right("Identification"))
    assertEquals(SectionNumbering.titleOf(16), Right("Other information"))
    assert(SectionNumbering.titleOf(0).isLeft)
    assert(SectionNumbering.titleOf(17).isLeft)
    assert(SectionNumbering.parseNumber("9").contains(9))
    assert(SectionNumbering.parseNumber("0").isLeft)
    assert(SectionNumbering.parseNumber("x").isLeft)

  test("SDS section numbering: outline rejects bad title, duplicate, out-of-order"):
    import cairn.examples.sds.SectionNumbering
    import SectionNumbering.{OutlineEntry, OutlineError}
    val ok = SectionNumbering.validateOutline(SectionNumbering.acetoneSparseOutline)
    assertEquals(ok.map(_.map(_.number)), Right(List(2, 3)))
    val badTitle = SectionNumbering.validateOutline(List(
      OutlineEntry(2, "Wrong heading")))
    assert(badTitle.swap.exists(_.exists {
      case OutlineError.TitleMismatch(2, "Wrong heading", "Hazards identification") => true
      case _ => false
    }))
    val dup = SectionNumbering.validateOutline(List(
      OutlineEntry(2, "Hazards identification"),
      OutlineEntry(2, "Hazards identification")))
    assert(dup.swap.exists(_.exists {
      case OutlineError.Duplicate(2) => true
      case _ => false
    }))
    val unordered = SectionNumbering.validateOutline(List(
      OutlineEntry(3, "Composition/information on ingredients"),
      OutlineEntry(2, "Hazards identification")))
    assert(unordered.swap.exists(_.exists {
      case OutlineError.OutOfOrder(List(3, 2)) => true
      case _ => false
    }))
    // order() sorts a bag into ascending SectionDefs
    assertEquals(
      SectionNumbering.order(List(16, 1, 2)).map(_.map(_.number)),
      Right(List(1, 2, 16)))
    assert(SectionNumbering.order(List(1, 99)).isLeft)

  test("SDS chemicals corpus: acetone fuller outline is all 16 EU-CLP sections"):
    import cairn.examples.sds.{Chemicals, SectionNumbering}
    val acetone = Chemicals.Acetone.pure
    assertEquals(acetone.populatedNumbers, (1 to 16).toList)
    assertEquals(acetone.cas, "67-64-1")
    val validated = acetone.validateOutline
    assertEquals(validated.map(_.map(_.number)), Right((1 to 16).toList))
    assertEquals(
      validated.map(_.map(_.title)),
      Right(SectionNumbering.euClp.map(_.title)))
    // every section carries at least one honest field
    assert(acetone.sections.values.forall(_.fields.nonEmpty))
    assert(acetone.sections(16).fields("otherInformation").contains("Demo/example"))
    // tutorial spine remains the sparse 2+3 subset
    assertEquals(
      Chemicals.acetoneTutorialSparse.map(_.number),
      List(2, 3))

  test("SDS chemicals corpus: ethanol sparse outline still validates"):
    import cairn.examples.sds.Chemicals
    val ethanol = Chemicals.Ethanol.pure
    assertEquals(ethanol.populatedNumbers, List(1, 2))
    assertEquals(ethanol.validateOutline.map(_.map(_.number)), Right(List(1, 2)))

  test("SDS section report: acetone 16-section map round-trips"):
    import cairn.examples.sds.{Chemicals, SectionReport}
    val text = SectionReport.render(Chemicals.Acetone.pure).fold(e => fail(e), identity)
    assert(text.startsWith("SDS REPORT: \"Acetone\" CAS: \"67-64-1\""))
    assert(text.contains("section 1 \"Identification\""))
    assert(text.contains("section 16 \"Other information\""))
    assert(text.contains("field otherInformation :"))
    assert(text.contains("Demo/example"))
    // every EU-CLP section appears once, ascending
    val sectionHeaders = text.linesIterator.filter(_.startsWith("section ")).toList
    assertEquals(sectionHeaders.size, 16)
    assertEquals(
      sectionHeaders.map(_.split(" ")(1).toInt),
      (1 to 16).toList)

  test("SDS section report: ethanol sparse map round-trips"):
    import cairn.examples.sds.{Chemicals, SectionReport}
    val text = SectionReport.render(Chemicals.Ethanol.pure).fold(e => fail(e), identity)
    assert(text.contains("section 1 \"Identification\""))
    assert(text.contains("section 2 \"Hazards identification\""))
    assert(!text.contains("section 3 "))
    assertEquals(
      text.linesIterator.count(_.startsWith("section ")),
      2)

  test("SDS section report: rejects outline that fails SectionNumbering"):
    import cairn.examples.sds.{Chemicals, SectionReport}
    val bad = Chemicals.ChemicalDoc(
      name = "Bad",
      cas = "0-0-0",
      sections = Map(
        1 -> Chemicals.SectionBody(1, Map("productName" -> "X")),
        99 -> Chemicals.SectionBody(99, Map("oops" -> "nope"))))
    val err = SectionReport.render(bad).swap.toOption.getOrElse(fail("expected Left"))
    assert(err.contains("BadNumber") || err.toLowerCase.contains("out of range") || err.contains("99"),
      clues(err))

  test("SDS section report: empty document still round-trips"):
    import cairn.examples.sds.{Chemicals, SectionReport}
    val empty = Chemicals.ChemicalDoc("Empty", "0-0-0", Map.empty)
    val text = SectionReport.render(empty).fold(e => fail(e), identity)
    assertEquals(text.trim, "SDS REPORT: \"Empty\" CAS: \"0-0-0\"")

  test("SDS language sections: euSection/outline parse/print round-trip"):
    val g = Sds.language.grammar
    val sec = Parser.parse(g,
      """eu section 2 fields ( hazardPhrases lang en : "H225; H319" , signalWord lang en : "Danger" )""")
      .fold(e => fail(e), identity)
    RoundTrip.check(g, sec).fold(e => fail(e), identity)
    assertEquals(sec,
      Cst.node("euSection", Cst.Leaf("2"),
        Cst.Node("list", List(
          Cst.node("sectionField", Cst.Leaf("hazardPhrases"), Cst.Leaf("en"), Cst.Leaf("H225; H319")),
          Cst.node("sectionField", Cst.Leaf("signalWord"), Cst.Leaf("en"), Cst.Leaf("Danger"))))))
    val outline = Parser.parse(g, """outline "Acetone" "67-64-1" sections ( s1 , s2 )""")
      .fold(e => fail(e), identity)
    RoundTrip.check(g, outline).fold(e => fail(e), identity)
    assertEquals(outline,
      Cst.node("outline", Cst.Leaf("Acetone"), Cst.Leaf("67-64-1"),
        Cst.Node("some", List(Cst.Node("list", List(Cst.Leaf("s1"), Cst.Leaf("s2")))))))
    val emptyOutline = Parser.parse(g, """outline "Empty" "0-0-0" sections ( )""")
      .fold(e => fail(e), identity)
    RoundTrip.check(g, emptyOutline).fold(e => fail(e), identity)
    assertEquals(emptyOutline,
      Cst.node("outline", Cst.Leaf("Empty"), Cst.Leaf("0-0-0"), Cst.Node("none", Nil)))

  test("SDS language sections: acetone thin module validates and round-trips"):
    import cairn.examples.sds.Chemicals
    val m = Chemicals.Acetone.thinModule
    Sds.validate(m).fold(e => fail(e), identity)
    assertEquals(m.defs.count(_._2 match
      case Cst.Node("euSection", _) => true
      case _ => false), 3)
    assert(m.get("acetoneOutline").exists {
      case Cst.Node("outline", List(Cst.Leaf("Acetone"), Cst.Leaf("67-64-1"),
          Cst.Node("some", List(Cst.Node("list", refs))))) =>
        refs.map { case Cst.Leaf(n) => n; case _ => "?" } == List("s1", "s2", "s16")
      case _ => false
    })
    for (_, term) <- m.defs do
      RoundTrip.check(Sds.language.grammar, term).fold(e => fail(e), identity)

  test("SDS language sections: full acetone 16-section module loads through pack"):
    import cairn.examples.sds.Chemicals
    val m = Chemicals.Acetone.asModule
    Sds.validate(m).fold(e => fail(e), identity)
    assertEquals(m.defs.count(_._2 match
      case Cst.Node("euSection", _) => true
      case _ => false), 16)
    // pack still closes with the new constructors
    assert(Sds.language.constructors.contains("euSection"))
    assert(packs.requireClosed("sds").digest == Sds.language.digest)

  test("SDS language sections: ΔSDS edits euSection; domain gate rejects bad numbers/refs"):
    import cairn.examples.sds.Chemicals
    val base = Chemicals.Acetone.thinModule
    val dl = Delta.deltaOf(Sds.language).fold(e => fail(e.map(_.render).mkString), identity)
    val ok = Parser.parse(dl.grammar,
      """{ replace s2 = eu section 2 fields ( signalWord lang en : "Warning" ) ; }""")
      .fold(e => fail(e), identity)
    val Right((m2, _)) = Sds.applySds(base, ok): @unchecked
    assert(m2.get("s2").exists {
      case Cst.Node("euSection", List(Cst.Leaf("2"), Cst.Node("list", List(
          Cst.Node("sectionField", List(Cst.Leaf("signalWord"), Cst.Leaf("en"),
            Cst.Leaf("Warning"))))))) => true
      case _ => false
    })
    val badNum = Parser.parse(dl.grammar,
      """{ replace s2 = eu section 99 fields ( oops lang en : "nope" ) ; }""")
      .fold(e => fail(e), identity)
    assert(Sds.applySds(base, badNum).swap.exists(_.contains("out of range")))
    val dangling = Parser.parse(dl.grammar,
      """{ replace acetoneOutline = outline "Acetone" "67-64-1" sections ( s1 , phantom ) ; }""")
      .fold(e => fail(e), identity)
    assert(Sds.applySds(base, dangling).swap.exists(_.contains("unknown section 'phantom'")))

  test("SDS multilingual section fields: parse + phrase-style lang fallback"):
    val g = Sds.language.grammar
    val sec = Parser.parse(g,
      """eu section 2 fields (
        |  signalWord lang en : "Danger" ,
        |  signalWord lang fr : "Danger" ,
        |  hazardPhrases lang en : "H225; H319"
        |)""".stripMargin)
      .fold(e => fail(e), identity)
    RoundTrip.check(g, sec).fold(e => fail(e), identity)
    Sds.validate(Module(List("s2" -> sec))).fold(e => fail(e), identity)
    assertEquals(Sds.sectionFieldText(sec, "signalWord", "fr"), Some("Danger"))
    assertEquals(Sds.sectionFieldText(sec, "signalWord", "de"), Some("Danger")) // → en
    assertEquals(Sds.sectionFieldText(sec, "hazardPhrases", "fr"), Some("H225; H319")) // → en
    assertEquals(Sds.sectionFieldText(sec, "missing", "en"), None)
    // ΔSDS can add a FR sibling without replacing EN
    val base = Module(List("s2" -> sec))
    val dl = Delta.deltaOf(Sds.language).fold(e => fail(e.map(_.render).mkString), identity)
    val addFr = Parser.parse(dl.grammar,
      """{ replace s2 = eu section 2 fields (
        |  signalWord lang en : "Danger" ,
        |  signalWord lang fr : "Danger" ,
        |  hazardPhrases lang en : "H225; H319" ,
        |  hazardPhrases lang fr : "H225; H319"
        |) ; }""".stripMargin)
      .fold(e => fail(e), identity)
    val Right((m2, _)) = Sds.applySds(base, addFr): @unchecked
    assertEquals(Sds.sectionFieldText(m2, "s2", "hazardPhrases", "fr"), Some("H225; H319"))
    val dup = Parser.parse(dl.grammar,
      """{ replace s2 = eu section 2 fields (
        |  signalWord lang en : "Danger" ,
        |  signalWord lang en : "Warning"
        |) ; }""".stripMargin)
      .fold(e => fail(e), identity)
    assert(Sds.applySds(base, dup).swap.exists(_.contains("duplicate field")))
