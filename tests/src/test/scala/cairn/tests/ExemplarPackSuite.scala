package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.PackLoader
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
    assert(sds.constructors.contains("identificationSection"), "SDS typed identification")
    assert(sds.constructors.contains("hazardsSection"), "SDS typed hazards")
    assert(sds.constructors.contains("compositionSection"), "SDS typed composition")
    assert(sds.constructors.contains("firstAidSection"), "SDS typed first-aid")
    assert(sds.constructors.contains("firefightingSection"), "SDS typed firefighting")
    assert(sds.constructors.contains("accidentalReleaseSection"), "SDS typed accidental release")
    assert(sds.constructors.contains("handlingStorageSection"), "SDS typed handling/storage")
    assert(sds.constructors.contains("exposureControlsSection"), "SDS typed exposure controls")
    assert(sds.constructors.contains("physicalChemicalSection"), "SDS typed physical/chemical")
    assert(sds.constructors.contains("stabilityReactivitySection"), "SDS typed stability/reactivity")
    assert(sds.constructors.contains("toxicologicalSection"), "SDS typed toxicological")
    assert(sds.constructors.contains("ecologicalSection"), "SDS typed ecological")
    assert(sds.constructors.contains("disposalSection"), "SDS typed disposal")
    assert(sds.constructors.contains("transportSection"), "SDS typed transport")
    assert(sds.constructors.contains("regulatorySection"), "SDS typed regulatory")
    assert(sds.constructors.contains("otherInformationSection"), "SDS typed other information")
    assert(sds.constructors.contains("outline"), "SDS section outlines")
    assert(sds.constructors.contains("sectionField"), "SDS section fields")
    assert(sds.constructors.contains("sectionFieldShadow"), "SDS section-field shadows")
    assert(sds.constructors.contains("sectionFieldState"), "SDS section-field translation state")
    assert(sds.constructors.contains("translationState"), "SDS phrase translation state")
    assert(sds.constructors.contains("fieldLocale"), "SDS typed section locale overlays")
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

  test("SDS phrase-staleness: derived ΔL materializes translationState on EN rewrite"):
    import cairn.examples.sds.PhraseStaleness
    val base = cairn.examples.sds.SdsTutorial.acetoneBase
    val oldHash = PhraseStaleness.textHash("Acetone Cleaner")
    val Right((m2, vcs)) = PhraseStaleness.applyEnRewrite(
      Sds.applySds, Sds.language, base, "prodName", "Acetone Cleaner (industrial)"): @unchecked
    assertEquals(vcs.base, base.digest)
    assertEquals(vcs.result, m2.digest)
    assert(m2.get("prodNameEn").exists {
      case Cst.Node("phrase", List(Cst.Leaf("prodName"), Cst.Leaf("en"),
          Cst.Leaf("Acetone Cleaner (industrial)"))) => true
      case _ => false
    })
    val mark = PhraseStaleness.markName("prodName", "fr")
    assert(m2.get(mark).exists {
      case Cst.Node("translationState", List(
          Cst.Leaf("prodName"), Cst.Leaf("fr"), Cst.Leaf(h),
          Cst.Leaf("staleBecauseSourceChanged"))) =>
        h == oldHash.hex
      case _ => false
    })
    val projected = PhraseStaleness.project(m2, "prodName")
    assertEquals(projected("fr").state, PhraseStaleness.State.StaleBecauseSourceChanged)
    assertEquals(projected("fr").translatedFromHash, oldHash.hex)
    assertEquals(projected("en").text, "Acetone Cleaner (industrial)")
    // corpus EN rewrite rejected; no mark added for h225 FR
    assert(PhraseStaleness.deriveEnRewrite(
      Sds.language, base, "h225", "rewritten").swap.exists(_.contains("corpusPhrase")))
    assert(Sds.checkTranslationStateTag("staleBecauseSourceChanged"))
    assert(Sds.checkTranslationStateTag("officialCorpus"))
    assert(!Sds.checkTranslationStateTag("notARealTag"))
    assert(Sds.language.judgments.contains("translationStateTag"))
    // gate rejects unknown state tags
    val dl = Delta.deltaOf(Sds.language).fold(e => fail(e.map(_.render).mkString), identity)
    val bad = Parser.parse(dl.grammar,
      """{ add badMark = translation state prodName lang de from "abc" as "bogus" ; }""")
      .fold(e => fail(e), identity)
    assert(Sds.applySds(base, bad).swap.exists(_.contains("unknown state tag")))

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

  test("SDS chemicals corpus: ethanol fuller outline is all 16 EU-CLP sections"):
    import cairn.examples.sds.{Chemicals, SectionNumbering}
    val ethanol = Chemicals.Ethanol.pure
    assertEquals(ethanol.populatedNumbers, (1 to 16).toList)
    assertEquals(ethanol.cas, "64-17-5")
    val validated = ethanol.validateOutline
    assertEquals(validated.map(_.map(_.number)), Right((1 to 16).toList))
    assertEquals(
      validated.map(_.map(_.title)),
      Right(SectionNumbering.euClp.map(_.title)))
    assert(ethanol.sections.values.forall(_.fields.nonEmpty))
    assert(ethanol.sections(16).fields("otherInformation").contains("secondary ethanol"))
    // legacy sparse 1+2 contrast still validates
    assertEquals(Chemicals.Ethanol.sparseLegacy.populatedNumbers, List(1, 2))
    assertEquals(
      Chemicals.Ethanol.sparseLegacy.validateOutline.map(_.map(_.number)),
      Right(List(1, 2)))

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

  test("SDS section report: ethanol 16-section map round-trips"):
    import cairn.examples.sds.{Chemicals, SectionReport}
    val text = SectionReport.render(Chemicals.Ethanol.pure).fold(e => fail(e), identity)
    assert(text.startsWith("SDS REPORT: \"Ethanol\" CAS: \"64-17-5\""))
    assert(text.contains("section 1 \"Identification\""))
    assert(text.contains("section 16 \"Other information\""))
    assert(text.contains("UN1170"))
    assert(text.contains("secondary ethanol"))
    val sectionHeaders = text.linesIterator.filter(_.startsWith("section ")).toList
    assertEquals(sectionHeaders.size, 16)
    assertEquals(
      sectionHeaders.map(_.split(" ")(1).toInt),
      (1 to 16).toList)

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
    assert(m.get("s1").exists {
      case Cst.Node("identificationSection", _) => true
      case _ => false
    })
    assert(m.get("s2").exists {
      case Cst.Node("hazardsSection", _) => true
      case _ => false
    })
    assert(m.get("s3").exists {
      case Cst.Node("compositionSection", _) => true
      case _ => false
    })
    assert(m.get("s9").exists {
      case Cst.Node("physicalChemicalSection", _) => true
      case _ => false
    })
    assert(m.get("s11").exists {
      case Cst.Node("toxicologicalSection", _) => true
      case _ => false
    })
    assert(m.get("s14").exists {
      case Cst.Node("transportSection", _) => true
      case _ => false
    })
    assert(m.get("s16").exists {
      case Cst.Node("otherInformationSection", _) => true
      case _ => false
    })
    assertEquals(m.defs.count(_._2 match
      case Cst.Node("euSection", _) => true
      case _ => false), 0)
    assert(m.get("acetoneOutline").exists {
      case Cst.Node("outline", List(Cst.Leaf("Acetone"), Cst.Leaf("67-64-1"),
          Cst.Node("some", List(Cst.Node("list", refs))))) =>
        refs.map { case Cst.Leaf(n) => n; case _ => "?" } ==
          List("s1", "s2", "s3", "s9", "s11", "s14", "s16")
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
    assert(Sds.language.constructors.contains("identificationSection"))
    assert(packs.requireClosed("sds").digest == Sds.language.digest)

  test("SDS language sections: ethanol thin module validates and round-trips"):
    import cairn.examples.sds.Chemicals
    val m = Chemicals.Ethanol.thinModule
    Sds.validate(m).fold(e => fail(e), identity)
    assert(m.get("s1").exists {
      case Cst.Node("identificationSection", _) => true
      case _ => false
    })
    assert(m.get("s2").exists {
      case Cst.Node("hazardsSection", _) => true
      case _ => false
    })
    assert(m.get("s3").exists {
      case Cst.Node("compositionSection", _) => true
      case _ => false
    })
    assert(m.get("s16").exists {
      case Cst.Node("otherInformationSection", _) => true
      case _ => false
    })
    assertEquals(m.defs.count(_._2 match
      case Cst.Node("euSection", _) => true
      case _ => false), 0)
    assert(m.get("ethanolOutline").exists {
      case Cst.Node("outline", List(Cst.Leaf("Ethanol"), Cst.Leaf("64-17-5"),
          Cst.Node("some", List(Cst.Node("list", refs))))) =>
        refs.map { case Cst.Leaf(n) => n; case _ => "?" } ==
          List("s1", "s2", "s3", "s9", "s11", "s14", "s16")
      case _ => false
    })
    for (_, term) <- m.defs do
      RoundTrip.check(Sds.language.grammar, term).fold(e => fail(e), identity)

  test("SDS language sections: full ethanol 16-section module loads through pack"):
    import cairn.examples.sds.Chemicals
    val m = Chemicals.Ethanol.asModule
    Sds.validate(m).fold(e => fail(e), identity)
    assertEquals(m.defs.count(_._2 match
      case Cst.Node("euSection", _) => true
      case _ => false), 16)
    assert(m.get("ethanolOutline").exists {
      case Cst.Node("outline", List(Cst.Leaf("Ethanol"), Cst.Leaf("64-17-5"),
          Cst.Node("some", List(Cst.Node("list", refs))))) =>
        refs.size == 16
      case _ => false
    })
    for (_, term) <- m.defs do
      RoundTrip.check(Sds.language.grammar, term).fold(e => fail(e), identity)

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
    assert(Sds.applySds(base, badNum).swap.exists(_.contains("sectionNumberOk")))
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

  test("SDS section-field staleness: FR goes stale when EN source hash changes"):
    import cairn.examples.sds.SectionFieldStaleness
    import cairn.examples.sds.PhraseStaleness
    val g = Sds.language.grammar
    val sec = Parser.parse(g,
      """eu section 2 fields (
        |  signalWord lang en : "Danger" ,
        |  signalWord lang fr : "Danger"
        |)""".stripMargin)
      .fold(e => fail(e), identity)
    val m = Module(List("s2" -> sec))
    val projected = SectionFieldStaleness.project(m, "s2", "signalWord")
    assertEquals(projected("fr").state, PhraseStaleness.State.HumanReviewed)
    assertEquals(
      SectionFieldStaleness.staleLangsAfterEnChange(m, "s2", "signalWord", "Warning"),
      Set("fr"))
    val same = PhraseStaleness.restale(projected, PhraseStaleness.textHash("Danger"))
    assertEquals(same("fr").state, PhraseStaleness.State.HumanReviewed)

  test("SDS chemicals fixtures: acetone thin FR section fields + optional restale"):
    import cairn.examples.sds.{Chemicals, SectionFieldStaleness, PhraseStaleness, SectionReport}
    val m = Chemicals.Acetone.thinModule
    Sds.validate(m).fold(e => fail(e), identity)
    assertEquals(Sds.sectionFieldText(m, "s1", "productName", "fr"), Some("Acétone"))
    assert(Sds.sectionFieldText(m, "s1", "usesAdvisedAgainst", "fr").exists(_.startsWith("Utilisation")))
    assertEquals(Sds.sectionFieldText(m, "s2", "signalWord", "fr"), Some("Danger"))
    assertEquals(Sds.sectionFieldText(m, "s2", "hazardPhrases", "fr"), Some("H225 ; H319 ; H336"))
    assertEquals(Sds.sectionFieldText(m, "s3", "componentName", "fr"), Some("Acétone"))
    assertEquals(Sds.sectionFieldText(m, "s9", "appearance", "fr"), Some("Liquide incolore."))
    // untranslated EN-only key still falls back
    assertEquals(
      Sds.sectionFieldText(m, "s3", "cas", "fr"),
      Sds.sectionFieldText(m, "s3", "cas", "en"))
    assert(Sds.sectionFieldText(m, "s16", "otherInformation", "fr").exists(_.contains("démonstration")))
    // corpus-ref signalWord: free-text restale is on productName, not signalWord FR
    assertEquals(
      SectionFieldStaleness.staleLangsAfterEnChange(m, "s1", "productName", "Acetone (lab)"),
      Set("fr"))
    val projected = SectionFieldStaleness.project(m, "s1", "productName")
    assertEquals(projected("fr").state, PhraseStaleness.State.HumanReviewed)
    // FR report projection resolves corpus refs
    val frXml = SectionReport.renderXml(m, "acetoneOutline", "fr").fold(e => fail(e), identity)
    assert(frXml.contains("Acétone"), frXml)
    assert(frXml.contains("Danger"), frXml)
    // host SectionReport doc maps stay EN-primary
    assertEquals(Chemicals.Acetone.thin.sections(1).fields("productName"), "Acetone")
    assert(Chemicals.Acetone.thin.sections(1).locales("fr").contains("productName"))
    for (_, term) <- m.defs do
      RoundTrip.check(Sds.language.grammar, term).fold(e => fail(e), identity)

  test("SDS chemicals fixtures: ethanol thin FR+DE + corpus signalWord"):
    import cairn.examples.sds.{Chemicals, SectionFieldStaleness, PhraseStaleness}
    val m = Chemicals.Ethanol.thinModule
    Sds.validate(m).fold(e => fail(e), identity)
    assertEquals(Sds.sectionFieldText(m, "s1", "productName", "fr"), Some("Éthanol"))
    assertEquals(Sds.sectionFieldText(m, "s1", "productName", "de"), Some("Ethanol"))
    assertEquals(Sds.sectionFieldText(m, "s2", "signalWord", "fr"), Some("Danger"))
    assertEquals(Sds.sectionFieldText(m, "s2", "signalWord", "de"), Some("Gefahr"))
    assertEquals(Sds.sectionFieldText(m, "s2", "hazardPhrases", "fr"), Some("H225 ; H319"))
    assertEquals(
      Sds.sectionFieldText(m, "s2", "hazardsNotOtherwiseClassified", "de"),
      Sds.sectionFieldText(m, "s2", "hazardsNotOtherwiseClassified", "en"))
    assert(Sds.sectionFieldText(m, "s16", "otherInformation", "fr").exists(_.contains("démonstration")))
    assertEquals(Chemicals.Ethanol.thin.sections(1).fields("productName"), "Ethanol")
    assert(Chemicals.Ethanol.thin.sections(1).locales("fr").contains("productName"))
    assert(Chemicals.Ethanol.thin.sections(1).locales("de").contains("productName"))
    assertEquals(
      SectionFieldStaleness.staleLangsAfterEnChange(m, "s1", "productName", "Ethanol (lab)"),
      Set("fr", "de"))
    val projected = SectionFieldStaleness.project(m, "s1", "productName")
    assertEquals(projected("fr").state, PhraseStaleness.State.HumanReviewed)
    for (_, term) <- m.defs do
      RoundTrip.check(Sds.language.grammar, term).fold(e => fail(e), identity)

  test("SDS thin fixtures: corpus fieldLocaleRef + sectionFieldShadow override"):
    import cairn.examples.sds.Chemicals
    val base = Chemicals.Acetone.thinModule
    // signalWord FR resolves through ghsDanger corpus phrase
    assertEquals(Sds.phraseText(base, "ghsDanger", "fr"), Some("Danger"))
    assertEquals(Sds.sectionFieldText(base, "s2", "signalWord", "fr"), Some("Danger"))
    val dl = Delta.deltaOf(Sds.language).fold(e => fail(e.map(_.render).mkString), identity)
    val shadowCs = Parser.parse(dl.grammar,
      """{ add indSignal = field shadow s2 overrides signalWord with "Warning - industrial" ; }""")
      .fold(e => fail(e), identity)
    val Right((mShadow, _)) = Sds.applySds(base, shadowCs): @unchecked
    assertEquals(Sds.sectionFieldText(mShadow, "s2", "signalWord", "fr"), Some("Warning - industrial"))
    assertEquals(Sds.sectionFieldText(mShadow, "s2", "signalWord", "en"), Some("Warning - industrial"))

  test("SDS section-field shadow: overrides text; rebase conflicts on section edit"):
    import cairn.examples.sds.Chemicals
    val base = Chemicals.Acetone.thinModule
    val dl = Delta.deltaOf(Sds.language).fold(e => fail(e.map(_.render).mkString), identity)
    val shadowCs = Parser.parse(dl.grammar,
      """{ add indSignal = field shadow s2 overrides signalWord with "Warning - industrial" ; }""")
      .fold(e => fail(e), identity)
    val Right((mShadow, _)) = Sds.applySds(base, shadowCs): @unchecked
    assertEquals(
      Sds.sectionFieldText(mShadow, "s2", "signalWord", "en"),
      Some("Warning - industrial"))
    assertEquals(
      Sds.sectionFieldText(mShadow, "s2", "signalWord", "fr"),
      Some("Warning - industrial"))
    // non-shadowed field still resolves from euSection
    assert(Sds.sectionFieldText(mShadow, "s2", "hazardPhrases", "en").exists(_.nonEmpty))
    val badRef = Parser.parse(dl.grammar,
      """{ add bad = field shadow phantom overrides signalWord with "x" ; }""")
      .fold(e => fail(e), identity)
    assert(Sds.applySds(base, badRef).swap.exists(_.contains("unknown section 'phantom'")))
    // rebase: base replaces shadowed section → semantic conflict
    val baseEdit = Parser.parse(dl.grammar,
      """{ replace s2 = eu section 2 fields ( signalWord lang en : "Caution" ) ; }""")
      .fold(e => fail(e), identity)
    assert(Sds.rebaseShadow(base, baseEdit, shadowCs).isLeft)
    // disjoint: base edits a non-shadowed section → merge ok
    val disjoint = Parser.parse(dl.grammar,
      """{ replace s1 = eu section 1 fields ( productName lang en : "Acetone (lab)" ) ; }""")
      .fold(e => fail(e), identity)
    val Right((merged, _)) = Sds.rebaseShadow(base, disjoint, shadowCs): @unchecked
    assertEquals(
      Sds.sectionFieldText(merged, "s2", "signalWord", "en"),
      Some("Warning - industrial"))
    RoundTrip.check(Sds.language.grammar,
      merged.get("indSignal").getOrElse(fail("missing indSignal")))
      .fold(e => fail(e), identity)

  test("EU-CLP profile language + annex-II module + sectionNumberOk judgment"):
    import cairn.examples.sds.{EuClp, SectionNumbering}
    assert(EuClp.language.constructors.contains("sectionDef"))
    assert(EuClp.language.judgments.contains("sectionNumberOk"))
    assert(EuClp.language.judgments.contains("sectionTitleOk"))
    assert(EuClp.language.judgments.contains("profileVersionOk"))
    assertEquals(EuClp.annexIiSections.map(_._1), (1 to 16).toList)
    assertEquals(EuClp.annexIiSections.map(_._2), SectionNumbering.euClpFallback.map(_.title))
    assert(EuClp.checkSectionNumber("1"))
    assert(EuClp.checkSectionNumber("16"))
    assert(!EuClp.checkSectionNumber("17"))
    assert(EuClp.checkSectionTitle("1", "Identification"))
    assert(EuClp.checkProfileVersion("1"))
    assert(!EuClp.checkProfileVersion("9"))
    assertEquals(SectionNumbering.euClp.map(_.title), SectionNumbering.euClpFallback.map(_.title))

  test("SDS chemical instances load from languages/sds/chemicals/*.cairn"):
    import cairn.examples.sds.{ChemicalSource, Chemicals, SectionReport}
    val m = Chemicals.Acetone.thinModule
    Sds.validate(m).fold(e => fail(e), identity)
    assert(m.get("acetoneOutline").isDefined)
    val fromDisk = ChemicalSource.acetoneThin(Sds.language).fold(e => fail(e), identity)
    assertEquals(fromDisk.digest, m.digest)
    assertEquals(fromDisk.digest, Chemicals.Acetone.thin.toTypedModule("acetoneOutline").digest)
    val report = SectionReport.render(m, "acetoneOutline").fold(e => fail(e), identity)
    assert(report.startsWith("SDS REPORT: \"Acetone\""))
    assert(report.contains("section 1 \"Identification\""))
    assertEquals(SectionReport.language.name, "sds-report")

  test("SDS typed identification/hazards parse + multilingual fallback"):
    val g = Sds.language.grammar
    val id = Parser.parse(g,
      """identification section (
        |  productName "Acetone" synonyms "2-Propanone" recommendedUse "solvent"
        |  usesAdvisedAgainst "food" supplierName "Demo" emergencyPhone "+00"
        |  locales ( productName lang fr : "Acétone" )
        |)""".stripMargin).fold(e => fail(e), identity)
    RoundTrip.check(g, id).fold(e => fail(e), identity)
    assertEquals(Sds.sectionFieldText(id, "productName", "fr"), Some("Acétone"))
    assertEquals(Sds.sectionFieldText(id, "synonyms", "fr"), Some("2-Propanone"))
    val hz = Parser.parse(g,
      """hazards section (
        |  classificationSummary "Flam. Liq. 2" hazardsNotOtherwiseClassified "dryness"
        |  hazardPhrases "H225" signalWord "Danger" pictograms "GHS02"
        |  locales ( )
        |)""".stripMargin).fold(e => fail(e), identity)
    RoundTrip.check(g, hz).fold(e => fail(e), identity)
    assertEquals(Sds.sectionNumber(id), Some(1))
    assertEquals(Sds.sectionNumber(hz), Some(2))

  test("sds-report projection surfaces (json/xml/csv/pdf) — not SDS language"):
    import cairn.examples.sds.{Chemicals, SectionReport}
    // Language digests identical across surfaces; surface digests differ.
    val d = packs.requireClosed("sds-report")
    val j = packs.requireClosed("sds-report", "json")
    val x = packs.requireClosed("sds-report", "xml")
    val c = packs.requireClosed("sds-report", "csv")
    val p = packs.requireClosed("sds-report", "pdf")
    assertEquals(d.digest, j.digest)
    assertEquals(d.digest, x.digest)
    assertEquals(d.digest, c.digest)
    assertEquals(d.digest, p.digest)
    assert(packs.requireSurface("sds-report", "default").digest !=
      packs.requireSurface("sds-report", "json").digest)
    assert(packs.requireSurface("sds-report", "json").digest !=
      packs.requireSurface("sds-report", "xml").digest)
    assert(packs.requireSurface("sds-report", "xml").digest !=
      packs.requireSurface("sds-report", "csv").digest)
    assert(packs.requireSurface("sds-report", "csv").digest !=
      packs.requireSurface("sds-report", "pdf").digest)
    // SDS object language has no report-format constructors
    assert(!packs.requireClosed("sds").constructors.contains("report"))
    assert(!packs.requireClosed("sds").constructors.contains("fieldLine"))
    val json = SectionReport.renderJson(Chemicals.Acetone.pure).fold(e => fail(e), identity)
    assert(json.startsWith("{name:\"Acetone\",cas:\"67-64-1\",sections:["))
    assert(json.contains("number:1,title:\"Identification\""))
    assert(json.contains("productName:\"Acetone\""))
    assert(!json.contains("\"node\""))
    val thinJson = SectionReport.renderJson(Chemicals.Acetone.thinModule, "acetoneOutline")
      .fold(e => fail(e), identity)
    assert(thinJson.contains("number:1,title:\"Identification\""))
    assert(thinJson.contains("number:2,title:\"Hazards identification\""))
    val xml = SectionReport.renderXml(Chemicals.Acetone.thinModule, "acetoneOutline")
      .fold(e => fail(e), identity)
    assert(xml.startsWith("<sdsReport name=\"Acetone\" cas=\"67-64-1\">"))
    assert(xml.contains("<section number=1 title=\"Identification\">"))
    assert(xml.contains("<field key=productName>\"Acetone\"</field>"))
    val csv = SectionReport.renderCsv(Chemicals.Acetone.thinModule, "acetoneOutline")
      .fold(e => fail(e), identity)
    assert(csv.startsWith("SDS,CSV,\"Acetone\",\"67-64-1\""))
    assert(csv.contains("section,1,\"Identification\""))
    assert(csv.contains("field,productName,\"Acetone\""))
    val pdfText = SectionReport.renderPdfSurface(Chemicals.Acetone.thinModule, "acetoneOutline")
      .fold(e => fail(e), identity)
    assert(pdfText.startsWith("PDF report:"), pdfText)
    assert(pdfText.contains("Acetone") && pdfText.contains("67-64-1"), pdfText)
    assert(pdfText.contains("section 1"), pdfText)
    assert(pdfText.contains("Identification"), pdfText)
    val pdfBytes = SectionReport.renderPdf(Chemicals.Acetone.thinModule, "acetoneOutline")
      .fold(e => fail(e), identity)
    assert(cairn.core.PdfMinimal.isPdf(pdfBytes), new String(pdfBytes.take(32)))
    assert(new String(pdfBytes, java.nio.charset.StandardCharsets.ISO_8859_1).contains("Acetone"))

  test("SDS sectionFieldRef resolves corpus phrases + shadow override"):
    val src =
      """h225 = corpus phrase h225 lang en text "Highly flammable liquid and vapour" ;
        |h225fr = corpus phrase h225 lang fr text "Liquide et vapeurs extremement inflammables" ;
        |s2 = eu section 2 fields ( hazardPhrases lang en ref h225 , hazardPhrases lang fr ref h225 , signalWord lang en : "Danger" ) ;
        |""".stripMargin
    val m = ModuleSurface.toModule(
      Parser.parse(ModuleSurface.grammar(Sds.language), src).fold(e => fail(e), identity))
      .fold(e => fail(e), _.sorted)
    Sds.validate(m).fold(e => fail(e), identity)
    assertEquals(
      Sds.sectionFieldText(m, "s2", "hazardPhrases", "en"),
      Some("Highly flammable liquid and vapour"))
    assertEquals(
      Sds.sectionFieldText(m, "s2", "hazardPhrases", "fr"),
      Some("Liquide et vapeurs extremement inflammables"))
    val dl = Delta.deltaOf(Sds.language).fold(e => fail(e.map(_.render).mkString), identity)
    val shadow = Parser.parse(dl.grammar,
      """{ add ov = field shadow s2 overrides hazardPhrases with "Industrial H225 wording" ; }""")
      .fold(e => fail(e), identity)
    val Right((shadowed, _)) = Sds.applySds(m, shadow): @unchecked
    assertEquals(
      Sds.sectionFieldText(shadowed, "s2", "hazardPhrases", "fr"),
      Some("Industrial H225 wording"))

  test("SDS section-field staleness: derived ΔL materializes sectionFieldState"):
    import cairn.examples.sds.{Chemicals, SectionFieldStaleness, PhraseStaleness}
    val base = Chemicals.Acetone.thinModule
    // Free-text FR sibling (productName) — not corpus-ref signalWord
    val oldHash = PhraseStaleness.textHash(
      Sds.sectionFieldText(base, "s1", "productName", "en").get)
    val Right((m2, _)) = SectionFieldStaleness.applyEnRewrite(
      Sds.applySds, Sds.language, base, "s1", "productName", "Acetone (lab)"): @unchecked
    val mark = SectionFieldStaleness.markName("s1", "productName", "fr")
    assert(m2.get(mark).exists {
      case Cst.Node("sectionFieldState", List(
          Cst.Leaf("s1"), Cst.Leaf("productName"), Cst.Leaf("fr"), Cst.Leaf(h),
          Cst.Leaf("staleBecauseSourceChanged"))) =>
        h == oldHash.hex
      case _ => false
    })
    assertEquals(
      SectionFieldStaleness.project(m2, "s1", "productName")("fr").state,
      PhraseStaleness.State.StaleBecauseSourceChanged)
    assertEquals(Sds.sectionFieldText(m2, "s1", "productName", "en"), Some("Acetone (lab)"))

  test("EU-CLP profile conformance over SDS module (not numbering alone)"):
    import cairn.examples.sds.{Chemicals, EuClp}
    val ok = EuClp.conform(Chemicals.Acetone.thinModule)
    assert(ok.ok, ok.errors.mkString("; "))
    assertEquals(ok.profileVersion, "1")
    assert(ok.sectionNumbers.contains(1) && ok.sectionNumbers.contains(14))
    val full = EuClp.conform(Chemicals.Acetone.asModule)
    assert(full.ok, full.errors.mkString("; "))
    assertEquals(full.sectionNumbers, (1 to 16).toList)

  test("SDS workflow + certificate packs: language-checked sequence/evidence; PackLoader load"):
    import cairn.examples.sds.{
      ChemicalSource, Chemicals, EuClp, SectionReport, SdsCertificateKinds, SdsWorkflow}
    // Workflow script + cert kinds are ordinary Cairn modules (no Scala recompile).
    assert(SdsWorkflow.language.judgments.contains("workflowStepOk"))
    assert(SdsWorkflow.language.judgments.contains("workflowPhaseOk"))
    assertEquals(
      SdsWorkflow.causalStepNames,
      List("author", "shadow", "rebase", "conflict", "approve", "sign", "publish"))
    assert(SdsWorkflow.checkStep("author"))
    assert(!SdsWorkflow.checkStep("notAStep"))
    assert(SdsCertificateKinds.language.judgments.contains("certificateKindOk"))
    assertEquals(
      SdsCertificateKinds.workflowKinds,
      List("sds-approval", "sds-tip-signature", "sds-publication"))
    assert(SdsCertificateKinds.checkKind("sds-approval"))
    assert(!SdsCertificateKinds.checkKind("sds-bogus"))
    // Verified application spine: chemical + EU-CLP conform + report surface print.
    val chem = Chemicals.Acetone.thinModule
    Sds.validate(chem).fold(e => fail(e), identity)
    assert(Sds.checkSectionNumber("1"))
    assert(!Sds.checkSectionNumber("17"))
    val conf = EuClp.conform(chem)
    assert(conf.ok, conf.errors.mkString("; "))
    val report = SectionReport.printSurface(
      "json",
      SectionReport.toCst(chem, "acetoneOutline").fold(e => fail(e), identity))
      .fold(e => fail(e), identity)
    assert(report.contains("number:1,title:\"Identification\""))
    // Digests stable across reload (content-addressed distribution).
    val wfReload = ChemicalSource.loadModule(
      SdsWorkflow.language, java.nio.file.Path.of("languages/sds-workflow/causal.cairn"))
      .fold(e => fail(e), identity)
    assertEquals(wfReload.digest, SdsWorkflow.causalModule.digest)
    val certReload = ChemicalSource.loadModule(
      SdsCertificateKinds.language,
      java.nio.file.Path.of("languages/sds-certificate/workflow-kinds.cairn"))
      .fold(e => fail(e), identity)
    assertEquals(certReload.digest, SdsCertificateKinds.workflowKindsModule.digest)

  test("effect bootstrap: interface lang + all packs; ActionKey.fromPinned; host seed fixpoint"):
    val loaded = cairn.runtime.EffectBootstrap.load(packs).fold(e => fail(e), identity)
    assertEquals(loaded.families.size, Effects.Family.values.length)
    assertEquals(loaded.pinned.size, Effects.Family.values.length)
    // Opaque keys from disk pins; Family thin bridge from packs.
    for name <- EffectMeta.fragmentPackNames do
      val fam = Effects.Family.forPack(name).getOrElse(fail(s"no Family.forPack($name)"))
      val pinned = loaded.pinned(fam)
      val key = loaded.actionKey(fam, pinned.actions.head).fold(e => fail(e), identity)
      assert(key.interfaceDigest.isDefined, name)
      assertEquals(key.family, fam.toString)
    // Host cold-start seeds still complete (handlers use them).
    EffectMeta.families.values.foreach(f => assertEquals(EffectMeta.completeness(f), Nil, f.fragment.name))

  test("ReplayReplication: want/have + revocation absorb + checkGrant (merge, not BFT)"):
    import cairn.systemhandler.{MemCas, ReplayReplication, ReplayStore, RevocationLog}
    val cas = MemCas()
    val ctx = EffectContext.forCas()
    val a = ReplayStore.memory()
    val b = ReplayStore.memory()
    a.consumeNonce("alice", "n1").fold(e => fail(e), identity)
    val snap = ReplayStore.publish(a, cas, ctx).fold(e => fail(e.toString), identity)
    val rev = RevocationLog()
    val revDig = rev.publish(cas, ctx, List("grant-1")).fold(e => fail(e), identity)
    val plan = ReplayReplication.plan(
      ReplayReplication.WantHave(Set.empty, Set.empty),
      Set(snap), Set(revDig))
    assertEquals(plan.snapshotsToFetch, List(snap))
    assertEquals(plan.revocationsToFetch, List(revDig))
    val revB = RevocationLog()
    ReplayReplication.applyPlan(b, revB, cas, ctx, plan).fold(e => fail(e), identity)
    assert(b.snapshot.flatNonces.contains("n1"))
    assert(revB.isRevoked("grant-1"))
    assert(ReplayReplication.checkGrant(revB, "grant-1").isLeft)
    assert(ReplayReplication.checkGrant(revB, "grant-ok").isRight)
