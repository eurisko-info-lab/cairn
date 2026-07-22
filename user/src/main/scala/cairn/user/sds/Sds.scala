package cairn.user.sds

import cairn.kernel.*
import cairn.systeminterface.PackAccess
import cairn.core.*

/** SDS pack (M47, §5b): Safety Data Sheet authoring — flagship of
  * `PKI → Law → SDS`. An SDS is NOT a flat document: it is a compiled view
  * over typed objects (substances, mixtures, phrases / corpus phrases,
  * products, shadows, regulatory `basis` citations into Law sections,
  * EU-CLP `euSection` / typed sections 1–16
  * `outline` / multilingual `sectionField` maps).
  *
  * Object language: [[languages/sds.cairn]] (`provides sds requires law`).
  * Closed composition pulls Law + PKI; compose without them fails.
  * ΔSDS = generic ΔL + domain validation. Scala = host glue only.
  * Phrase + section-field staleness: `translationState` / `sectionFieldState`
  * + `translationStateTag` judgment; host derive/apply emit real ΔSDS
  * changesets. Regulatory conformance: ΔSDS domain gate calls `eu-clp`
  * `sectionNumberOk` and SDS `translationStateTag` judgments (not host-only
  * Sets); full-module [[EuClp.conform]] remains a thin host walk over those
  * facts. Chemical instances load from `languages/sds/chemicals/` `.cairn`
  * files (`ChemicalSource`); host maps remain emit fixtures. Report encodings
  * (JSON/XML/CSV/PDF/XLS) are **not** SDS vocabulary — they live in the
  * separate `sds-report` projection pack ([[cairn.examples.sds.SectionReport]])
  * that *consumes* SDS modules/outlines.
  * `sectionField` / `sectionFieldRef` resolve with multilingual fallback, then
  * `sectionFieldShadow` overrides. Typed sections flatten to the same
  * key/lang resolution path.
  */
final class Sds(packs: PackAccess):
  lazy val fragments: List[Fragment] = packs.requireOwn("sds")

  /** Own fragment only — compose fails: `requires law` unmet without Law/PKI. */
  def ownCompose: Either[List[ComposeError], ComposedLanguage] =
    Compose.compose("sds", fragments)

  /** Closed language: SDS + demoted Law + demoted PKI. */
  lazy val language: ComposedLanguage = packs.requireClosed("sds")

  /** EU-CLP regulatory judgments (`sectionNumberOk`) — disk SoT via PackLoader. */
  private lazy val euClpLanguage: ComposedLanguage = packs.requireClosed("eu-clp")

  private val identificationKeys: Set[String] = Set(
    "productName", "synonyms", "recommendedUse", "usesAdvisedAgainst",
    "supplierName", "emergencyPhone")
  private val hazardsKeys: Set[String] = Set(
    "classificationSummary", "hazardsNotOtherwiseClassified", "hazardPhrases",
    "signalWord", "pictograms")
  private val compositionKeys: Set[String] = Set(
    "componentName", "cas", "ec", "concentration")
  private val firstAidKeys: Set[String] = Set(
    "generalAdvice", "inhalation", "skinContact", "eyeContact", "ingestion")
  private val firefightingKeys: Set[String] = Set(
    "extinguishingMedia", "unsuitableExtinguishingMedia", "specialHazards",
    "firefighterProtection")
  private val accidentalReleaseKeys: Set[String] = Set(
    "personalPrecautions", "environmentalPrecautions", "cleanupMethods")
  private val handlingStorageKeys: Set[String] = Set(
    "handling", "storage", "storageIncompatibilities")
  private val exposureControlsKeys: Set[String] = Set(
    "occupationalExposureLimit", "engineeringControls", "eyeProtection",
    "skinProtection", "respiratoryProtection")
  private val physicalChemicalKeys: Set[String] = Set(
    "appearance", "odor", "molecularWeight", "meltingPoint", "boilingPoint",
    "flashPoint", "density", "solubility", "explosiveLimits")
  private val stabilityReactivityKeys: Set[String] = Set(
    "stability", "conditionsToAvoid", "incompatibleMaterials",
    "hazardousDecomposition")
  private val toxicologicalKeys: Set[String] = Set(
    "ld50Oral", "irritation", "inhalationEffects", "carcinogenicity")
  private val ecologicalKeys: Set[String] = Set(
    "ecotoxicity", "persistence", "bioaccumulation", "mobility")
  private val disposalKeys: Set[String] = Set(
    "disposalMethods", "wasteClassification")
  private val transportKeys: Set[String] = Set(
    "unNumber", "properShippingName", "transportHazardClass", "packingGroup")
  private val regulatoryKeys: Set[String] = Set(
    "regulatoryInfo", "reachStatus", "usInventory")
  private val otherInformationKeys: Set[String] = Set(
    "revisionDate", "otherInformation")

  val typedSectionTags: Set[String] = Set(
    "identificationSection", "hazardsSection", "compositionSection",
    "firstAidSection", "firefightingSection", "accidentalReleaseSection",
    "handlingStorageSection", "exposureControlsSection",
    "physicalChemicalSection", "stabilityReactivitySection",
    "toxicologicalSection", "ecologicalSection", "disposalSection",
    "transportSection", "regulatorySection", "otherInformationSection")

  /** Ordered EN slot keys per typed section tag (for report / rewrite). */
  val typedSectionKeys: Map[String, List[String]] = Map(
    "identificationSection" -> List(
      "productName", "synonyms", "recommendedUse", "usesAdvisedAgainst",
      "supplierName", "emergencyPhone"),
    "hazardsSection" -> List(
      "classificationSummary", "hazardsNotOtherwiseClassified", "hazardPhrases",
      "signalWord", "pictograms"),
    "compositionSection" -> List("componentName", "cas", "ec", "concentration"),
    "firstAidSection" -> List(
      "generalAdvice", "inhalation", "skinContact", "eyeContact", "ingestion"),
    "firefightingSection" -> List(
      "extinguishingMedia", "unsuitableExtinguishingMedia", "specialHazards",
      "firefighterProtection"),
    "accidentalReleaseSection" -> List(
      "personalPrecautions", "environmentalPrecautions", "cleanupMethods"),
    "handlingStorageSection" -> List(
      "handling", "storage", "storageIncompatibilities"),
    "exposureControlsSection" -> List(
      "occupationalExposureLimit", "engineeringControls", "eyeProtection",
      "skinProtection", "respiratoryProtection"),
    "physicalChemicalSection" -> List(
      "appearance", "odor", "molecularWeight", "meltingPoint", "boilingPoint",
      "flashPoint", "density", "solubility", "explosiveLimits"),
    "stabilityReactivitySection" -> List(
      "stability", "conditionsToAvoid", "incompatibleMaterials",
      "hazardousDecomposition"),
    "toxicologicalSection" -> List(
      "ld50Oral", "irritation", "inhalationEffects", "carcinogenicity"),
    "ecologicalSection" -> List(
      "ecotoxicity", "persistence", "bioaccumulation", "mobility"),
    "disposalSection" -> List("disposalMethods", "wasteClassification"),
    "transportSection" -> List(
      "unNumber", "properShippingName", "transportHazardClass", "packingGroup"),
    "regulatorySection" -> List("regulatoryInfo", "reachStatus", "usInventory"),
    "otherInformationSection" -> List("revisionDate", "otherInformation"))

  /** Kernel-check `sectionNumberOk` (eu-clp pack) — ΔSDS domain gate uses this,
    * not a host-only 1..16 Set.
    */
  def checkSectionNumber(n: String): Boolean =
    val cfg = CheckerCfg(euClpLanguage.judgments.values.toList)
    val goal = Cst.node("sectionNumberOk", Cst.Leaf(n))
    Search.infer(cfg, goal).flatMap(d => Checker.check(cfg, d).left.map(_.render)).isRight

  /** EU-CLP number implied by a section body ctor. */
  def sectionNumber(sec: Cst): Option[Int] = sec match
    case Cst.Node("euSection", List(Cst.Leaf(num), _)) => num.toIntOption
    case Cst.Node("identificationSection", _) => Some(1)
    case Cst.Node("hazardsSection", _) => Some(2)
    case Cst.Node("compositionSection", _) => Some(3)
    case Cst.Node("firstAidSection", _) => Some(4)
    case Cst.Node("firefightingSection", _) => Some(5)
    case Cst.Node("accidentalReleaseSection", _) => Some(6)
    case Cst.Node("handlingStorageSection", _) => Some(7)
    case Cst.Node("exposureControlsSection", _) => Some(8)
    case Cst.Node("physicalChemicalSection", _) => Some(9)
    case Cst.Node("stabilityReactivitySection", _) => Some(10)
    case Cst.Node("toxicologicalSection", _) => Some(11)
    case Cst.Node("ecologicalSection", _) => Some(12)
    case Cst.Node("disposalSection", _) => Some(13)
    case Cst.Node("transportSection", _) => Some(14)
    case Cst.Node("regulatorySection", _) => Some(15)
    case Cst.Node("otherInformationSection", _) => Some(16)
    case _ => None

  private def isSectionBody(term: Cst): Boolean = term match
    case Cst.Node(tag, _) if tag == "euSection" || typedSectionTags.contains(tag) => true
    case _ => false

  private def localeRows(overlays: Cst): List[Cst] = overlays match
    case Cst.Node("none", _) => Nil
    case Cst.Node("some", List(Cst.Node("list", xs))) => xs
    case Cst.Node("list", xs) => xs
    case _ => Nil

  // ---- domain validation (ΔSDS = generic ΔL + these checks) ----

  def validate(m: Module): Either[String, Unit] =
    val errs = List.newBuilder[String]
    val seenTranslationStates = scala.collection.mutable.HashSet.empty[(String, String)]
    val seenFieldStates = scala.collection.mutable.HashSet.empty[(String, String, String)]
    def defined(n: String) = m.get(n).isDefined
    def requireNonEmpty(ctor: String, name: String, label: String, v: String): Unit =
      if v.isEmpty then errs += s"$ctor '$name': empty $label"
    def validateLocales(
        name: String,
        ctor: String,
        overlays: Cst,
        allowed: Set[String]
    ): Unit =
      val seen = scala.collection.mutable.HashSet.empty[(String, String)]
      for row <- localeRows(overlays) do row match
        case Cst.Node("fieldLocale", List(Cst.Leaf(k), Cst.Leaf(lang), Cst.Leaf(_))) =>
          if k.isEmpty then errs += s"$ctor '$name': empty locale key"
          else if !allowed.contains(k) then
            errs += s"$ctor '$name': locale key '$k' not in typed slots"
          else if lang.isEmpty then errs += s"$ctor '$name' locale '$k': empty lang"
          else if !seen.add((k, lang)) then
            errs += s"$ctor '$name' duplicate locale '$k' lang '$lang'"
        case Cst.Node("fieldLocaleRef", List(Cst.Leaf(k), Cst.Leaf(lang), Cst.Leaf(ref))) =>
          if k.isEmpty then errs += s"$ctor '$name': empty locale key"
          else if !allowed.contains(k) then
            errs += s"$ctor '$name': locale key '$k' not in typed slots"
          else if lang.isEmpty then errs += s"$ctor '$name' locale '$k': empty lang"
          else if ref.isEmpty then errs += s"$ctor '$name' locale '$k': empty phrase ref"
          else if !defined(ref) then
            errs += s"$ctor '$name' locale '$k' references unknown phrase '$ref'"
          else if !seen.add((k, lang)) then
            errs += s"$ctor '$name' duplicate locale '$k' lang '$lang'"
        case other => errs += s"$ctor '$name': bad locale ${other.render}"
    for (name, term) <- m.defs do term match
      case Cst.Node("mixture", List(Cst.Node("list", comps))) =>
        var total = 0L
        for c <- comps do c match
          case Cst.Node("component", List(Cst.Leaf(ref), Cst.Leaf(pct))) =>
            total += pct.toLong
            if !defined(ref) then errs += s"mixture '$name' references unknown substance '$ref'"
          case other => errs += s"mixture '$name': bad component ${other.render}"
        if total > 100 then errs += s"mixture '$name' percentages sum to $total > 100"
      case Cst.Node("product", List(_, Cst.Leaf(mix), Cst.Node("list", phraseRefs))) =>
        if !defined(mix) then errs += s"product '$name' references unknown mixture '$mix'"
        for p <- phraseRefs do p match
          case Cst.Leaf(pr) if !defined(pr) => errs += s"product '$name' references unknown phrase '$pr'"
          case _ => ()
      case Cst.Node("shadow", List(Cst.Leaf(prod), Cst.Leaf(phrase), _)) =>
        if !defined(prod) then errs += s"shadow '$name' references unknown product '$prod'"
        if !defined(phrase) then errs += s"shadow '$name' references unknown phrase '$phrase'"
      case Cst.Node("sectionFieldShadow", List(Cst.Leaf(sec), Cst.Leaf(key), _)) =>
        if key.isEmpty then errs += s"sectionFieldShadow '$name': empty field key"
        m.get(sec) match
          case Some(t) if isSectionBody(t) => ()
          case Some(_) =>
            errs += s"sectionFieldShadow '$name' references '$sec' which is not a section body"
          case None =>
            errs += s"sectionFieldShadow '$name' references unknown section '$sec'"
      case Cst.Node("translationState", List(Cst.Leaf(phrase), Cst.Leaf(lang), Cst.Leaf(hash), Cst.Leaf(tag))) =>
        if phrase.isEmpty then errs += s"translationState '$name': empty phrase ref"
        else if !m.defs.exists {
            case (_, Cst.Node("phrase" | "corpusPhrase", List(Cst.Leaf(n), _, _))) => n == phrase
            case _ => false
          } then errs += s"translationState '$name' references unknown phrase '$phrase'"
        if lang.isEmpty then errs += s"translationState '$name': empty lang"
        if hash.isEmpty then errs += s"translationState '$name': empty from-hash"
        if !checkTranslationStateTag(tag) then
          errs += s"translationState '$name': unknown state tag '$tag'"
        else if phrase.nonEmpty && lang.nonEmpty && !seenTranslationStates.add((phrase, lang)) then
          errs += s"translationState '$name' duplicate mark for '$phrase' lang '$lang'"
      case Cst.Node("sectionFieldState", List(
          Cst.Leaf(sec), Cst.Leaf(key), Cst.Leaf(lang), Cst.Leaf(hash), Cst.Leaf(tag))) =>
        if sec.isEmpty then errs += s"sectionFieldState '$name': empty section ref"
        else m.get(sec) match
          case Some(t) if isSectionBody(t) => ()
          case Some(_) =>
            errs += s"sectionFieldState '$name' references '$sec' which is not a section body"
          case None =>
            errs += s"sectionFieldState '$name' references unknown section '$sec'"
        if key.isEmpty then errs += s"sectionFieldState '$name': empty field key"
        if lang.isEmpty then errs += s"sectionFieldState '$name': empty lang"
        if hash.isEmpty then errs += s"sectionFieldState '$name': empty from-hash"
        if !checkTranslationStateTag(tag) then
          errs += s"sectionFieldState '$name': unknown state tag '$tag'"
        else if sec.nonEmpty && key.nonEmpty && lang.nonEmpty &&
            !seenFieldStates.add((sec, key, lang)) then
          errs += s"sectionFieldState '$name' duplicate mark for '$sec'.$key lang '$lang'"
      case Cst.Node("basis", List(Cst.Leaf(target), Cst.Leaf(section))) =>
        if !defined(target) then errs += s"basis '$name' references unknown product '$target'"
        if section.isEmpty then errs += s"basis '$name' missing Law section number"
      case Cst.Node("euSection", List(Cst.Leaf(num), Cst.Node("list", fields))) =>
        num.toIntOption match
          case Some(n) if checkSectionNumber(n.toString) => ()
          case Some(n) => errs += s"euSection '$name' number $n fails sectionNumberOk"
          case None => errs += s"euSection '$name' number '$num' is not an integer"
        val seen = scala.collection.mutable.HashSet.empty[(String, String)]
        for f <- fields do f match
          case Cst.Node("sectionField", List(Cst.Leaf(k), Cst.Leaf(lang), Cst.Leaf(_))) =>
            if k.isEmpty then errs += s"euSection '$name': empty field key"
            else if lang.isEmpty then errs += s"euSection '$name' field '$k': empty lang"
            else if !seen.add((k, lang)) then
              errs += s"euSection '$name' duplicate field '$k' lang '$lang'"
          case Cst.Node("sectionFieldRef", List(Cst.Leaf(k), Cst.Leaf(lang), Cst.Leaf(ref))) =>
            if k.isEmpty then errs += s"euSection '$name': empty field key"
            else if lang.isEmpty then errs += s"euSection '$name' field '$k': empty lang"
            else if ref.isEmpty then errs += s"euSection '$name' field '$k': empty phrase ref"
            else if !defined(ref) then
              errs += s"euSection '$name' field '$k' references unknown phrase '$ref'"
            else if !seen.add((k, lang)) then
              errs += s"euSection '$name' duplicate field '$k' lang '$lang'"
          case other => errs += s"euSection '$name': bad field ${other.render}"
      case Cst.Node("identificationSection", List(
          Cst.Leaf(pn), Cst.Leaf(syn), Cst.Leaf(use), Cst.Leaf(against),
          Cst.Leaf(supplier), Cst.Leaf(phone), overlays)) =>
        requireNonEmpty("identificationSection", name, "productName", pn)
        requireNonEmpty("identificationSection", name, "synonyms", syn)
        requireNonEmpty("identificationSection", name, "recommendedUse", use)
        requireNonEmpty("identificationSection", name, "usesAdvisedAgainst", against)
        requireNonEmpty("identificationSection", name, "supplierName", supplier)
        requireNonEmpty("identificationSection", name, "emergencyPhone", phone)
        validateLocales(name, "identificationSection", overlays, identificationKeys)
      case Cst.Node("hazardsSection", List(
          Cst.Leaf(cls), Cst.Leaf(hnoc), Cst.Leaf(phrases), Cst.Leaf(signal),
          Cst.Leaf(pictos), overlays)) =>
        requireNonEmpty("hazardsSection", name, "classificationSummary", cls)
        requireNonEmpty("hazardsSection", name, "hazardsNotOtherwiseClassified", hnoc)
        requireNonEmpty("hazardsSection", name, "hazardPhrases", phrases)
        requireNonEmpty("hazardsSection", name, "signalWord", signal)
        requireNonEmpty("hazardsSection", name, "pictograms", pictos)
        validateLocales(name, "hazardsSection", overlays, hazardsKeys)
      case Cst.Node("compositionSection", List(
          Cst.Leaf(comp), Cst.Leaf(cas), Cst.Leaf(ec), Cst.Leaf(conc), overlays)) =>
        requireNonEmpty("compositionSection", name, "componentName", comp)
        requireNonEmpty("compositionSection", name, "cas", cas)
        requireNonEmpty("compositionSection", name, "ec", ec)
        requireNonEmpty("compositionSection", name, "concentration", conc)
        validateLocales(name, "compositionSection", overlays, compositionKeys)
      case Cst.Node("firstAidSection", List(
          Cst.Leaf(ga), Cst.Leaf(inh), Cst.Leaf(skin), Cst.Leaf(eye),
          Cst.Leaf(ing), overlays)) =>
        requireNonEmpty("firstAidSection", name, "generalAdvice", ga)
        requireNonEmpty("firstAidSection", name, "inhalation", inh)
        requireNonEmpty("firstAidSection", name, "skinContact", skin)
        requireNonEmpty("firstAidSection", name, "eyeContact", eye)
        requireNonEmpty("firstAidSection", name, "ingestion", ing)
        validateLocales(name, "firstAidSection", overlays, firstAidKeys)
      case Cst.Node("firefightingSection", List(
          Cst.Leaf(em), Cst.Leaf(uem), Cst.Leaf(sh), Cst.Leaf(fp), overlays)) =>
        requireNonEmpty("firefightingSection", name, "extinguishingMedia", em)
        requireNonEmpty("firefightingSection", name, "unsuitableExtinguishingMedia", uem)
        requireNonEmpty("firefightingSection", name, "specialHazards", sh)
        requireNonEmpty("firefightingSection", name, "firefighterProtection", fp)
        validateLocales(name, "firefightingSection", overlays, firefightingKeys)
      case Cst.Node("accidentalReleaseSection", List(
          Cst.Leaf(pp), Cst.Leaf(ep), Cst.Leaf(cm), overlays)) =>
        requireNonEmpty("accidentalReleaseSection", name, "personalPrecautions", pp)
        requireNonEmpty("accidentalReleaseSection", name, "environmentalPrecautions", ep)
        requireNonEmpty("accidentalReleaseSection", name, "cleanupMethods", cm)
        validateLocales(name, "accidentalReleaseSection", overlays, accidentalReleaseKeys)
      case Cst.Node("handlingStorageSection", List(
          Cst.Leaf(h), Cst.Leaf(st), Cst.Leaf(si), overlays)) =>
        requireNonEmpty("handlingStorageSection", name, "handling", h)
        requireNonEmpty("handlingStorageSection", name, "storage", st)
        requireNonEmpty("handlingStorageSection", name, "storageIncompatibilities", si)
        validateLocales(name, "handlingStorageSection", overlays, handlingStorageKeys)
      case Cst.Node("exposureControlsSection", List(
          Cst.Leaf(oel), Cst.Leaf(ec), Cst.Leaf(eye), Cst.Leaf(skin),
          Cst.Leaf(resp), overlays)) =>
        requireNonEmpty("exposureControlsSection", name, "occupationalExposureLimit", oel)
        requireNonEmpty("exposureControlsSection", name, "engineeringControls", ec)
        requireNonEmpty("exposureControlsSection", name, "eyeProtection", eye)
        requireNonEmpty("exposureControlsSection", name, "skinProtection", skin)
        requireNonEmpty("exposureControlsSection", name, "respiratoryProtection", resp)
        validateLocales(name, "exposureControlsSection", overlays, exposureControlsKeys)
      case Cst.Node("physicalChemicalSection", List(
          Cst.Leaf(app), Cst.Leaf(odor), Cst.Leaf(mw), Cst.Leaf(mp), Cst.Leaf(bp),
          Cst.Leaf(fp), Cst.Leaf(dens), Cst.Leaf(sol), Cst.Leaf(expl), overlays)) =>
        requireNonEmpty("physicalChemicalSection", name, "appearance", app)
        requireNonEmpty("physicalChemicalSection", name, "odor", odor)
        requireNonEmpty("physicalChemicalSection", name, "molecularWeight", mw)
        requireNonEmpty("physicalChemicalSection", name, "meltingPoint", mp)
        requireNonEmpty("physicalChemicalSection", name, "boilingPoint", bp)
        requireNonEmpty("physicalChemicalSection", name, "flashPoint", fp)
        requireNonEmpty("physicalChemicalSection", name, "density", dens)
        requireNonEmpty("physicalChemicalSection", name, "solubility", sol)
        requireNonEmpty("physicalChemicalSection", name, "explosiveLimits", expl)
        validateLocales(name, "physicalChemicalSection", overlays, physicalChemicalKeys)
      case Cst.Node("stabilityReactivitySection", List(
          Cst.Leaf(st), Cst.Leaf(cta), Cst.Leaf(im), Cst.Leaf(hd), overlays)) =>
        requireNonEmpty("stabilityReactivitySection", name, "stability", st)
        requireNonEmpty("stabilityReactivitySection", name, "conditionsToAvoid", cta)
        requireNonEmpty("stabilityReactivitySection", name, "incompatibleMaterials", im)
        requireNonEmpty("stabilityReactivitySection", name, "hazardousDecomposition", hd)
        validateLocales(name, "stabilityReactivitySection", overlays, stabilityReactivityKeys)
      case Cst.Node("toxicologicalSection", List(
          Cst.Leaf(ld50), Cst.Leaf(irr), Cst.Leaf(inh), Cst.Leaf(carc), overlays)) =>
        requireNonEmpty("toxicologicalSection", name, "ld50Oral", ld50)
        requireNonEmpty("toxicologicalSection", name, "irritation", irr)
        requireNonEmpty("toxicologicalSection", name, "inhalationEffects", inh)
        requireNonEmpty("toxicologicalSection", name, "carcinogenicity", carc)
        validateLocales(name, "toxicologicalSection", overlays, toxicologicalKeys)
      case Cst.Node("ecologicalSection", List(
          Cst.Leaf(eco), Cst.Leaf(pers), Cst.Leaf(bio), Cst.Leaf(mob), overlays)) =>
        requireNonEmpty("ecologicalSection", name, "ecotoxicity", eco)
        requireNonEmpty("ecologicalSection", name, "persistence", pers)
        requireNonEmpty("ecologicalSection", name, "bioaccumulation", bio)
        requireNonEmpty("ecologicalSection", name, "mobility", mob)
        validateLocales(name, "ecologicalSection", overlays, ecologicalKeys)
      case Cst.Node("disposalSection", List(
          Cst.Leaf(dm), Cst.Leaf(wc), overlays)) =>
        requireNonEmpty("disposalSection", name, "disposalMethods", dm)
        requireNonEmpty("disposalSection", name, "wasteClassification", wc)
        validateLocales(name, "disposalSection", overlays, disposalKeys)
      case Cst.Node("transportSection", List(
          Cst.Leaf(un), Cst.Leaf(psn), Cst.Leaf(cls), Cst.Leaf(pg), overlays)) =>
        requireNonEmpty("transportSection", name, "unNumber", un)
        requireNonEmpty("transportSection", name, "properShippingName", psn)
        requireNonEmpty("transportSection", name, "transportHazardClass", cls)
        requireNonEmpty("transportSection", name, "packingGroup", pg)
        validateLocales(name, "transportSection", overlays, transportKeys)
      case Cst.Node("regulatorySection", List(
          Cst.Leaf(ri), Cst.Leaf(rs), Cst.Leaf(us), overlays)) =>
        requireNonEmpty("regulatorySection", name, "regulatoryInfo", ri)
        requireNonEmpty("regulatorySection", name, "reachStatus", rs)
        requireNonEmpty("regulatorySection", name, "usInventory", us)
        validateLocales(name, "regulatorySection", overlays, regulatoryKeys)
      case Cst.Node("otherInformationSection", List(
          Cst.Leaf(rd), Cst.Leaf(oi), overlays)) =>
        requireNonEmpty("otherInformationSection", name, "revisionDate", rd)
        requireNonEmpty("otherInformationSection", name, "otherInformation", oi)
        validateLocales(name, "otherInformationSection", overlays, otherInformationKeys)
      case Cst.Node("outline", List(_, _, sectionsField)) =>
        val refs = sectionsField match
          case Cst.Node("none", _) => Nil
          case Cst.Node("some", List(Cst.Node("list", rs))) => rs
          case Cst.Node("list", rs) => rs // tolerate non-opt shape
          case other =>
            errs += s"outline '$name': bad sections ${other.render}"
            Nil
        val nums = List.newBuilder[Int]
        for r <- refs do r match
          case Cst.Leaf(ref) =>
            m.get(ref) match
              case Some(sec) =>
                sectionNumber(sec) match
                  case Some(n) if checkSectionNumber(n.toString) => nums += n
                  case Some(n) =>
                    errs += s"outline '$name' section '$ref' number $n fails sectionNumberOk"
                  case None =>
                    errs += s"outline '$name' references '$ref' which is not a section body"
              case None =>
                errs += s"outline '$name' references unknown section '$ref'"
          case other => errs += s"outline '$name': bad section ref ${other.render}"
        val ns = nums.result()
        if ns.distinct.sizeIs != ns.size then
          errs += s"outline '$name' has duplicate section numbers"
        if ns != ns.sorted then
          errs += s"outline '$name' section numbers not ascending: ${ns.mkString(",")}"
      case _ => ()
    val es = errs.result()
    if es.isEmpty then Right(()) else Left(es.mkString("; "))

  /** ΔSDS application: the generic ΔL, then the domain gate. */
  def applySds(m: Module, change: Cst): Either[String, (Module, Delta.ValidatedChangeSet)] =
    Delta.apply(language, m, change).flatMap { out =>
      validate(out._1).map(_ => out) }

  /** Kernel-check a `translationStateTag` judgment goal (parallel to EU-CLP
    * `sectionNumberOk`).
    */
  def checkTranslationStateTag(tag: String): Boolean =
    val cfg = CheckerCfg(language.judgments.values.toList)
    val goal = Cst.node("translationStateTag", Cst.Leaf(tag))
    Search.infer(cfg, goal).flatMap(d => Checker.check(cfg, d).left.map(_.render)).isRight

  /** Phrase / product / section names a shadow change-set overrides.
    * Domain-aware footprint for GRANITE-style shadow rebase (base edit of an
    * overridden phrase/product/section is a semantic conflict, even when ΔL
    * names differ). Includes both phrase `shadow` and `sectionFieldShadow`.
    */
  def shadowOverrideTargets(change: Cst): Set[String] =
    def items(c: Cst): List[Cst] = c match
      case Cst.Node(_, List(Cst.Node("list", xs))) => xs
      case Cst.Node(_, xs)                         => xs
      case _                                       => List(c)
    items(change).flatMap {
      case Cst.Node(_, List(_, term)) => term match
        case Cst.Node("shadow", List(Cst.Leaf(prod), Cst.Leaf(phrase), _)) =>
          List(prod, phrase)
        case Cst.Node("sectionFieldShadow", List(Cst.Leaf(sec), _, _)) =>
          List(sec)
        case _ => Nil
      case _ => Nil
    }.toSet

  /** Rebase an industrial shadow over a base revision. Disjoint overrides
    * merge; overlapping phrase/product edits surface as Merge.Conflict.
    */
  def rebaseShadow(
      base: Module,
      baseChange: Cst,
      shadowChange: Cst
  ): Either[Merge.Conflict, (Module, Delta.ValidatedChangeSet)] =
    val touched = ChangeAlgebra.footprint(language, baseChange)
    val overrides = shadowOverrideTargets(shadowChange)
    val overlap = touched.intersect(overrides)
    if overlap.nonEmpty then
      Left(Merge.Conflict(
        overlap,
        Artifact(ArtifactKind.ChangeSet, Cst.toCanon(baseChange)).digest,
        Artifact(ArtifactKind.ChangeSet, Cst.toCanon(shadowChange)).digest))
    else
      Merge.threeWay(language, base, baseChange, shadowChange) match
        case Left(c) => Left(c)
        case Right(result) =>
          validate(result._1) match
            case Left(err) => throw IllegalStateException(s"rebase produced invalid SDS: $err")
            case Right(_)  => Right(result)

  // ---- the compiled document view (a bidirectional surface, M47) ----

  val docGrammar: GrammarSpec = GrammarSpec(
    name = "sds-document",
    tokens = TokenSpec(List("SDS", "section", "hazard"), List(":"), None),
    categories = List(
      CategorySpec("doc", List(
        ConstructorSpec("doc", List(
          Elem.Tok("SDS"), Elem.Tok(":"), Elem.StrLeaf, Elem.Star(Elem.Cat("line")))))),
      CategorySpec("line", List(
        ConstructorSpec("hazardLine", List(
          Elem.Tok("hazard"), Elem.NameLeaf, Elem.Tok(":"), Elem.StrLeaf))))),
    precCategories = Nil,
    printRules = List(
      PrintRule("doc", List(
        PrintSeg.Lit("SDS:"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Newline,
        PrintSeg.SepFields(1, "\n"))),
      PrintRule("hazardLine", List(
        PrintSeg.Lit("hazard"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
        PrintSeg.Lit(":"), PrintSeg.Space, PrintSeg.StrField(1)))),
    top = "doc")

  /** Resolve a phrase (free-text or corpus) with multilingual fallback:
    * exact lang → `en` → any.
    */
  def phraseText(m: Module, phraseName: String, lang: String): Option[String] =
    def texts: List[(String, String)] = m.defs.collect {
      case (_, Cst.Node("corpusPhrase" | "phrase", List(Cst.Leaf(n), Cst.Leaf(l), Cst.Leaf(t))))
          if n == phraseName =>
        (l, t)
    }.toList
    val all = texts
    all.collectFirst { case (l, t) if l == lang => t }
      .orElse(all.collectFirst { case (l, t) if l == "en" => t })
      .orElse(all.headOption.map(_._2))

  /** EN slot map + locale free-text rows for a typed section (refs excluded). */
  private def typedPlainRows(section: Cst): List[(String, String, String)] = section match
    case Cst.Node(tag, kids) if typedSectionTags.contains(tag) =>
      val keys = typedSectionKeys.getOrElse(tag, Nil)
      if kids.length != keys.length + 1 then Nil
      else
        val en = keys.zip(kids.init).collect {
          case (k, Cst.Leaf(v)) => (k, "en", v)
        }
        val loc = localeRows(kids.last).collect {
          case Cst.Node("fieldLocale", List(Cst.Leaf(k), Cst.Leaf(l), Cst.Leaf(t))) =>
            (k, l, t)
        }
        en ++ loc
    case _ => Nil

  private def typedRefRows(section: Cst): List[(String, String, String)] = section match
    case Cst.Node(tag, kids) if typedSectionTags.contains(tag) =>
      localeRows(kids.last).collect {
        case Cst.Node("fieldLocaleRef", List(Cst.Leaf(k), Cst.Leaf(l), Cst.Leaf(ref))) =>
          (k, l, ref)
      }
    case _ => Nil

  private def resolveTypedKey(tag: String, fieldKey: String): String =
    if tag == "toxicologicalSection" && fieldKey == "LD50Oral" then "ld50Oral"
    else fieldKey

  /** Resolve a section field inside an `euSection` or typed section with the
    * same multilingual fallback as [[phraseText]]: exact lang → `en` → any.
    */
  def sectionFieldText(section: Cst, fieldKey: String, lang: String): Option[String] =
    section match
      case Cst.Node("euSection", List(_, Cst.Node("list", fields))) =>
        val all = fields.collect {
          case Cst.Node("sectionField", List(Cst.Leaf(k), Cst.Leaf(l), Cst.Leaf(t)))
              if k == fieldKey =>
            (l, t)
        }
        all.collectFirst { case (l, t) if l == lang => t }
          .orElse(all.collectFirst { case (l, t) if l == "en" => t })
          .orElse(all.headOption.map(_._2))
      case Cst.Node(tag, _) if typedSectionTags.contains(tag) =>
        val k = resolveTypedKey(tag, fieldKey)
        val all = typedPlainRows(section).collect {
          case (key, l, t) if key == k => (l, t)
        }
        all.collectFirst { case (l, t) if l == lang => t }
          .orElse(all.collectFirst { case (l, t) if l == "en" => t })
          .orElse(all.headOption.map(_._2))
      case _ => None

  /** Lookup [[sectionFieldText]] by module binding (section ref), applying
    * any `sectionFieldShadow` industrial override for that (section, key).
    * `sectionFieldRef` / `fieldLocaleRef` rows resolve through [[phraseText]].
    */
  def sectionFieldText(m: Module, sectionRef: String, fieldKey: String, lang: String)
      : Option[String] =
    val overridden = m.defs.collectFirst {
      case (_, Cst.Node("sectionFieldShadow", List(Cst.Leaf(s), Cst.Leaf(k), Cst.Leaf(text))))
          if s == sectionRef && (k == fieldKey || k == resolveTypedKey("", fieldKey)) =>
        text
    }
    overridden.orElse {
      m.get(sectionRef).flatMap {
        case sec @ Cst.Node("euSection", List(_, Cst.Node("list", fields))) =>
          val refs = fields.collect {
            case Cst.Node("sectionFieldRef", List(Cst.Leaf(k), Cst.Leaf(l), Cst.Leaf(ref)))
                if k == fieldKey =>
              (l, ref)
          }
          val viaRef =
            refs.collectFirst { case (l, ref) if l == lang => ref }
              .orElse(refs.collectFirst { case (l, ref) if l == "en" => ref })
              .orElse(refs.headOption.map(_._2))
              .flatMap(r => phraseText(m, r, lang))
          viaRef.orElse(sectionFieldText(sec, fieldKey, lang))
        case sec @ Cst.Node(tag, _) if typedSectionTags.contains(tag) =>
          val rk = resolveTypedKey(tag, fieldKey)
          val refs = typedRefRows(sec).collect {
            case (k, l, ref) if k == rk => (l, ref)
          }
          val viaRef =
            refs.collectFirst { case (l, ref) if l == lang => ref }
              .orElse(refs.collectFirst { case (l, ref) if l == "en" => ref })
              .orElse(refs.headOption.map(_._2))
              .flatMap(r => phraseText(m, r, lang))
          viaRef.orElse(sectionFieldText(sec, fieldKey, lang))
        case other => sectionFieldText(other, fieldKey, lang)
      }
    }

  /** Compile a product's document: phrases in the requested language (with
    * fallback), shadow overrides applied — a view over typed objects.
    */
  def render(m: Module, productName: String, lang: String): Either[String, String] =
    for
      _ <- validate(m)
      product <- m.get(productName).toRight(s"no product '$productName'")
      labelRefs <- product match
        case Cst.Node("product", List(Cst.Leaf(label), _, Cst.Node("list", prs))) =>
          Right((label, prs.collect { case Cst.Leaf(p) => p }))
        case other => Left(s"'$productName' is not a product: ${other.render}")
      label = labelRefs._1
      phraseRefs = labelRefs._2
      lines <- phraseRefs.foldLeft[Either[String, List[Cst]]](Right(Nil)) { (acc, pr) =>
        acc.flatMap { ls =>
          val overridden = m.defs.collectFirst {
            case (_, Cst.Node("shadow", List(Cst.Leaf(p), Cst.Leaf(ph), Cst.Leaf(text))))
              if p == productName && ph == pr => text }
          val text = overridden.orElse(phraseText(m, pr, lang))
          text.toRight(s"no phrase '$pr' (lang=$lang)").map(t =>
            ls :+ Cst.node("hazardLine", Cst.Leaf(pr), Cst.Leaf(t)))
        }
      }
      doc = Cst.node("doc", Cst.Leaf(label), Cst.Node("list", lines))
      text <- Printer.print(docGrammar, doc)
      _ <- RoundTrip.check(docGrammar, doc)
    yield text
