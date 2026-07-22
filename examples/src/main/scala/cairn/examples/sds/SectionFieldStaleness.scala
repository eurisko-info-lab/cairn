package cairn.examples.sds

import cairn.kernel.*
import cairn.core.*

/** Multilingual section-field staleness machine — sibling of
  * [[PhraseStaleness]] for `euSection` / typed sections / `sectionField` rows.
  *
  * Reuses [[PhraseStaleness.restale]] / [[PhraseStaleness.TranslatedText]]:
  * free-text locale siblings become [[PhraseStaleness.State.StaleBecauseSourceChanged]]
  * when the English source hash drifts. Section fields have no official-corpus
  * ctor (unlike `corpusPhrase`), so every projected row defaults HumanReviewed.
  *
  * Two paths:
  *   - [[restale]] / [[project]] — pure projection (legacy host judgment)
  *   - [[deriveEnRewrite]] — real ΔSDS changeset that rewrites EN and
  *     materializes `sectionFieldState` marks (derived ΔL, not Studio UI)
  *
  * [[project]] prefers persisted `sectionFieldState` marks when present.
  * Industrial overrides live as language `sectionFieldShadow` terms.
  */
object SectionFieldStaleness:
  import PhraseStaleness.{State, TranslatedText, restale, textHash, stateTag, tagState}

  final case class FieldRow(lang: String, text: String)

  def markName(sectionRef: String, fieldKey: String, lang: String): String =
    s"${sectionRef}__${fieldKey}__${lang}__state"

  def stateTerm(
      sectionRef: String,
      fieldKey: String,
      lang: String,
      fromHash: Digest,
      state: State
  ): Cst =
    Cst.node(
      "sectionFieldState",
      Cst.Leaf(sectionRef),
      Cst.Leaf(fieldKey),
      Cst.Leaf(lang),
      Cst.Leaf(fromHash.hex),
      Cst.Leaf(stateTag(state)))

  private def localeRows(overlays: Cst): List[Cst] = overlays match
    case Cst.Node("none", _) => Nil
    case Cst.Node("some", List(Cst.Node("list", xs))) => xs
    case Cst.Node("list", xs) => xs
    case _ => Nil

  /** Slot order mirrors SDS typed section ctors (see `Sds.typedSectionKeys`). */
  private val typedSlots: Map[String, List[String]] = Map(
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

  private def typedEnSlots(sec: Cst): Map[String, String] = sec match
    case Cst.Node(tag, kids) =>
      typedSlots.get(tag) match
        case Some(slots) if kids.length == slots.length + 1 =>
          slots.zip(kids.init).collect { case (k, Cst.Leaf(v)) => k -> v }.toMap
        case _ => Map.empty
    case _ => Map.empty

  private def rowsForKey(sec: Cst, fieldKey: String): List[FieldRow] = sec match
    case Cst.Node("euSection", List(_, Cst.Node("list", fields))) =>
      fields.collect {
        case Cst.Node("sectionField", List(Cst.Leaf(k), Cst.Leaf(lang), Cst.Leaf(text)))
            if k == fieldKey =>
          FieldRow(lang, text)
      }
    case Cst.Node(tag, kids) if typedEnSlots(sec).nonEmpty =>
      val resolved =
        if tag == "toxicologicalSection" && fieldKey == "LD50Oral" then "ld50Oral"
        else fieldKey
      val enRow = typedEnSlots(sec).get(resolved).toList.map(t => FieldRow("en", t))
      val loc = localeRows(kids.last).collect {
        case Cst.Node("fieldLocale", List(Cst.Leaf(k), Cst.Leaf(lang), Cst.Leaf(text)))
            if k == resolved =>
          FieldRow(lang, text)
      }
      enRow ++ loc
    case _ => Nil

  /** Persisted marks: lang → (fromHash, state). */
  def stateMarks(m: Module, sectionRef: String, fieldKey: String): Map[String, (String, State)] =
    m.defs.collect {
      case (_, Cst.Node("sectionFieldState", List(
          Cst.Leaf(sec), Cst.Leaf(key), Cst.Leaf(lang), Cst.Leaf(hash), Cst.Leaf(tag))))
          if sec == sectionRef && (key == fieldKey ||
            (fieldKey == "LD50Oral" && key == "ld50Oral")) =>
        tagState.get(tag).map(st => lang -> (hash, st))
    }.flatten.toMap

  /** Project all locale variants of one field key inside a section binding. */
  def project(m: Module, sectionRef: String, fieldKey: String): Map[String, TranslatedText] =
    m.get(sectionRef) match
      case Some(sec) =>
        val rows = rowsForKey(sec, fieldKey)
        val marks = stateMarks(m, sectionRef, fieldKey)
        val enText = rows.collectFirst { case FieldRow("en", t) => t }.getOrElse("")
        val enHash = textHash(enText)
        rows.map { case FieldRow(lang, text) =>
          marks.get(lang) match
            case Some((hash, st)) => lang -> TranslatedText(text, hash, st)
            case None => lang -> TranslatedText(text, enHash.hex, State.HumanReviewed)
        }.toMap
      case _ => Map.empty

  /** Langs (other than `en`) that become stale if English free-text becomes `newEnText`. */
  def staleLangsAfterEnChange(
      m: Module,
      sectionRef: String,
      fieldKey: String,
      newEnText: String
  ): Set[String] =
    val after = restale(project(m, sectionRef, fieldKey), textHash(newEnText))
    after.collect {
      case (lang, t) if lang != "en" && t.state == State.StaleBecauseSourceChanged => lang
    }.toSet

  /** Rewrite EN free-text for a field inside an euSection (list of sectionField).
    * Typed sections: rebuild EN slot + keep locales.
    */
  private def rewriteEn(sec: Cst, fieldKey: String, newEnText: String): Either[String, Cst] =
    sec match
      case Cst.Node("euSection", List(num, Cst.Node("list", fields))) =>
        var found = false
        val next = fields.map {
          case Cst.Node("sectionField", List(Cst.Leaf(k), Cst.Leaf("en"), _)) if k == fieldKey =>
            found = true
            Cst.node("sectionField", Cst.Leaf(k), Cst.Leaf("en"), Cst.Leaf(newEnText))
          case other => other
        }
        if !found then Left(s"no EN sectionField '$fieldKey'")
        else Right(Cst.Node("euSection", List(num, Cst.Node("list", next))))
      case Cst.Node(tag, kids) =>
        typedSlots.get(tag) match
          case None => Left("section body is not rewriteable")
          case Some(slots) if kids.length != slots.length + 1 =>
            Left(s"bad $tag arity")
          case Some(slots) =>
            val key =
              if tag == "toxicologicalSection" && fieldKey == "LD50Oral" then "ld50Oral"
              else fieldKey
            if !slots.contains(key) then Left(s"unknown $tag key '$fieldKey'")
            else
              val vals = kids.init.map { case Cst.Leaf(t) => t; case _ => "" }
              Right(Cst.Node(tag,
                vals.updated(slots.indexOf(key), newEnText).map(Cst.Leaf(_)) :+ kids.last))
      case _ => Left("section body is not rewriteable")

  /** Derive a ΔSDS changeset: replace EN field and add/replace
    * `sectionFieldState` marks for langs that go stale.
    */
  def deriveEnRewrite(
      l: ComposedLanguage,
      m: Module,
      sectionRef: String,
      fieldKey: String,
      newEnText: String
  ): Either[String, Cst] =
    m.get(sectionRef) match
      case None => Left(s"no section '$sectionRef'")
      case Some(sec) =>
        val rows = rowsForKey(sec, fieldKey)
        rows.find(_.lang == "en") match
          case None => Left(s"no EN field '$fieldKey' in '$sectionRef'")
          case Some(enRow) =>
            rewriteEn(sec, fieldKey, newEnText).map { rewritten =>
              val oldHash = textHash(enRow.text)
              val after = restale(project(m, sectionRef, fieldKey), textHash(newEnText))
              val markKey =
                if fieldKey == "LD50Oral" then "ld50Oral" else fieldKey
              val ops = List.newBuilder[Cst]
              ops += Cst.Node(
                Delta.tag(l, "replace"),
                List(Cst.Leaf(sectionRef), rewritten))
              for (lang, t) <- after.toList.sortBy(_._1)
              if lang != "en" && t.state == State.StaleBecauseSourceChanged do
                val name = markName(sectionRef, markKey, lang)
                val term = stateTerm(sectionRef, markKey, lang, oldHash, State.StaleBecauseSourceChanged)
                val op = if m.get(name).isDefined then "replace" else "add"
                ops += Cst.Node(Delta.tag(l, op), List(Cst.Leaf(name), term))
              Cst.Node(Delta.tag(l, "changeset"), List(Cst.Node("list", ops.result())))
            }

  def applyEnRewrite(
      applySds: (Module, Cst) => Either[String, (Module, Delta.ValidatedChangeSet)],
      l: ComposedLanguage,
      m: Module,
      sectionRef: String,
      fieldKey: String,
      newEnText: String
  ): Either[String, (Module, Delta.ValidatedChangeSet)] =
    deriveEnRewrite(l, m, sectionRef, fieldKey, newEnText).flatMap(ch => applySds(m, ch))
