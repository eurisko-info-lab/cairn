package cairn.examples.sds

import cairn.kernel.*
import cairn.core.*

/** Multilingual section-field staleness machine — thin sibling of
  * [[PhraseStaleness]] for `euSection` / typed identification+hazards /
  * `sectionField` rows.
  *
  * Reuses [[PhraseStaleness.restale]] / [[PhraseStaleness.TranslatedText]]:
  * free-text locale siblings become [[PhraseStaleness.State.StaleBecauseSourceChanged]]
  * when the English source hash drifts. Section fields have no official-corpus
  * ctor (unlike `corpusPhrase`), so every projected row is HumanReviewed.
  * State is projected over Module objects — not Studio-persisted.
  *
  * Industrial overrides live as language `sectionFieldShadow` terms (see SDS
  * domain gate + [[cairn.user.sds.Sds.sectionFieldText]]); this machine only
  * judges translation freshness of the underlying fields.
  */
object SectionFieldStaleness:
  import PhraseStaleness.{State, TranslatedText, restale, textHash}

  private def localeRows(overlays: Cst): List[Cst] = overlays match
    case Cst.Node("none", _) => Nil
    case Cst.Node("some", List(Cst.Node("list", xs))) => xs
    case Cst.Node("list", xs) => xs
    case _ => Nil

  private def rowsForKey(sec: Cst, fieldKey: String): List[(String, String)] = sec match
    case Cst.Node("euSection", List(_, Cst.Node("list", fields))) =>
      fields.collect {
        case Cst.Node("sectionField", List(Cst.Leaf(k), Cst.Leaf(lang), Cst.Leaf(text)))
            if k == fieldKey =>
          (lang, text)
      }
    case Cst.Node("identificationSection", List(
        Cst.Leaf(pn), Cst.Leaf(syn), Cst.Leaf(use), Cst.Leaf(against),
        Cst.Leaf(supplier), Cst.Leaf(phone), overlays)) =>
      val en = Map(
        "productName" -> pn, "synonyms" -> syn, "recommendedUse" -> use,
        "usesAdvisedAgainst" -> against, "supplierName" -> supplier,
        "emergencyPhone" -> phone)
      val enRow = en.get(fieldKey).toList.map(t => ("en", t))
      val loc = localeRows(overlays).collect {
        case Cst.Node("fieldLocale", List(Cst.Leaf(k), Cst.Leaf(lang), Cst.Leaf(text)))
            if k == fieldKey =>
          (lang, text)
      }
      enRow ++ loc
    case Cst.Node("hazardsSection", List(
        Cst.Leaf(cls), Cst.Leaf(hnoc), Cst.Leaf(phrases), Cst.Leaf(signal),
        Cst.Leaf(pictos), overlays)) =>
      val en = Map(
        "classificationSummary" -> cls,
        "hazardsNotOtherwiseClassified" -> hnoc,
        "hazardPhrases" -> phrases,
        "signalWord" -> signal,
        "pictograms" -> pictos)
      val enRow = en.get(fieldKey).toList.map(t => ("en", t))
      val loc = localeRows(overlays).collect {
        case Cst.Node("fieldLocale", List(Cst.Leaf(k), Cst.Leaf(lang), Cst.Leaf(text)))
            if k == fieldKey =>
          (lang, text)
      }
      enRow ++ loc
    case _ => Nil

  /** Project all locale variants of one field key inside a section binding. */
  def project(m: Module, sectionRef: String, fieldKey: String): Map[String, TranslatedText] =
    m.get(sectionRef) match
      case Some(sec) =>
        val rows = rowsForKey(sec, fieldKey)
        val enText = rows.collectFirst { case ("en", t) => t }.getOrElse("")
        val enHash = textHash(enText)
        rows.map { case (lang, text) =>
          lang -> TranslatedText(text, enHash.hex, State.HumanReviewed)
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
