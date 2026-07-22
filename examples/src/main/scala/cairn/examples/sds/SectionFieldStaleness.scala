package cairn.examples.sds

import cairn.kernel.*
import cairn.core.*

/** Multilingual section-field staleness machine — thin sibling of
  * [[PhraseStaleness]] for `euSection` / `sectionField` rows.
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

  /** Project all locale variants of one field key inside an `euSection` binding. */
  def project(m: Module, sectionRef: String, fieldKey: String): Map[String, TranslatedText] =
    m.get(sectionRef) match
      case Some(Cst.Node("euSection", List(_, Cst.Node("list", fields)))) =>
        val rows = fields.collect {
          case Cst.Node("sectionField", List(Cst.Leaf(k), Cst.Leaf(lang), Cst.Leaf(text)))
              if k == fieldKey =>
            (lang, text)
        }
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
