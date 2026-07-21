package cairn.examples.sds

import cairn.kernel.*
import cairn.core.*

/** Multilingual phrase-staleness machine (GRANITE PROMPT §15 / Multilingual.restale).
  *
  * Thin honest stub: official `corpusPhrase` entries never go stale; free-text
  * `phrase` translations become [[State.StaleBecauseSourceChanged]] when their
  * English source hash drifts. State is a *projected* judgment over Module
  * objects — not yet persisted as SDS Studio fields or a full phrase corpus.
  */
object PhraseStaleness:
  enum State:
    case OfficialCorpus, HumanReviewed, AiDraft, StaleBecauseSourceChanged, Rejected

  final case class TranslatedText(text: String, translatedFromHash: String, state: State)

  def textHash(text: String): Digest = Digest.of(Canon.CStr(text))

  /** Pure restale (GRANITE `Multilingual.restale`). Keys are lang codes. */
  def restale(
      current: Map[String, TranslatedText],
      newEnglishHash: Digest
  ): Map[String, TranslatedText] =
    val target = newEnglishHash.hex
    current.map {
      case ("en", t) => "en" -> t
      case (lang, t) if t.state == State.OfficialCorpus => lang -> t
      case (lang, t) if t.translatedFromHash == target => lang -> t
      case (lang, t) => lang -> t.copy(state = State.StaleBecauseSourceChanged)
    }

  private def entry(
      official: Boolean,
      text: String,
      enHash: Digest
  ): TranslatedText =
    if official then TranslatedText(text, enHash.hex, State.OfficialCorpus)
    else TranslatedText(text, enHash.hex, State.HumanReviewed)

  /** Project all locale variants of a phrase name from a Module. */
  def project(m: Module, phraseName: String): Map[String, TranslatedText] =
    val rows: List[(String, Boolean, String)] = m.defs.collect {
      case (_, Cst.Node("corpusPhrase", List(Cst.Leaf(n), Cst.Leaf(lang), Cst.Leaf(text))))
          if n == phraseName =>
        (lang, true, text)
      case (_, Cst.Node("phrase", List(Cst.Leaf(n), Cst.Leaf(lang), Cst.Leaf(text))))
          if n == phraseName =>
        (lang, false, text)
    }.toList
    val enText = rows.collectFirst { case ("en", _, t) => t }.getOrElse("")
    val enHash = textHash(enText)
    rows.map { case (lang, official, text) => lang -> entry(official, text, enHash) }.toMap

  /** Langs (other than `en`) that become stale if English free-text becomes `newEnText`. */
  def staleLangsAfterEnChange(m: Module, phraseName: String, newEnText: String): Set[String] =
    val after = restale(project(m, phraseName), textHash(newEnText))
    after.collect {
      case (lang, t) if lang != "en" && t.state == State.StaleBecauseSourceChanged => lang
    }.toSet
