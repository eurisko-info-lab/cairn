package cairn.examples.sds
import cairn.runtime.EffectContexts

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.PackLoader

/** Multilingual phrase-staleness machine (GRANITE PROMPT §15 / Multilingual.restale).
  *
  * Official `corpusPhrase` entries never go stale; free-text `phrase`
  * translations become [[State.StaleBecauseSourceChanged]] when their English
  * source hash drifts.
  *
  * Two paths:
  *   - [[restale]] / [[project]] — pure projection (legacy host judgment)
  *   - [[deriveEnRewrite]] — real ΔSDS changeset that rewrites EN and
  *     materializes `translationState` marks (derived ΔL, not Studio UI)
  *
  * [[project]] prefers persisted `translationState` marks when present.
  */
object PhraseStaleness:
  enum State:
    case OfficialCorpus, HumanReviewed, AiDraft, StaleBecauseSourceChanged, Rejected

  final case class TranslatedText(text: String, translatedFromHash: String, state: State)

  final case class PhraseRow(defName: String, lang: String, official: Boolean, text: String)

  /** Tags from the SDS `translationStateTag` judgment — not a host allowlist. */
  def officialTags(language: ComposedLanguage): Map[State, String] =
    val leaves = language.judgments.get("translationStateTag").toList
      .flatMap(_.rules)
      .flatMap { r =>
        r.conclusion match
          case Cst.Node("translationStateTag", List(Cst.Leaf(t))) => Some(t)
          case _ => None
      }.toSet
    val wanted = Map(
      State.OfficialCorpus -> "officialCorpus",
      State.HumanReviewed -> "humanReviewed",
      State.AiDraft -> "aiDraft",
      State.StaleBecauseSourceChanged -> "staleBecauseSourceChanged",
      State.Rejected -> "rejected")
    wanted.filter { case (_, tag) => leaves.contains(tag) }

  lazy val stateTag: Map[State, String] =
    officialTags(PackLoader(EffectContexts.forPackLoader()).requireClosed("sds"))

  lazy val tagState: Map[String, State] = stateTag.map(_.swap)

  def textHash(text: String): Digest = Digest.of(Canon.CStr(text))

  def markName(phraseName: String, lang: String): String =
    s"${phraseName}__${lang}__state"

  def stateTerm(
      phraseName: String,
      lang: String,
      fromHash: Digest,
      state: State
  ): Cst =
    Cst.node(
      "translationState",
      Cst.Leaf(phraseName),
      Cst.Leaf(lang),
      Cst.Leaf(fromHash.hex),
      Cst.Leaf(stateTag(state)))

  /** Pure restale via [[MultilingualRestale]] (GRANITE `Multilingual.restale`). */
  def restale(
      current: Map[String, TranslatedText],
      newEnglishHash: Digest
  ): Map[String, TranslatedText] =
    MultilingualRestale.restale(
      current,
      newEnglishHash.hex,
      isOfficial = _.state == State.OfficialCorpus,
      fromHash = _.translatedFromHash,
      markStale = t => t.copy(state = State.StaleBecauseSourceChanged),
    )

  private def entry(
      official: Boolean,
      text: String,
      enHash: Digest
  ): TranslatedText =
    if official then TranslatedText(text, enHash.hex, State.OfficialCorpus)
    else TranslatedText(text, enHash.hex, State.HumanReviewed)

  def phraseRows(m: Module, phraseName: String): List[PhraseRow] =
    val corpus = ModuleFieldResolve.namedBindings(
      m, Set("corpusPhrase"), 0, 1, 2, phraseName).map {
      case (defName, lang, text) => PhraseRow(defName, lang, true, text)
    }
    val free = ModuleFieldResolve.namedBindings(
      m, Set("phrase"), 0, 1, 2, phraseName).map {
      case (defName, lang, text) => PhraseRow(defName, lang, false, text)
    }
    corpus ++ free

  /** Persisted marks: lang → (fromHash, state). */
  def stateMarks(m: Module, phraseName: String): Map[String, (String, State)] =
    ModuleFieldResolve.stateMarks(m, "translationState", List(phraseName), 1, 2, 3)
      .flatMap { case (lang, hash, tag) =>
        tagState.get(tag).map(st => lang -> (hash, st))
      }.toMap

  /** Project all locale variants of a phrase name from a Module.
    * Materialized `translationState` marks override default projection.
    */
  def project(m: Module, phraseName: String): Map[String, TranslatedText] =
    val rows = phraseRows(m, phraseName)
    val marks = stateMarks(m, phraseName)
    val enText = rows.collectFirst { case PhraseRow(_, "en", _, t) => t }.getOrElse("")
    val enHash = textHash(enText)
    rows.map { case PhraseRow(_, lang, official, text) =>
      marks.get(lang) match
        case Some((hash, st)) => lang -> TranslatedText(text, hash, st)
        case None             => lang -> entry(official, text, enHash)
    }.toMap

  /** Langs (other than `en`) that become stale if English free-text becomes `newEnText`. */
  def staleLangsAfterEnChange(m: Module, phraseName: String, newEnText: String): Set[String] =
    val after = restale(project(m, phraseName), textHash(newEnText))
    after.collect {
      case (lang, t) if lang != "en" && t.state == State.StaleBecauseSourceChanged => lang
    }.toSet

  /** Derive a ΔSDS changeset: replace EN free-text and add/replace
    * `translationState` marks for free-text langs that go stale.
    * Corpus EN is rejected (official phrases are not restale-rewritten).
    */
  def deriveEnRewrite(
      l: ComposedLanguage,
      m: Module,
      phraseName: String,
      newEnText: String
  ): Either[String, Cst] =
    val rows = phraseRows(m, phraseName)
    rows.find(_.lang == "en") match
      case None => Left(s"no EN phrase '$phraseName'")
      case Some(enRow) if enRow.official =>
        Left(s"corpusPhrase '$phraseName' EN is not restale-rewritten")
      case Some(enRow) =>
        val oldHash = textHash(enRow.text)
        val newHash = textHash(newEnText)
        val after = restale(project(m, phraseName), newHash)
        val ops = List.newBuilder[Cst]
        ops += Cst.Node(
          Delta.tag(l, "replace"),
          List(
            Cst.Leaf(enRow.defName),
            Cst.node("phrase", Cst.Leaf(phraseName), Cst.Leaf("en"), Cst.Leaf(newEnText))))
        for (lang, t) <- after.toList.sortBy(_._1)
        if lang != "en" && t.state == State.StaleBecauseSourceChanged do
          val name = markName(phraseName, lang)
          val term = stateTerm(phraseName, lang, oldHash, State.StaleBecauseSourceChanged)
          val op = if m.get(name).isDefined then "replace" else "add"
          ops += Cst.Node(Delta.tag(l, op), List(Cst.Leaf(name), term))
        Right(Cst.Node(Delta.tag(l, "changeset"), List(Cst.Node("list", ops.result()))))

  /** Apply [[deriveEnRewrite]] through the SDS domain gate. */
  def applyEnRewrite(
      applySds: (Module, Cst) => Either[String, (Module, Delta.ValidatedChangeSet)],
      l: ComposedLanguage,
      m: Module,
      phraseName: String,
      newEnText: String
  ): Either[String, (Module, Delta.ValidatedChangeSet)] =
    deriveEnRewrite(l, m, phraseName, newEnText).flatMap(ch => applySds(m, ch))
