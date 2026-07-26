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
  * ΔSDS = generic ΔL + [[ModuleStructural]] specs + Search.prove. Typed
  * section keys come from [[SurfaceSlots]] (surface `Tok`/`StrLeaf`), not
  * host maps. Phrase + section-field staleness: `translationState` /
  * `sectionFieldState` + `translationStateTag` judgment; host derive/apply
  * emit real ΔSDS changesets. Regulatory conformance: ΔSDS domain gate
  * calls `eu-clp` `sectionNumberOk` and SDS `translationStateTag`.
  * Chemical instances load from `languages/sds/chemicals/` via
  * `ChemicalSource`. Report encodings are the `sds-report` projection pack.
  * `sectionField` / `sectionFieldRef` resolve with multilingual fallback, then
  * `sectionFieldShadow` overrides.
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

  /** Typed section ctors from the pack (declaration order ⇒ EU-CLP 1..16). */
  lazy val typedSectionOrder: List[String] =
    SurfaceSlots.ctorsEndingWith(language, "Section", exclude = Set("euSection"))

  lazy val typedSectionTags: Set[String] = typedSectionOrder.toSet

  /** Typed section tag → its 1-indexed EU-CLP number (pack declaration
    * order) — pure data feeding [[ModuleStructural.NumberSource.ByTag]],
    * replacing the Scala-side lookup [[sectionNumber]] used to do inline.
    */
  private lazy val typedSectionNumbers: Map[String, Int] =
    typedSectionOrder.zipWithIndex.map((tag, i) => tag -> (i + 1)).toMap

  /** Ordered EN slot keys from the SDS surface (`Tok(label) StrLeaf`), not host maps. */
  lazy val typedSectionKeys: Map[String, List[String]] =
    typedSectionOrder.map(tag => tag -> SurfaceSlots.labeledStrSlots(language.grammar, tag)).toMap

  private lazy val sectionBodyTags: Set[String] = typedSectionTags + "euSection"

  /** Kernel-check `sectionNumberOk` (eu-clp pack) — ΔSDS domain gate uses this,
    * not a host-only 1..16 Set.
    */
  def checkSectionNumber(n: String): Boolean =
    Search.prove(
      CheckerCfg(euClpLanguage.judgments.values.toList),
      Cst.node("sectionNumberOk", Cst.Leaf(n))).isRight

  /** EU-CLP number implied by a section body ctor (pack order / euSection leaf). */
  def sectionNumber(sec: Cst): Option[Int] = sec match
    case Cst.Node("euSection", List(Cst.Leaf(num), _)) => num.toIntOption
    case Cst.Node(tag, _) =>
      val i = typedSectionOrder.indexOf(tag)
      if i >= 0 then Some(i + 1) else None
    case _ => None

  // ---- domain validation (ΔSDS = ModuleStructural specs + Search.prove) ----

  /** Specs that are genuinely GRAMMAR-DERIVED, not hand-authored pack data:
    * which typed sections exist, their field labels, and their 1-indexed
    * EU-CLP position all come from `sds.cairn`'s own ctor declarations via
    * [[SurfaceSlots]] — duplicating them as static `validate` text would
    * either drift against the grammar or require re-deriving the exact same
    * per-section enumeration this generic computation already avoids. Kept
    * in Scala for that reason; everything else lives in `sds.cairn` as
    * `provider`/`validate` pack data (see [[ValidationModelLoader]]).
    */
  private lazy val dynamicSpecs: List[ModuleStructural.Spec] =
    val typedSpecs = typedSectionKeys.toList.flatMap { (tag, keys) =>
      List(
        ModuleStructural.Spec.NonEmptyLeaves(tag, keys.indices.toList, keys),
        ModuleStructural.Spec.KeyedLocaleOverlay(
          tag, keys.length, keys.toSet,
          "fieldLocale", "fieldLocaleRef", 0, 1, 2, tag),
      )
    }
    List(
      ModuleStructural.Spec.RefTagIn("sectionFieldShadow", 0, sectionBodyTags, "sectionFieldShadow"),
      ModuleStructural.Spec.RefTagIn("sectionFieldState", 0, sectionBodyTags, "sectionFieldState"),
      ModuleStructural.Spec.OutlineNums(
        "outline", 2,
        List(
          ModuleStructural.NumberSource.FromLeaf("euSection", 0),
          ModuleStructural.NumberSource.ByTag(typedSectionNumbers)),
        JudgmentRef(euClpLanguage.digest, "sectionNumberOk"), "outline"),
    ) ++ typedSpecs

  /** The judgment-providing languages any spec's [[JudgmentRef]] names,
    * keyed by their own digest — SDS's own `language` (translationStateTag,
    * via the pack's `provider self = language sds;`) and `euClpLanguage`
    * (sectionNumberOk, via `provider clp = language eu-clp;`). Public:
    * external callers building a `ModuleGate.fromValidationModel`/
    * `ModuleStructural.run` resolver (e.g. `EuClp.conform`,
    * `SdsCausalWorkflow`) need the same mapping.
    */
  lazy val judgmentProviders: Map[Digest, ComposedLanguage] =
    Map(language.digest -> language, euClpLanguage.digest -> euClpLanguage)

  /** Canonical, content-addressed identity for the complete `sds.validate`
    * gate: the pack-declared specs (loaded from `sds.cairn`'s `provider`/
    * `validate` declarations via [[ValidationModelLoader]]) plus
    * [[dynamicSpecs]]. `ModuleGate.fromValidationModel`'s descriptor binds
    * to this digest directly, so two gates built from the same model are
    * provably the same check (including which judgment-provider languages
    * it uses), not just same-named.
    */
  lazy val validationModel: ValidationModel =
    val loaded = ValidationModelLoader.resolve(language, packs.requireClosed(_))
    loaded.copy(specs = loaded.specs ++ dynamicSpecs)

  def validate(m: Module): Either[String, Unit] =
    val es = ModuleStructural.run(m, validationModel.specs, judgmentProviders.get)
    if es.isEmpty then Right(()) else Left(es.mkString("; "))

  /** ΔSDS application: the generic ΔL, then the domain gate. */
  def applySds(m: Module, change: Cst): Either[String, (Module, Delta.ValidatedChangeSet)] =
    Delta.apply(language, m, change).flatMap { out =>
      validate(out._1).map(_ => out) }

  /** Kernel-check a `translationStateTag` judgment goal (parallel to EU-CLP
    * `sectionNumberOk`).
    */
  def checkTranslationStateTag(tag: String): Boolean =
    Search.prove(
      CheckerCfg(language.judgments.values.toList),
      Cst.node("translationStateTag", Cst.Leaf(tag))).isRight

  /** Phrase / product / section names a shadow change-set overrides.
    * Domain-aware footprint for GRANITE-style shadow rebase — ctor indices
    * only; walk is [[ModuleStructural.changeLeafRefs]].
    */
  def shadowOverrideTargets(change: Cst): Set[String] =
    ModuleStructural.changeLeafRefs(
      change,
      Map("shadow" -> List(0, 1), "sectionFieldShadow" -> List(0)))

  /** Rebase an industrial shadow over a base revision. Disjoint overrides
    * merge; overlapping phrase/product edits surface as Merge.Conflict.
    * The SDS whole-document gate runs inside [[Merge.threeWay]] via
    * [[ModuleGate]], so domain rejection is witnessed on the conflict artifact.
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
        overlap.map(SemanticLocation.WholeDefinition(_)),
        Artifact(ArtifactKind.ChangeSet, Cst.toCanon(baseChange)).digest,
        Artifact(ArtifactKind.ChangeSet, Cst.toCanon(shadowChange)).digest))
    else
      Merge.threeWay(
        language, base, baseChange, shadowChange,
        ModuleGate.fromValidationModel("sds.validate", validationModel, judgmentProviders.get))

  // ---- the compiled document view (a bidirectional surface, M47) ----

  /** Product-phrase document surface: `languages/sds-document.cairn` +
    * `surfaces/default.cairn` — an ordinary pack grammar, not a host
    * `GrammarSpec` literal. Loads without recompiling Scala, same as
    * `sds-report`.
    */
  private lazy val docLanguage: ComposedLanguage = packs.requireClosed("sds-document")
  def docGrammar: GrammarSpec = docLanguage.grammar

  /** Resolve a phrase (free-text or corpus) with multilingual fallback:
    * exact lang → `en` → any ([[MultilingualResolve]]).
    */
  def phraseText(m: Module, phraseName: String, lang: String): Option[String] =
    MultilingualResolve.pick(
      ModuleFieldResolve.namedLangText(
        m, Set("corpusPhrase", "phrase"), 0, 1, 2, phraseName),
      lang)

  /** EN slot map + locale free-text rows for a typed section (refs excluded). */
  private def typedPlainRows(section: Cst): List[(String, String, String)] = section match
    case Cst.Node(tag, kids) if typedSectionTags.contains(tag) =>
      val keys = typedSectionKeys.getOrElse(tag, Nil)
      val en = ModuleFieldResolve.typedSlots(keys, kids).toList.map((k, v) => (k, "en", v))
      val loc = kids.lastOption.toList.flatMap(ModuleFieldResolve.optListRows).collect {
        case Cst.Node("fieldLocale", List(Cst.Leaf(k), Cst.Leaf(l), Cst.Leaf(t))) => (k, l, t)
      }
      en ++ loc
    case _ => Nil

  private def typedRefRows(section: Cst): List[(String, String, String)] = section match
    case Cst.Node(tag, kids) if typedSectionTags.contains(tag) =>
      kids.lastOption.toList.flatMap(ModuleFieldResolve.optListRows).collect {
        case Cst.Node("fieldLocaleRef", List(Cst.Leaf(k), Cst.Leaf(l), Cst.Leaf(ref))) =>
          (k, l, ref)
      }
    case _ => Nil

  /** Resolve a section field inside an `euSection` or typed section with the
    * same multilingual fallback as [[phraseText]]: exact lang → `en` → any.
    */
  def sectionFieldText(section: Cst, fieldKey: String, lang: String): Option[String] =
    section match
      case Cst.Node("euSection", List(_, Cst.Node("list", fields))) =>
        MultilingualResolve.pick(
          ModuleFieldResolve.keyedRows(
            fields, Set("sectionField"), 0, 1, 2, fieldKey),
          lang)
      case Cst.Node(tag, _) if typedSectionTags.contains(tag) =>
        MultilingualResolve.pick(
          typedPlainRows(section).collect { case (k, l, t) if k == fieldKey => (l, t) },
          lang)
      case _ => None

  /** Lookup [[sectionFieldText]] by module binding (section ref), applying
    * any `sectionFieldShadow` industrial override for that (section, key).
    * `sectionFieldRef` / `fieldLocaleRef` rows resolve through [[phraseText]].
    */
  def sectionFieldText(m: Module, sectionRef: String, fieldKey: String, lang: String)
      : Option[String] =
    val overridden = m.defs.collectFirst {
      case (_, Cst.Node("sectionFieldShadow", List(Cst.Leaf(s), Cst.Leaf(k), Cst.Leaf(text))))
          if s == sectionRef && k == fieldKey =>
        text
    }
    overridden.orElse {
      m.get(sectionRef).flatMap {
        case sec @ Cst.Node("euSection", List(_, Cst.Node("list", fields))) =>
          MultilingualResolve.pickRef(
            ModuleFieldResolve.keyedRows(
              fields, Set("sectionFieldRef"), 0, 1, 2, fieldKey),
            lang).flatMap(r => phraseText(m, r, lang))
            .orElse(sectionFieldText(sec, fieldKey, lang))
        case sec @ Cst.Node(tag, _) if typedSectionTags.contains(tag) =>
          MultilingualResolve.pickRef(
            typedRefRows(sec).collect { case (k, l, ref) if k == fieldKey => (l, ref) },
            lang).flatMap(r => phraseText(m, r, lang))
            .orElse(sectionFieldText(sec, fieldKey, lang))
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
