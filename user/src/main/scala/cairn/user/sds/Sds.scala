package cairn.user.sds

import cairn.kernel.*
import cairn.systeminterface.PackAccess
import cairn.core.*

/** SDS pack (M47, §5b): Safety Data Sheet authoring — flagship of
  * `PKI → Law → SDS`. An SDS is NOT a flat document: it is a compiled view
  * over typed objects (substances, mixtures, phrases / corpus phrases,
  * products, shadows, regulatory `basis` citations into Law sections,
  * EU-CLP `euSection` / typed `identificationSection` / `hazardsSection` /
  * `outline` / multilingual `sectionField` maps).
  *
  * Object language: [[languages/sds.cairn]] (`provides sds requires law`).
  * Closed composition pulls Law + PKI; compose without them fails.
  * ΔSDS = generic ΔL + domain validation. Scala = host glue only.
  * Phrase staleness (official corpus vs free-text restale) lives in the
  * examples host machine — see `cairn.examples.sds.PhraseStaleness`.
  * Regulatory section numbering prefers the versioned `eu-clp` pack
  * (`cairn.examples.sds.EuClp` / `SectionNumbering`). Chemical instances load
  * from `languages/sds/chemicals/` `.cairn` files (`ChemicalSource`); host maps remain
  * emit fixtures. Section report is the `sds-report` surface pack
  * (`default` text + `json` machine surface).
  * `sectionField` / `sectionFieldRef` resolve with multilingual fallback, then
  * `sectionFieldShadow` overrides. Typed identification/hazards sections flatten
  * to the same key/lang resolution path. Section-field staleness lives in
  * `cairn.examples.sds.SectionFieldStaleness` (reuses `PhraseStaleness.restale`).
  */
final class Sds(packs: PackAccess):
  lazy val fragments: List[Fragment] = packs.requireOwn("sds")

  /** Own fragment only — compose fails: `requires law` unmet without Law/PKI. */
  def ownCompose: Either[List[ComposeError], ComposedLanguage] =
    Compose.compose("sds", fragments)

  /** Closed language: SDS + demoted Law + demoted PKI. */
  lazy val language: ComposedLanguage = packs.requireClosed("sds")

  /** EU-CLP section numbers accepted by the ΔSDS domain gate (1..16). */
  private val euClpNumbers: Set[Int] = (1 to 16).toSet

  private val identificationKeys: Set[String] = Set(
    "productName", "synonyms", "recommendedUse", "usesAdvisedAgainst",
    "supplierName", "emergencyPhone")
  private val hazardsKeys: Set[String] = Set(
    "classificationSummary", "hazardsNotOtherwiseClassified", "hazardPhrases",
    "signalWord", "pictograms")

  /** EU-CLP number implied by a section body ctor. */
  def sectionNumber(sec: Cst): Option[Int] = sec match
    case Cst.Node("euSection", List(Cst.Leaf(num), _)) => num.toIntOption
    case Cst.Node("identificationSection", _) => Some(1)
    case Cst.Node("hazardsSection", _) => Some(2)
    case _ => None

  private def isSectionBody(term: Cst): Boolean = term match
    case Cst.Node("euSection" | "identificationSection" | "hazardsSection", _) => true
    case _ => false

  private def localeRows(overlays: Cst): List[Cst] = overlays match
    case Cst.Node("none", _) => Nil
    case Cst.Node("some", List(Cst.Node("list", xs))) => xs
    case Cst.Node("list", xs) => xs
    case _ => Nil

  // ---- domain validation (ΔSDS = generic ΔL + these checks) ----

  def validate(m: Module): Either[String, Unit] =
    val errs = List.newBuilder[String]
    def defined(n: String) = m.get(n).isDefined
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
      case Cst.Node("basis", List(Cst.Leaf(target), Cst.Leaf(section))) =>
        if !defined(target) then errs += s"basis '$name' references unknown product '$target'"
        if section.isEmpty then errs += s"basis '$name' missing Law section number"
      case Cst.Node("euSection", List(Cst.Leaf(num), Cst.Node("list", fields))) =>
        num.toIntOption match
          case Some(n) if euClpNumbers.contains(n) => ()
          case Some(n) => errs += s"euSection '$name' number $n out of range (expected 1..16)"
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
        if pn.isEmpty then errs += s"identificationSection '$name': empty productName"
        if syn.isEmpty then errs += s"identificationSection '$name': empty synonyms"
        if use.isEmpty then errs += s"identificationSection '$name': empty recommendedUse"
        if against.isEmpty then errs += s"identificationSection '$name': empty usesAdvisedAgainst"
        if supplier.isEmpty then errs += s"identificationSection '$name': empty supplierName"
        if phone.isEmpty then errs += s"identificationSection '$name': empty emergencyPhone"
        validateLocales(name, "identificationSection", overlays, identificationKeys)
      case Cst.Node("hazardsSection", List(
          Cst.Leaf(cls), Cst.Leaf(hnoc), Cst.Leaf(phrases), Cst.Leaf(signal),
          Cst.Leaf(pictos), overlays)) =>
        if cls.isEmpty then errs += s"hazardsSection '$name': empty classificationSummary"
        if hnoc.isEmpty then errs += s"hazardsSection '$name': empty hazardsNotOtherwiseClassified"
        if phrases.isEmpty then errs += s"hazardsSection '$name': empty hazardPhrases"
        if signal.isEmpty then errs += s"hazardsSection '$name': empty signalWord"
        if pictos.isEmpty then errs += s"hazardsSection '$name': empty pictograms"
        validateLocales(name, "hazardsSection", overlays, hazardsKeys)
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
                  case Some(n) if euClpNumbers.contains(n) => nums += n
                  case Some(n) =>
                    errs += s"outline '$name' section '$ref' number $n out of range"
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
    case Cst.Node("identificationSection", List(
        Cst.Leaf(pn), Cst.Leaf(syn), Cst.Leaf(use), Cst.Leaf(against),
        Cst.Leaf(supplier), Cst.Leaf(phone), overlays)) =>
      val en = List(
        ("productName", "en", pn),
        ("synonyms", "en", syn),
        ("recommendedUse", "en", use),
        ("usesAdvisedAgainst", "en", against),
        ("supplierName", "en", supplier),
        ("emergencyPhone", "en", phone))
      val loc = localeRows(overlays).collect {
        case Cst.Node("fieldLocale", List(Cst.Leaf(k), Cst.Leaf(l), Cst.Leaf(t))) =>
          (k, l, t)
      }
      en ++ loc
    case Cst.Node("hazardsSection", List(
        Cst.Leaf(cls), Cst.Leaf(hnoc), Cst.Leaf(phrases), Cst.Leaf(signal),
        Cst.Leaf(pictos), overlays)) =>
      val en = List(
        ("classificationSummary", "en", cls),
        ("hazardsNotOtherwiseClassified", "en", hnoc),
        ("hazardPhrases", "en", phrases),
        ("signalWord", "en", signal),
        ("pictograms", "en", pictos))
      val loc = localeRows(overlays).collect {
        case Cst.Node("fieldLocale", List(Cst.Leaf(k), Cst.Leaf(l), Cst.Leaf(t))) =>
          (k, l, t)
      }
      en ++ loc
    case _ => Nil

  private def typedRefRows(section: Cst): List[(String, String, String)] = section match
    case Cst.Node("identificationSection" | "hazardsSection", kids) =>
      localeRows(kids.last).collect {
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
        val all = fields.collect {
          case Cst.Node("sectionField", List(Cst.Leaf(k), Cst.Leaf(l), Cst.Leaf(t)))
              if k == fieldKey =>
            (l, t)
        }
        all.collectFirst { case (l, t) if l == lang => t }
          .orElse(all.collectFirst { case (l, t) if l == "en" => t })
          .orElse(all.headOption.map(_._2))
      case Cst.Node("identificationSection" | "hazardsSection", _) =>
        val all = typedPlainRows(section).collect {
          case (k, l, t) if k == fieldKey => (l, t)
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
          if s == sectionRef && k == fieldKey =>
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
        case sec @ Cst.Node("identificationSection" | "hazardsSection", _) =>
          val refs = typedRefRows(sec).collect {
            case (k, l, ref) if k == fieldKey => (l, ref)
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
