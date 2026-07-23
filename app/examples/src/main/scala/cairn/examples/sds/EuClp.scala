package cairn.examples.sds

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.PackLoader
import cairn.systemhandler.EffectContext
import cairn.user.sds.Sds

/** EU-CLP / REACH Annex II regulatory profile pack (versioned language).
  *
  * Language: [[languages/eu-clp.cairn]] + surface. Instance module:
  * [[languages/sds/profiles/eu-clp-annex-ii.cairn]] (profile version `"1"`).
  * [[SectionNumbering]] prefers this module; host list is the fallback mirror.
  *
  * [[conform]] is a full profile judgment over an SDS module (outline +
  * section bodies + `sectionNumberOk`), not section numbering alone.
  */
object EuClp:
  private lazy val packs = PackLoader(EffectContext.forPackLoader())
  lazy val language: ComposedLanguage = packs.requireClosed("eu-clp")
  private lazy val sds = Sds(packs)

  lazy val annexIiModule: Module =
    ChemicalSource.euClpAnnexIi(language).fold(e => throw RuntimeException(e), identity)

  /** Canonical (number, title) rows from the annex-II v1 profile module. */
  def annexIiSections: List[(Int, String)] =
    val defs = annexIiModule.defs.collect {
      case (name, Cst.Node("sectionDef", List(Cst.Leaf(num), Cst.Leaf(title)))) =>
        num.toIntOption.map(n => (n, title, name))
    }.flatten
    val order = annexIiModule.get("euClpAnnexIi") match
      case Some(Cst.Node("profile", List(_, _, Cst.Node("list", refs)))) =>
        refs.collect { case Cst.Leaf(n) => n }
      case _ => defs.map(_._3)
    val byName = defs.map(d => d._3 -> (d._1, d._2)).toMap
    order.flatMap(byName.get)

  def checkSectionNumber(n: String): Boolean =
    val cfg = CheckerCfg(language.judgments.values.toList)
    val goal = Cst.node("sectionNumberOk", Cst.Leaf(n))
    Search.infer(cfg, goal).flatMap(d => Checker.check(cfg, d).left.map(_.render)).isRight

  def checkSectionTitle(n: String, title: String): Boolean =
    val cfg = CheckerCfg(language.judgments.values.toList)
    val goal = Cst.node("sectionTitleOk", Cst.Leaf(n), Cst.Leaf(title))
    Search.infer(cfg, goal).flatMap(d => Checker.check(cfg, d).left.map(_.render)).isRight

  def checkProfileVersion(v: String): Boolean =
    val cfg = CheckerCfg(language.judgments.values.toList)
    val goal = Cst.node("profileVersionOk", Cst.Leaf(v))
    Search.infer(cfg, goal).flatMap(d => Checker.check(cfg, d).left.map(_.render)).isRight

  final case class ConformanceReport(
      outlineName: String,
      sectionNumbers: List[Int],
      profileVersion: String,
      ok: Boolean,
      errors: List[String]
  )

  /** Full regulatory-profile conformance over an SDS module (host judgment
    * wired to Cairn `sectionNumberOk` / `sectionTitleOk` / `profileVersionOk`):
    *   - at least one `outline` with ascending unique section refs
    *   - each body resolves to an EU-CLP number accepted by `sectionNumberOk`
    *   - annex-II titles accepted by `sectionTitleOk` (not host-only compare)
    *   - SDS domain validate passes (`sectionNumberOk` + `translationStateTag`)
    *   - profile version accepted by `profileVersionOk`
    *
    * Residual Scala orchestration: outline walk / ascending-unique structural
    * checks. The *facts* are Cairn judgments in `eu-clp`. Not Studio.
    */
  def conform(m: Module, outlineName: Option[String] = None): ConformanceReport =
    val errs = List.newBuilder[String]
    val profileVersion = annexIiModule.get("euClpAnnexIi") match
      case Some(Cst.Node("profile", List(_, Cst.Leaf(v), _))) =>
        if !checkProfileVersion(v) then errs += s"profile version '$v' fails profileVersionOk"
        v
      case _ =>
        errs += "annex-II profile missing version"
        "?"
    val titleByNum = annexIiSections.toMap
    sds.validate(m) match
      case Left(e) => errs += s"SDS validate: $e"
      case Right(_) => ()
    val outlines = m.defs.collect {
      case (n, Cst.Node("outline", _)) => n
    }.toList.sorted
    val chosen = outlineName.orElse(outlines.headOption)
    val nums = chosen match
      case None =>
        errs += "no outline binding in SDS module"
        Nil
      case Some(on) =>
        m.get(on) match
          case Some(Cst.Node("outline", List(_, _, sectionsField))) =>
            val refs = sectionsField match
              case Cst.Node("none", _) => Nil
              case Cst.Node("some", List(Cst.Node("list", rs))) =>
                rs.collect { case Cst.Leaf(r) => r }
              case Cst.Node("list", rs) => rs.collect { case Cst.Leaf(r) => r }
              case other =>
                errs += s"outline '$on': bad sections ${other.render}"
                Nil
            if refs.isEmpty then errs += s"outline '$on' has no sections"
            val collected = List.newBuilder[Int]
            for ref <- refs do
              m.get(ref) match
                case None => errs += s"outline '$on' references unknown '$ref'"
                case Some(sec) =>
                  sds.sectionNumber(sec) match
                    case None =>
                      errs += s"'$ref' is not a section body"
                    case Some(n) =>
                      if !checkSectionNumber(n.toString) then
                        errs += s"section $n fails sectionNumberOk"
                      titleByNum.get(n) match
                        case None => errs += s"section $n not in annex-II profile"
                        case Some(title) =>
                          if !checkSectionTitle(n.toString, title) then
                            errs += s"section $n title fails sectionTitleOk: '$title'"
                          val expected = SectionNumbering.byNumber.get(n)
                          if expected.exists(_ != title) then
                            errs += s"profile title drift for $n: pack='$title' host='${expected.get}'"
                      collected += n
            val ns = collected.result()
            if ns != ns.sorted then
              errs += s"outline '$on' numbers not ascending: ${ns.mkString(",")}"
            if ns.distinct.sizeIs != ns.size then
              errs += s"outline '$on' has duplicate numbers"
            ns
          case Some(other) =>
            errs += s"'$on' is not an outline: ${other.render}"
            Nil
          case None =>
            errs += s"no outline '$on'"
            Nil
    val es = errs.result()
    ConformanceReport(
      outlineName = chosen.getOrElse(""),
      sectionNumbers = nums,
      profileVersion = profileVersion,
      ok = es.isEmpty,
      errors = es)
