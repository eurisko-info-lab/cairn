package cairn.examples.sds
import cairn.runtime.EffectContexts

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.PackLoader
import cairn.user.sds.Sds

/** EU-CLP / REACH Annex II regulatory profile pack (versioned language).
  *
  * Language: [[languages/eu-clp.cairn]] + surface. Instance module:
  * [[languages/sds/profiles/eu-clp-annex-ii.cairn]] (profile version `"1"`).
  *
  * [[conform]] composes SDS module gate + pack judgments — not a second
  * SDS outline walk.
  */
object EuClp:
  private lazy val packs = PackLoader(EffectContexts.forPackLoader())
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

  private def cfg: CheckerCfg = CheckerCfg(language.judgments.values.toList)

  def checkSectionNumber(n: String): Boolean =
    Search.prove(cfg, Cst.node("sectionNumberOk", Cst.Leaf(n))).isRight

  def checkSectionTitle(n: String, title: String): Boolean =
    Search.prove(cfg, Cst.node("sectionTitleOk", Cst.Leaf(n), Cst.Leaf(title))).isRight

  def checkProfileVersion(v: String): Boolean =
    Search.prove(cfg, Cst.node("profileVersionOk", Cst.Leaf(v))).isRight

  final case class ConformanceReport(
      outlineName: String,
      sectionNumbers: List[Int],
      profileVersion: String,
      ok: Boolean,
      errors: List[String]
  )

  /** Profile conformance: SDS gate + profileVersionOk + sectionTitleOk per outline body. */
  def conform(m: Module, outlineName: Option[String] = None): ConformanceReport =
    val errs = List.newBuilder[String]
    val profileVersion = annexIiModule.get("euClpAnnexIi") match
      case Some(Cst.Node("profile", List(_, Cst.Leaf(v), _))) =>
        if !checkProfileVersion(v) then errs += s"profile version '$v' fails profileVersionOk"
        v
      case _ =>
        errs += "annex-II profile missing version"
        "?"
    ModuleGate.require(ModuleGate.fromJudgment("sds.validate")(sds.validate), m) match
      case Left(e) => errs += e
      case Right(_) => ()
    val titleByNum = annexIiSections.toMap
    val outlines = m.defs.collect { case (n, Cst.Node("outline", _)) => n }.toList.sorted
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
            val collected = List.newBuilder[Int]
            for ref <- refs do
              m.get(ref).flatMap(sds.sectionNumber) match
                case Some(n) =>
                  titleByNum.get(n) match
                    case Some(title) if !checkSectionTitle(n.toString, title) =>
                      errs += s"section $n title fails sectionTitleOk: '$title'"
                    case None =>
                      errs += s"section $n not in annex-II profile"
                    case _ => ()
                  collected += n
                case None =>
                  errs += s"outline '$on' references non-section '$ref'"
            collected.result()
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
