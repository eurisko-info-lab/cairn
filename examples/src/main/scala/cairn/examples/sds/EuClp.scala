package cairn.examples.sds

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.PackLoader
import cairn.systemhandler.EffectContext

/** EU-CLP / REACH Annex II regulatory profile pack (versioned language).
  *
  * Language: [[languages/eu-clp.cairn]] + surface. Instance module:
  * [[languages/sds/profiles/eu-clp-annex-ii.cairn]] (profile version `"1"`).
  * [[SectionNumbering]] prefers this module; host list is the fallback mirror.
  */
object EuClp:
  private lazy val packs = PackLoader(EffectContext.forPackLoader())
  lazy val language: ComposedLanguage = packs.requireClosed("eu-clp")

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
