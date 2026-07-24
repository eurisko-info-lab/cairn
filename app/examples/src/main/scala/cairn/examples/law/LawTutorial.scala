package cairn.examples.law

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.ModuleSource

/** Thin Law tutorial: load statute language (closed over PKI), load the
  * model act from `languages/law/acts/`, validate citations, repeal via free ΔLaw.
  */
object LawTutorial:
  final case class Report(
      languageProvides: Set[String],
      languageRequiresMet: Boolean,
      citationOk: Boolean,
      enactedByCert: String,
      repealDangling: Boolean
  )

  def run(): Report =
    val packs = cairn.runtime.PackLoader(cairn.runtime.EffectContexts.forPackLoader())
    val Law = cairn.examples.law.Law(packs)
    val Pki = cairn.examples.pki.Pki(packs)
    val lang = Law.language
    val provides = lang.fragments.flatMap(_.provides).toSet
    val requires = lang.fragments.flatMap(_.requires).toSet
    val met = requires.subsetOf(provides)
    val act = ModuleSource.modelChemicalSafetyAct(lang).fold(e => throw RuntimeException(e), identity)
    val citationOk = Law.validate(act).isRight
    val cert = act.get("authority") match
      case Some(Cst.Node("enactedBy", List(_, Cst.Leaf(c)))) => c
      case _ => ""
    // Repeal = free ΔL `remove` (dual of `add`). s1 is cited by later sections —
    // remove dependents first, then s1.
    val dl = Delta.deltaOf(lang).fold(e => throw RuntimeException(e.map(_.render).mkString), identity)
    val stripped = Module(act.defs.filterNot((n, _) => n == "s2" || n == "s3" || n == "act" || n == "authority"))
    val repeal = Parser.parse(dl.grammar, """{ remove s1 ; }""").fold(e => throw RuntimeException(e), identity)
    val after = Delta.apply(lang, stripped, repeal).fold(e => throw RuntimeException(e), _._1)
    val dangling = after.get("s1").isEmpty
    assert(lang.constructors.contains("cert"), "Law closed language must include PKI cert ctor")
    assert(Pki.language.constructors.contains("cert"))
    Report(provides, met, citationOk, cert, dangling)
