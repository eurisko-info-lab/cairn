package cairn.user.law

import cairn.kernel.*
import cairn.systeminterface.PackAccess
import cairn.core.*

/** Law pack — middle link in `PKI → Law → SDS`.
  *
  * Object language: [[languages/law.cairn]] (`fragment statutes provides law requires cert`).
  * Closed composition pulls PKI's `cert` fragment; compose without PKI fails.
  * `enactedBy` cites a PKI certificate name as enacting authority.
  * Scala glue: citation judgment + ΔLaw gate + model act fixture.
  */
object Law:
  lazy val fragments: List[Fragment] = PackAccess.get.requireOwn("law")

  /** Own fragment only — compose fails: `requires cert` unmet without PKI. */
  def ownCompose: Either[List[ComposeError], ComposedLanguage] =
    Compose.compose("law", fragments)

  /** Closed language: Law + demoted PKI (cert). */
  lazy val language: ComposedLanguage = PackAccess.get.requireClosed("law")

  private val citation = """Section\s+(\S+)""".r

  /** Citation judgment: every "Section N" mention must resolve in the module. */
  def citationCheck(m: Module): List[String] =
    val byNumber = m.defs.collect {
      case (name, Cst.Node("section", List(Cst.Leaf(num), _, _))) => num -> name
    }.toMap
    val known = byNumber.keySet ++ m.defs.map(_._1).toSet
    val errs = List.newBuilder[String]
    for (name, term) <- m.defs do term match
      case Cst.Node("section", List(Cst.Leaf(num), _, Cst.Leaf(text))) =>
        for m <- citation.findAllMatchIn(text) do
          val cited = m.group(1).stripSuffix(".").stripSuffix(",")
          if !known.contains(cited) then
            errs += s"Section $num cites Section $cited, which does not exist"
      case Cst.Node("enactedBy", List(Cst.Leaf(statute), Cst.Leaf(cert))) =>
        if statute.isEmpty then errs += "enactedBy missing statute name"
        if cert.isEmpty then errs += "enactedBy missing PKI cert name"
      case _ => ()
    errs.result()

  def validate(m: Module): Either[String, Unit] =
    val cs = citationCheck(m)
    if cs.isEmpty then Right(()) else Left(cs.mkString("; "))

  def applyLaw(m: Module, change: Cst): Either[String, (Module, Delta.ValidatedChangeSet)] =
    Delta.apply(language, m, change).flatMap { out =>
      validate(out._1).map(_ => out) }

  /** Model Chemical Safety Act slice — citations + PKI enacting authority. */
  def modelAct: Module = Module(List(
    "s1" -> Cst.node("section", Cst.Leaf("1"), Cst.Leaf("Purpose"),
      Cst.Leaf("This Act governs chemical safety disclosure.")),
    "s2" -> Cst.node("section", Cst.Leaf("2"), Cst.Leaf("Definitions"),
      Cst.Leaf("Substance has the meaning in Section 1.")),
    "s3" -> Cst.node("section", Cst.Leaf("3"), Cst.Leaf("SDS duty"),
      Cst.Leaf("Publishers must maintain an SDS as defined in Section 2.")),
    "act" -> Cst.node("statute", Cst.Leaf("Model Chemical Safety Act"),
      Cst.Node("list", List(Cst.Leaf("s1"), Cst.Leaf("s2"), Cst.Leaf("s3")))),
    "authority" -> Cst.node("enactedBy", Cst.Leaf("act"), Cst.Leaf("root"))
  )).sorted
