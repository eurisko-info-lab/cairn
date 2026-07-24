package cairn.user.law

import cairn.kernel.*
import cairn.systeminterface.PackAccess
import cairn.core.*

/** Law pack — middle link in PKI to Law to SDS.
  *
  * Object language: languages/law.cairn (fragment statutes provides law requires cert).
  * Closed composition pulls PKI's cert fragment; compose without PKI fails.
  * enactedBy cites a PKI certificate name as enacting authority.
  *
  * Statute fixtures live under languages/law/acts/ (load via ModuleSource).
  * Citation validity uses pack judgments (`citeOk` / `enactedByOk`) via Search.prove.
  * Host only indexes section numbers → binding names (checker cannot invent names).
  */
final class Law(packs: PackAccess):
  lazy val fragments: List[Fragment] = packs.requireOwn("law")

  def ownCompose: Either[List[ComposeError], ComposedLanguage] =
    Compose.compose("law", fragments)

  lazy val language: ComposedLanguage = packs.requireClosed("law")

  private def cfg: CheckerCfg = CheckerCfg(language.judgments.values.toList)

  def moduleCtx(m: Module): Cst =
    m.defs.foldRight(Cst.node("ctxNil")) { case ((name, term), acc) =>
      Cst.node("ctxCons", Cst.Leaf(name), term, acc)
    }

  private def sectionByNumber(m: Module): Map[String, String] =
    m.defs.collect {
      case (name, Cst.Node("section", List(Cst.Leaf(num), _, _))) => num -> name
    }.toMap

  /** Prove pack judgments for every `cites` / `enactedBy` term. */
  def judgmentCheck(m: Module): List[String] =
    val ctx = moduleCtx(m)
    val byNum = sectionByNumber(m)
    val errs = List.newBuilder[String]
    for (_, term) <- m.defs do term match
      case c @ Cst.Node("cites", List(Cst.Leaf(from), Cst.Leaf(to))) =>
        (byNum.get(from), byNum.get(to)) match
          case (Some(fromName), Some(toName)) =>
            val goal = Cst.node(
              "citeOk", ctx, Cst.Leaf(fromName), Cst.Leaf(toName), c)
            Search.prove(cfg, goal) match
              case Left(e) => errs += s"citeOk failed for ${c.render}: $e"
              case Right(_) => ()
          case (None, _) =>
            errs += s"cites ${c.render}: source section $from does not exist"
          case (_, None) =>
            errs += s"cites ${c.render}: target section $to does not exist"
      case c @ Cst.Node("enactedBy", _) =>
        Search.prove(cfg, Cst.node("enactedByOk", ctx, c)) match
          case Left(e) => errs += s"enactedByOk failed for ${c.render}: $e"
          case Right(_) => ()
      case _ => ()
    errs.result()

  private val citation = """Section\s+(\S+)""".r

  /** Migration aid: free-text "Section N" must have a matching structured `cites`. */
  def freeTextCiteCheck(m: Module): List[String] =
    val sectionNums = sectionByNumber(m).keySet
    val structured = m.defs.collect {
      case (_, Cst.Node("cites", List(Cst.Leaf(from), Cst.Leaf(to)))) => (from, to)
    }.toSet
    val errs = List.newBuilder[String]
    for (_, term) <- m.defs do term match
      case Cst.Node("section", List(Cst.Leaf(num), _, Cst.Leaf(text))) =>
        for mt <- citation.findAllMatchIn(text) do
          val cited = mt.group(1).stripSuffix(".").stripSuffix(",")
          if !sectionNums.contains(cited) then
            errs += s"Section $num cites Section $cited, which does not exist"
          else if !structured.contains((num, cited)) then
            errs += s"Section $num free-text cites $cited without structured cites term"
      case _ => ()
    errs.result()

  def citationCheck(m: Module): List[String] =
    judgmentCheck(m) ++ freeTextCiteCheck(m)

  def validate(m: Module): Either[String, Unit] =
    val cs = citationCheck(m)
    if cs.isEmpty then Right(()) else Left(cs.mkString("; "))

  def applyLaw(m: Module, change: Cst): Either[String, (Module, Delta.ValidatedChangeSet)] =
    Delta.apply(language, m, change).flatMap { out =>
      validate(out._1).map(_ => out) }
