package cairn.examples.search

import cairn.kernel.*
import cairn.workbench.*
import cairn.ledger.Provenance
import cairn.proof.Claim

/** Search pack — Fact–Intent–Hint board as a `.cairn` object language.
  *
  * Object language: [[languages/search.cairn]] (`fragment board provides search`,
  * standalone). Blackboard = CAS module + optional PoA publish. Workers / LLM /
  * dispatcher are intentionally out of scope here — host stubs only.
  */
object Search:
  lazy val fragments: List[Fragment] = PackLoader.requireOwn("search")

  def ownCompose: Either[List[ComposeError], ComposedLanguage] =
    Compose.compose("search", fragments)

  lazy val language: ComposedLanguage = PackLoader.requireClosed("search")

  /** Seed board: origin Fact + goal Intent. */
  def seedBoard: Module = Module(List(
    "start" -> Cst.node("origin", Cst.Leaf("unknown start state")),
    "target" -> Cst.node("goal", Cst.Leaf("reach a confirmed finding"))
  )).sorted

  /** Domain gate: edge endpoints and board members must resolve. */
  def wellFormed(m: Module): Either[String, Unit] =
    val known = m.defs.map(_._1).toSet
    val errs = List.newBuilder[String]
    for (name, term) <- m.defs do term match
      case Cst.Node("supports", List(Cst.Leaf(a), Cst.Leaf(b))) =>
        if !known.contains(a) then errs += s"supports '$name' references unknown '$a'"
        if !known.contains(b) then errs += s"supports '$name' references unknown '$b'"
      case Cst.Node("spawns", List(Cst.Leaf(a), Cst.Leaf(b))) =>
        if !known.contains(a) then errs += s"spawns '$name' references unknown '$a'"
        if !known.contains(b) then errs += s"spawns '$name' references unknown '$b'"
      case Cst.Node("board", List(Cst.Node("list", ns))) =>
        for n <- ns do n match
          case Cst.Leaf(ref) if !known.contains(ref) =>
            errs += s"board '$name' references unknown '$ref'"
          case _ => ()
      case Cst.Node("origin" | "goal" | "fact" | "intent" | "hint", List(Cst.Leaf(t))) =>
        if t.isEmpty then errs += s"'$name' has empty text"
      case other =>
        errs += s"'$name' is not a search board term: ${other.render}"
    val es = errs.result()
    if es.isEmpty then Right(()) else Left(es.mkString("; "))

  /** Stub goalMet: true when at least one `fact` exists alongside a `goal`. */
  def goalMet(m: Module): Boolean =
    val hasGoal = m.defs.exists((_, t) => t match
      case Cst.Node("goal", _) => true
      case _ => false)
    val hasFact = m.defs.exists((_, t) => t match
      case Cst.Node("fact", _) => true
      case _ => false)
    hasGoal && hasFact

  def applySearch(m: Module, change: Cst): Either[String, (Module, Delta.ValidatedChangeSet)] =
    Delta.apply(language, m, change).flatMap { out =>
      wellFormed(out._1).map(_ => out) }

  /** Proof-free claim that the board's goal is met (§2 Claim vs Theorem). */
  def goalMetClaim(m: Module): Claim =
    Claim("goalMet", Cst.node("goalMet", Cst.Leaf(if goalMet(m) then "true" else "false")),
      subject = m.digest)

  final case class GraphNode(name: String, kind: String, text: String)
  final case class GraphEdge(name: String, kind: String, from: String, to: String)
  final case class BoardGraph(nodes: List[GraphNode], edges: List[GraphEdge])

  /** Extract a Fact–Intent–Hint graph from a board module. */
  def graphOf(m: Module): BoardGraph =
    val nodes = List.newBuilder[GraphNode]
    val edges = List.newBuilder[GraphEdge]
    for (name, term) <- m.defs do term match
      case Cst.Node(k @ ("origin" | "fact"), List(Cst.Leaf(t))) =>
        nodes += GraphNode(name, k, t)
      case Cst.Node(k @ ("goal" | "intent"), List(Cst.Leaf(t))) =>
        nodes += GraphNode(name, k, t)
      case Cst.Node("hint", List(Cst.Leaf(t))) =>
        nodes += GraphNode(name, "hint", t)
      case Cst.Node(k @ ("supports" | "spawns"), List(Cst.Leaf(a), Cst.Leaf(b))) =>
        edges += GraphEdge(name, k, a, b)
      case Cst.Node("board", _) => ()
      case _ => ()
    BoardGraph(nodes.result(), edges.result())

  /** Put a Fact term in CAS with provenance hop Intent → Fact (`cairn why`). */
  def putFact(
      cas: Cas,
      factTerm: Cst,
      intentDigest: Digest,
      tool: String = "explore"
  ): Digest =
    val art = Artifact(ArtifactKind.Term, Cst.toCanon(factTerm))
    val d = cas.put(art).valueHash
    Provenance.record(cas, d, List(intentDigest), tool)
    d

  def putTerm(cas: Cas, term: Cst): Digest =
    cas.put(Artifact(ArtifactKind.Term, Cst.toCanon(term))).valueHash
