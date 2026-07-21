package cairn.examples.search

import cairn.kernel.*
import cairn.systeminterface.{Cas, PackAccess}
import cairn.systemhandler.{CasEffects, EffectContext}
import cairn.ledger.Provenance
import cairn.proof.{Certify, Certificate, Claim, TestCase, TestSuite}
import cairn.core.{Search as DerivSearch, Module, Delta}

/** Search pack — Fact–Intent–Hint board as a `.cairn` object language.
  *
  * Object language: [[languages/search.cairn]] (`fragment board provides search`,
  * standalone). Blackboard = CAS module + optional PoA publish. Workers / LLM /
  * dispatcher are intentionally out of scope here — host stubs only.
  *
  * Board edges (`supports` / `spawns`) are certified as Claims with test-suite
  * Certificates; provenance links Certificate ← Fact ← Intent so `cairn why`
  * walks the DAG. `wellFormed` / `goalMet` are real declarative judgments in
  * the language file (same generic kernel Checker as PKI's chain judgment) —
  * see [[checkWellFormed]] / [[checkGoalMet]]. The host [[wellFormed]] /
  * [[goalMet]] gates remain the whole-board checks (including `board(list)`
  * membership, which the fixed-arity rule DSL can't express) and back
  * [[certifyEdge]]'s existing test-suite certification, unchanged.
  */
final class Search(packs: PackAccess):
  lazy val fragments: List[Fragment] = packs.requireOwn("search")

  def ownCompose: Either[List[ComposeError], ComposedLanguage] =
    Compose.compose("search", fragments)

  lazy val language: ComposedLanguage = packs.requireClosed("search")

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

  /** The board as a checker context — mirrors `PkiMax.registryCtx`. */
  def boardCtx(m: Module): Cst =
    m.defs.foldRight(Cst.node("ctxNil")) { case ((name, term), acc) =>
      Cst.node("ctxCons", Cst.Leaf(name), term, acc) }

  def checkerCfg: CheckerCfg = CheckerCfg(language.judgments.values.toList)

  /** Kernel-checked wellFormed: an untrusted `DerivSearch.infer` proposes a
    * derivation for `term` against the board's context, `Checker.check`
    * certifies it — same two-step "propose, then certify" as
    * `PkiMax.validate`. Covers every board term EXCEPT `board(list)` (see
    * languages/search.cairn's judgment doc comment for why).
    */
  def checkWellFormed(m: Module, term: Cst): Either[String, Derivation] =
    val cfg = checkerCfg
    val goal = Cst.node("wellFormed", boardCtx(m), term)
    DerivSearch.infer(cfg, goal).flatMap { d => Checker.check(cfg, d).left.map(_.render).map(_ => d) }

  /** Kernel-checked goalMet for one concrete (goal, fact) witness pair. The
    * EXISTENTIAL "does some such pair exist" stays [[goalMet]]'s job (a host
    * scan) — `$ctx-lookup` requires a already-known name, not a search
    * target, so the judgment can only check a candidate, not find one.
    */
  def checkGoalMet(m: Module, goalName: String, factName: String): Either[String, Derivation] =
    val cfg = checkerCfg
    val goal = Cst.node("goalMet", boardCtx(m), Cst.Leaf(goalName), Cst.Leaf(factName))
    DerivSearch.infer(cfg, goal).flatMap { d => Checker.check(cfg, d).left.map(_.render).map(_ => d) }

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
      tool: String = "explore",
      ctx: EffectContext = EffectContext.forCas(),
  ): Digest =
    val art = Artifact(ArtifactKind.Term, Cst.toCanon(factTerm))
    val d = CasEffects.put(cas, art, ctx).fold(e => throw RuntimeException(e.toString), _.valueHash)
    Provenance.record(cas, d, List(intentDigest), tool, ctx)
      .fold(e => throw RuntimeException(e.toString), identity)
    d

  def putTerm(cas: Cas, term: Cst, ctx: EffectContext = EffectContext.forCas()): Digest =
    CasEffects.put(cas, Artifact(ArtifactKind.Term, Cst.toCanon(term)), ctx)
      .fold(e => throw RuntimeException(e.toString), _.valueHash)

  /** Evidence digests carried by a certified board edge. */
  final case class EdgeCert(
      claim: Claim,
      certificate: Certificate,
      claimDigest: Digest,
      certDigest: Digest,
      suiteDigest: Digest
  )

  /** Certify a `supports` / `spawns` edge: Claim + test-suite Certificate after
    * [[wellFormed]], then provenance Certificate ← Fact (and Fact already ← Intent).
    * Dangling endpoints fail [[wellFormed]] and yield `Left` — no certificate.
    */
  def certifyEdge(
      cas: Cas,
      board: Module,
      edgeName: String,
      factDigest: Digest,
      tool: String = "supports",
      ctx: EffectContext = EffectContext.forCas(),
  ): Either[String, EdgeCert] =
    wellFormed(board).flatMap { _ =>
      board.get(edgeName).toRight(s"no edge named '$edgeName'").flatMap {
        case Cst.Node(k @ ("supports" | "spawns"), List(Cst.Leaf(a), Cst.Leaf(b))) =>
          val claim = Claim(
            s"$k-$edgeName",
            Cst.node(k, Cst.Leaf(a), Cst.Leaf(b)),
            subject = board.digest)
          val suite = TestSuite(
            s"edge-$edgeName",
            board.digest,
            List(TestCase(
              "endpoints-known",
              Cst.node("check", Cst.Leaf(a), Cst.Leaf(b)),
              Cst.node("ok"))))
          val known = board.defs.map(_._1).toSet
          val eval = (t: Cst) => t match
            case Cst.Node("check", List(Cst.Leaf(x), Cst.Leaf(y))) =>
              if known.contains(x) && known.contains(y) then Right(Cst.node("ok"))
              else Left(s"dangling edge endpoint(s): $x, $y")
            case other => Left(s"unexpected edge check term: ${other.render}")
          Certify.byTests(claim, suite, eval).flatMap { cert =>
            for
              claimDig <- CasEffects.put(cas, claim.artifact, ctx).left.map(_.toString).map(_.valueHash)
              suiteDig <- CasEffects.put(cas, suite.artifact, ctx).left.map(_.toString).map(_.valueHash)
              certDig <- CasEffects.put(cas, cert.artifact, ctx).left.map(_.toString).map(_.valueHash)
              _ <- Provenance.record(cas, certDig, List(factDigest, claimDig, suiteDig), tool, ctx)
                .left.map(_.toString)
            yield EdgeCert(claim, cert, claimDig, certDig, suiteDig)
          }
        case other =>
          Left(s"'$edgeName' is not a supports/spawns edge: ${other.render}")
      }
    }

object Search:
  /** Pure fixture — no pack load. */
  def seedBoard: Module = Module(List(
    "start" -> Cst.node("origin", Cst.Leaf("unknown start state")),
    "target" -> Cst.node("goal", Cst.Leaf("reach a confirmed finding"))
  )).sorted
