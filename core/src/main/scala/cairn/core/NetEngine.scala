package cairn.core

import cairn.kernel.*

/** Interaction-net computation backend (S25–S28, §2 Δ-net).
  *
  * Graph-mode data: agents with one principal port and n auxiliary ports,
  * wires connecting ports. Interaction rules fire on principal–principal
  * pairs. The engine is generic: rules are data supplied by a language pack;
  * no agent kind is privileged here.
  */
final case class AgentKind(name: String, arity: Int) // arity = number of aux ports

/** Port address: agent id + port index (0 = principal, 1..arity = aux). */
final case class PortRef(agent: Int, port: Int)

final case class Agent(id: Int, kind: String, label: String = "")

final case class Net(agents: Map[Int, Agent], wires: Set[(PortRef, PortRef)]):
  def canon: Canon = Canon.cmap(
    "agents" -> Canon.CList(agents.values.toList.sortBy(_.id).map(a =>
      Canon.cmap("id" -> Canon.CInt(a.id), "kind" -> Canon.CStr(a.kind), "label" -> Canon.CStr(a.label)))),
    "wires" -> Canon.CList(wires.toList.map(normWire).sortBy(_.toString).map((a, b) =>
      Canon.cmap(
        "a" -> Canon.cmap("agent" -> Canon.CInt(a.agent), "port" -> Canon.CInt(a.port)),
        "b" -> Canon.cmap("agent" -> Canon.CInt(b.agent), "port" -> Canon.CInt(b.port))))))
  def artifact: Artifact = Artifact(ArtifactKind.Net, canon)
  private def normWire(w: (PortRef, PortRef)): (PortRef, PortRef) =
    val (a, b) = w
    if (a.agent, a.port).toString <= (b.agent, b.port).toString then (a, b) else (b, a)

  def peer(p: PortRef): Option[PortRef] =
    wires.collectFirst {
      case (a, b) if a == p => b
      case (a, b) if b == p => a
    }

/** How an interaction rule rewires (see [[NetEngine.step]]).
  * `newAgents` entries may be `"@left"` / `"@right"`, meaning "a copy of the
  * consumed left/right agent (kind AND label)" — how duplication rules copy
  * labelled constants without a rule per label (M25).
  */
enum RulePort:
  case Ext(side: Int, aux: Int)
  case New(agentIndex: Int, port: Int)

final case class NetRule(
    name: String,
    left: String, right: String, // agent kinds of the principal pair
    newAgents: List[String],
    connections: List[(RulePort, RulePort)],
)

final case class NetLanguage(name: String, kinds: List[AgentKind], rules: List[NetRule]):
  def kindOf(n: String): Option[AgentKind] = kinds.find(_.name == n)

object NetEngine:
  /** Well-formedness judgment (S28): every port of every agent is wired
    * exactly once (linearity), kinds are declared, arities respected.
    */
  def wellFormed(lang: NetLanguage, net: Net): Either[List[String], Unit] =
    var errs = List.newBuilder[String]
    val portUse = scala.collection.mutable.Map[PortRef, Int]().withDefaultValue(0)
    for (a, b) <- net.wires; p <- List(a, b) do portUse(p) += 1
    for a <- net.agents.values do
      lang.kindOf(a.kind) match
        case None => errs += s"agent ${a.id}: undeclared kind '${a.kind}'"
        case Some(k) =>
          for p <- 0 to k.arity do
            portUse(PortRef(a.id, p)) match
              case 1 => ()
              case 0 => errs += s"agent ${a.id} (${a.kind}): port $p unwired"
              case n => errs += s"agent ${a.id} (${a.kind}): port $p wired $n times (linearity violation)"
    for (a, b) <- net.wires; p <- List(a, b) do
      net.agents.get(p.agent) match
        case None => errs += s"wire references missing agent ${p.agent}"
        case Some(ag) =>
          lang.kindOf(ag.kind).foreach { k =>
            if p.port > k.arity then errs += s"agent ${p.agent} (${ag.kind}): port ${p.port} exceeds arity ${k.arity}" }
    val es = errs.result()
    if es.isEmpty then Right(()) else Left(es)

  /** Find active pairs (principal-to-principal wires). Pairs without rules
    * (interface/inert agents) are part of the normal form.
    */
  def activePairs(net: Net): List[(Int, Int)] =
    net.wires.toList.collect { case (PortRef(a, 0), PortRef(b, 0)) => (a, b) }.sorted

  def ruleFor(lang: NetLanguage, net: Net, pair: (Int, Int)): Option[(NetRule, Int, Int)] =
    val (la, ra) = pair
    val lk = net.agents(la).kind
    val rk = net.agents(ra).kind
    lang.rules.find(r => r.left == lk && r.right == rk).map((_, la, ra))
      .orElse(lang.rules.find(r => r.left == rk && r.right == lk).map((_, ra, la)))

  /** Apply one specific rule instance at a principal pair (port fusion). */
  def applyPair(net: Net, rule: NetRule, leftId: Int, rightId: Int): Either[String, Net] =
    var nextId = if net.agents.isEmpty then 0 else net.agents.keys.max + 1
    val created = rule.newAgents.map { spec =>
      val a = spec match
        case "@left"  => Agent(nextId, net.agents(leftId).kind, net.agents(leftId).label)
        case "@right" => Agent(nextId, net.agents(rightId).kind, net.agents(rightId).label)
        case kind     => Agent(nextId, kind)
      nextId += 1; a }
    def resolve(rp: RulePort): PortRef = rp match
      case RulePort.New(k, p)   => PortRef(created(k).id, p)
      case RulePort.Ext(0, aux) => PortRef(leftId, aux + 1)
      case RulePort.Ext(_, aux) => PortRef(rightId, aux + 1)
    val isDead = (p: PortRef) => p.agent == leftId || p.agent == rightId
    val activeWire = (a: PortRef, b: PortRef) =>
      Set(a, b) == Set(PortRef(leftId, 0), PortRef(rightId, 0))
    var wires: List[(PortRef, PortRef)] =
      net.wires.toList.filterNot(activeWire.tupled) ++
        rule.connections.map((x, y) => (resolve(x), resolve(y)))
    var guard = wires.length * 4 + 8
    def deadIn(w: (PortRef, PortRef)): Option[PortRef] =
      if isDead(w._1) then Some(w._1) else if isDead(w._2) then Some(w._2) else None
    while wires.exists(w => deadIn(w).isDefined) && guard > 0 do
      guard -= 1
      val w = wires.find(w => deadIn(w).isDefined).get
      val p = deadIn(w).get
      if w._1 == p && w._2 == p then
        wires = wires.filterNot(_ == w)
      else
        val liveEndOfW = if w._1 == p then w._2 else w._1
        val rest = wires.diff(List(w))
        rest.find(w2 => w2._1 == p || w2._2 == p) match
          case Some(w2) =>
            val otherEnd = if w2._1 == p then w2._2 else w2._1
            wires = rest.diff(List(w2)) :+ (liveEndOfW, otherEnd)
          case None =>
            return Left(s"rule '${rule.name}': dead port $p has no fusion partner (non-linear rule)")
    if guard <= 0 then Left(s"rule '${rule.name}': port fusion did not terminate")
    else
      Right(Net(
        agents = net.agents - leftId - rightId ++ created.map(a => a.id -> a).toMap,
        wires = wires.toSet))

  def step(lang: NetLanguage, net: Net): Either[String, Option[Net]] =
    activePairs(net).flatMap(ruleFor(lang, net, _)).headOption match
      case None => Right(None)
      case Some((rule, l, r)) => applyPair(net, rule, l, r).map(Some(_))

  /** M27: simultaneous reduction of all independent active pairs. Confluence
    * of interaction nets makes the result order-independent; agent-disjoint
    * pairs applied in one sweep are semantically simultaneous.
    */
  final case class ParallelStats(sweeps: Int, pairsPerSweep: List[Int])

  def parallelStep(lang: NetLanguage, net: Net): Either[String, Option[(Net, Int)]] =
    val candidates = activePairs(net).flatMap(ruleFor(lang, net, _))
    if candidates.isEmpty then Right(None)
    else
      // greedy agent-disjoint selection
      var used = Set.empty[Int]
      val selected = candidates.filter { (_, l, r) =>
        if used.contains(l) || used.contains(r) then false
        else { used += l; used += r; true } }
      selected.foldLeft[Either[String, Net]](Right(net)) { case (acc, (rule, l, r)) =>
        acc.flatMap(applyPair(_, rule, l, r))
      }.map(n2 => Some((n2, selected.length)))

  def normalizeParallel(lang: NetLanguage, net: Net, fuel: Int = 10_000): Either[String, (Net, ParallelStats)] =
    def loop(n: Net, sweeps: Int, pairs: List[Int], fuel: Int): Either[String, (Net, ParallelStats)] =
      if fuel <= 0 then Left("out of fuel reducing net")
      else parallelStep(lang, n).flatMap {
        case Some((n2, k)) => loop(n2, sweeps + 1, pairs :+ k, fuel - 1)
        case None          => Right((n, ParallelStats(sweeps, pairs)))
      }
    loop(net, 0, Nil, fuel)

  def normalize(lang: NetLanguage, net: Net, fuel: Int = 10_000): Either[String, Net] =
    if fuel <= 0 then Left("out of fuel reducing net")
    else step(lang, net).flatMap {
      case Some(n2) => normalize(lang, n2, fuel - 1)
      case None     => Right(net)
    }
