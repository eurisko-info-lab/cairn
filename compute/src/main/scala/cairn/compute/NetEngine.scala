package cairn.compute

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

final case class Agent(id: Int, kind: String)

final case class Net(agents: Map[Int, Agent], wires: Set[(PortRef, PortRef)]):
  def canon: Canon = Canon.cmap(
    "agents" -> Canon.CList(agents.values.toList.sortBy(_.id).map(a =>
      Canon.cmap("id" -> Canon.CInt(a.id), "kind" -> Canon.CStr(a.kind)))),
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

/** How an interaction rule rewires: for a principal pair (L, R) of kinds
  * (lKind, rKind), the rule deletes both agents, creates `newAgents`, and
  * reconnects ports. External ports are addressed symbolically:
  *   Ext(0, i) = aux port i of the deleted left agent's former peer side
  *   Ext(1, i) = aux port i of the deleted right agent
  *   New(k, p) = port p of newly created agent k
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

  /** Find an active pair: two agents wired principal-to-principal. */
  def activePair(net: Net): Option[(Int, Int)] =
    net.wires.collectFirst {
      case (PortRef(a, 0), PortRef(b, 0)) => (a, b)
    }

  def step(lang: NetLanguage, net: Net): Either[String, Option[Net]] =
    activePair(net) match
      case None => Right(None)
      case Some((la, ra)) =>
        val lk = net.agents(la).kind
        val rk = net.agents(ra).kind
        val ruleOpt = lang.rules.find(r => r.left == lk && r.right == rk).map((_, la, ra))
          .orElse(lang.rules.find(r => r.left == rk && r.right == lk).map((_, ra, la)))
        ruleOpt match
          case None => Left(s"no interaction rule for principal pair ($lk, $rk)")
          case Some((rule, leftId, rightId)) =>
            var nextId = if net.agents.isEmpty then 0 else net.agents.keys.max + 1
            val created = rule.newAgents.map { kind =>
              val a = Agent(nextId, kind); nextId += 1; a }
            def resolve(rp: RulePort): PortRef = rp match
              case RulePort.New(k, p)   => PortRef(created(k).id, p)
              case RulePort.Ext(0, aux) => PortRef(leftId, aux + 1)
              case RulePort.Ext(_, aux) => PortRef(rightId, aux + 1)
            // Rewire: connections may point at Ext ports, meaning "whatever was
            // attached to that aux port of the deleted agent".
            def externalPeer(p: PortRef): Either[String, PortRef] =
              net.peer(p).toRight(s"rule '${rule.name}': external port $p unwired")
            val newWiresE: Either[String, List[(PortRef, PortRef)]] =
              rule.connections.foldLeft[Either[String, List[(PortRef, PortRef)]]](Right(Nil)) {
                case (acc, (x, y)) =>
                  acc.flatMap { ws =>
                    def endpoint(rp: RulePort): Either[String, PortRef] = rp match
                      case RulePort.New(_, _) => Right(resolve(rp))
                      case ext                => externalPeer(resolve(ext))
                    for a <- endpoint(x); b <- endpoint(y) yield ws :+ (a, b)
                  }
              }
            newWiresE.map { newWires =>
              val deadPorts = (p: PortRef) => p.agent == leftId || p.agent == rightId
              val keptWires = net.wires.filterNot((a, b) => deadPorts(a) || deadPorts(b))
              Some(Net(
                agents = net.agents - leftId - rightId ++ created.map(a => a.id -> a).toMap,
                wires = keptWires ++ newWires))
            }

  def normalize(lang: NetLanguage, net: Net, fuel: Int = 10_000): Either[String, Net] =
    if fuel <= 0 then Left("out of fuel reducing net")
    else step(lang, net).flatMap {
      case Some(n2) => normalize(lang, n2, fuel - 1)
      case None     => Right(net)
    }
