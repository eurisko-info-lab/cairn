package cairn.examples.icnet

import cairn.kernel.*
import cairn.compute.*

/** IcNet (M25): FULL interaction combinators — γ (fan), δ (duplicator),
  * ε (eraser) with the classic annihilation/commutation table, plus inert
  * `free` (interface) and `konst` (labelled constant) agents.
  * AffineNet remains the replicator-free fragment; this pack is where
  * duplication lives.
  */
object IcNet:
  import RulePort.*

  /** γγ and δδ annihilate; γδ commute; ε erases everything; δ copies konst. */
  val language: NetLanguage = NetLanguage(
    name = "ic-net",
    kinds = List(
      AgentKind("fan", 2),   // γ
      AgentKind("dup", 2),   // δ
      AgentKind("era", 0),   // ε
      AgentKind("free", 0),
      AgentKind("konst", 0)),
    rules = List(
      // annihilations: aux ports connect pairwise
      NetRule("fan-fan", "fan", "fan", Nil,
        List((Ext(0, 0), Ext(1, 0)), (Ext(0, 1), Ext(1, 1)))),
      NetRule("dup-dup", "dup", "dup", Nil,
        List((Ext(0, 0), Ext(1, 0)), (Ext(0, 1), Ext(1, 1)))),
      // γδ commutation: each agent is duplicated past the other.
      // new agents: 0,1 = δ copies facing γ's former peers; 2,3 = γ copies
      // facing δ's former peers; aux ports cross-wired.
      NetRule("fan-dup", "fan", "dup", List("dup", "dup", "fan", "fan"),
        List(
          (New(0, 0), Ext(0, 0)), (New(1, 0), Ext(0, 1)),
          (New(2, 0), Ext(1, 0)), (New(3, 0), Ext(1, 1)),
          (New(0, 1), New(2, 1)), (New(0, 2), New(3, 1)),
          (New(1, 1), New(2, 2)), (New(1, 2), New(3, 2)))),
      // erasure
      NetRule("era-fan", "era", "fan", List("era", "era"),
        List((New(0, 0), Ext(1, 0)), (New(1, 0), Ext(1, 1)))),
      NetRule("era-dup", "era", "dup", List("era", "era"),
        List((New(0, 0), Ext(1, 0)), (New(1, 0), Ext(1, 1)))),
      NetRule("era-era", "era", "era", Nil, Nil),
      NetRule("era-konst", "era", "konst", Nil, Nil),
      // δ copies constants: two fresh copies of the SAME labelled agent
      NetRule("dup-konst", "dup", "konst", List("@right", "@right"),
        List((New(0, 0), Ext(0, 0)), (New(1, 0), Ext(0, 1))))))

  // ---- M26: general λ → net lowering (δ for shared variables) + readback ----

  /** Lower a full STLC λ-term (var/lam/app + boolean constants; `if` stays on
    * the tree engine). Variables used n ≥ 2 times get a δ-tree; unused binders
    * get ε. Returns the net and the root `free` agent id.
    */
  def lower(term: Cst): Either[String, (Net, Int)] =
    val b = NetBuilder()
    def countUses(t: Cst, x: String): Int = t match
      case Cst.Node("var", List(Cst.Leaf(y))) => if x == y then 1 else 0
      case Cst.Node("lam", List(Cst.Leaf(y), _, body)) => if x == y then 0 else countUses(body, x)
      case Cst.Node(_, cs) => cs.map(countUses(_, x)).sum
      case _ => 0
    // env: variable -> a fresh port for its NEXT occurrence (fed by a δ tree)
    def go(t: Cst, env: Map[String, () => PortRef], out: PortRef): Either[String, Unit] =
      t match
        case Cst.Node("var", List(Cst.Leaf(x))) =>
          env.get(x) match
            case None => Left(s"unbound variable '$x'")
            case Some(nextPort) => b.wire(nextPort(), out); Right(())
        case Cst.Node("lam", List(Cst.Leaf(x), _, body)) =>
          val f = b.agent("fan")
          b.wire(PortRef(f, 0), out)
          val binderPort = PortRef(f, 1)
          val uses = countUses(body, x)
          if uses == 0 then
            val e = b.agent("era")
            b.wire(PortRef(e, 0), binderPort)
            go(body, env - x, PortRef(f, 2))
          else if uses == 1 then
            var consumed = false
            go(body, env + (x -> (() => { consumed = true; binderPort })), PortRef(f, 2))
          else
            // δ-tree: uses-1 duplicators chained off the binder port
            val ports = scala.collection.mutable.Queue[PortRef]()
            var feed = binderPort
            for i <- 1 until uses do
              val d = b.agent("dup")
              b.wire(PortRef(d, 0), feed)
              ports += PortRef(d, 1)
              feed = PortRef(d, 2)
            ports += feed
            go(body, env + (x -> (() => ports.dequeue())), PortRef(f, 2))
        case Cst.Node("app", List(f, a)) =>
          val g = b.agent("fan")
          for
            _ <- go(f, env, PortRef(g, 0))
            _ <- go(a, env, PortRef(g, 1))
          yield b.wire(PortRef(g, 2), out)
        case Cst.Node("true", _)  => val k = b.agent("konst", "true"); b.wire(PortRef(k, 0), out); Right(())
        case Cst.Node("false", _) => val k = b.agent("konst", "false"); b.wire(PortRef(k, 0), out); Right(())
        case other => Left(s"term does not lower: ${other.render}")
    val root = b.agent("free")
    go(term, Map.empty, PortRef(root, 0)).map(_ => (b.net, root))

  /** Readback (M26): decode a normal-form net into a λ-term. Fans reached via
    * their PRINCIPAL port are lambdas; via aux2, applications-in-normal-form
    * do not occur (they would be active); konst agents are constants; wires to
    * binder ports are variables. Fails honestly on undecodable shapes.
    */
  def readback(net: Net, root: Int): Either[String, Cst] =
    var fresh = 0
    def go(port: PortRef, binders: Map[PortRef, String]): Either[String, Cst] =
      net.peer(port) match
        case None => Left(s"dangling wire at $port")
        case Some(p) =>
          binders.get(p) match
            case Some(x) => Right(Cst.node("var", Cst.Leaf(x)))
            case None =>
              val agent = net.agents.get(p.agent).toRight(s"missing agent ${p.agent}").fold(e => return Left(e), identity)
              (agent.kind, p.port) match
                case ("konst", 0) => Right(Cst.node(agent.label))
                case ("fan", 0) =>
                  fresh += 1
                  val x = s"r$fresh"
                  val binderPort = PortRef(agent.id, 1)
                  go(PortRef(agent.id, 2), binders + (binderPort -> x))
                    .map(body => Cst.node("lam", Cst.Leaf(x), Cst.node("tyBool"), body))
                case (kind, portIdx) => Left(s"cannot read back $kind agent via port $portIdx")
    go(PortRef(root, 0), Map.empty)
