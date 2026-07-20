package cairn.examples.affinenet

import cairn.kernel.*
import cairn.compute.*

/** AffineNet pack (S27, §5): a Δ-net language with Fan and Eraser ONLY.
  * Replicators are structurally absent — the kind table simply does not
  * contain one, so no replicating net is constructible (§6 Phase 3).
  *
  * Agent kinds:
  *  - fan   (γ, arity 2): doubles as λ-abstraction and application
  *  - era   (ε, arity 0): garbage collection
  *  - free  (arity 0): interface anchor — never interacts
  *  - konst (arity 0): opaque constant — never interacts on its own
  */
object AffineNet:
  import RulePort.*

  val language: NetLanguage = NetLanguage(
    name = "affine-net",
    kinds = List(
      AgentKind("fan", 2),
      AgentKind("era", 0),
      AgentKind("free", 0),
      AgentKind("konst", 0)),
    rules = List(
      // γγ annihilation: aux ports connect pairwise
      NetRule("fan-fan", "fan", "fan", Nil,
        List((Ext(0, 0), Ext(1, 0)), (Ext(0, 1), Ext(1, 1)))),
      // ε meets γ: erase both branches
      NetRule("era-fan", "era", "fan", List("era", "era"),
        List((New(0, 0), Ext(1, 0)), (New(1, 0), Ext(1, 1)))),
      NetRule("era-era", "era", "era", Nil, Nil),
      NetRule("era-konst", "era", "konst", Nil, Nil)))

  /** Net builder with fresh agent ids. */
  final class Builder:
    private var next = 0
    private val agents = scala.collection.mutable.Map[Int, Agent]()
    private val wires = scala.collection.mutable.Set[(PortRef, PortRef)]()
    def agent(kind: String): Int =
      val id = next; next += 1; agents(id) = Agent(id, kind); id
    def wire(a: PortRef, b: PortRef): Unit = wires += ((a, b))
    def net: Net = Net(agents.toMap, wires.toSet)

  /** Lowering (S29): affine λ-terms to nets. Terms use the STLC Cst shapes
    * (var/lam/app) plus Node("konst",[Leaf(c)]) constants. Each bound variable
    * must occur at most once (affine); unused binders get an eraser.
    * Returns the net plus the id of the root `free` agent.
    */
  def lower(term: Cst): Either[String, (Net, Int)] =
    val b = Builder()
    // env: variable name -> the port its (single) occurrence must attach to
    def go(t: Cst, env: Map[String, PortRef], out: PortRef, used: scala.collection.mutable.Set[String]): Either[String, Unit] =
      t match
        case Cst.Node("var", List(Cst.Leaf(x))) =>
          env.get(x) match
            case None => Left(s"unbound variable '$x'")
            case Some(port) =>
              if used.contains(x) then Left(s"variable '$x' used twice — not affine")
              else { used += x; b.wire(port, out); Right(()) }
        case Cst.Node("lam", List(Cst.Leaf(x), _, body)) =>
          val f = b.agent("fan")
          b.wire(PortRef(f, 0), out)
          val binderPort = PortRef(f, 1)
          val innerUsed = scala.collection.mutable.Set[String]()
          go(body, env + (x -> binderPort), PortRef(f, 2), innerUsed).map { _ =>
            if !innerUsed.contains(x) then
              val e = b.agent("era")
              b.wire(PortRef(e, 0), binderPort)
            used ++= innerUsed.diff(Set(x))
          }
        case Cst.Node("app", List(f, a)) =>
          val g = b.agent("fan")
          // application γ: principal at the function, aux1 = argument, aux2 = result
          for
            _ <- go(f, env, PortRef(g, 0), used)
            _ <- go(a, env, PortRef(g, 1), used)
          yield b.wire(PortRef(g, 2), out)
        case Cst.Node("konst", List(Cst.Leaf(_))) | Cst.Node("true", _) | Cst.Node("false", _) =>
          val k = b.agent("konst")
          b.wire(PortRef(k, 0), out)
          Right(())
        case other => Left(s"term does not lower to affine net: ${other.render}")
    val root = b.agent("free")
    go(term, Map.empty, PortRef(root, 0), scala.collection.mutable.Set()).map(_ => (b.net, root))
