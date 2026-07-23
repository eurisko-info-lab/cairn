package cairn.core

/** Generic net builder (shared by packs). */
final class NetBuilder:
  private var next = 0
  private val agents = scala.collection.mutable.Map[Int, Agent]()
  private val wires = scala.collection.mutable.Set[(PortRef, PortRef)]()
  def agent(kind: String, label: String = ""): Int =
    val id = next; next += 1; agents(id) = Agent(id, kind, label); id
  def wire(a: PortRef, b: PortRef): Unit = wires += ((a, b))
  def net: Net = Net(agents.toMap, wires.toSet)
