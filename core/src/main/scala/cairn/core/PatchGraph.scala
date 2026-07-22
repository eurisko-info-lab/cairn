package cairn.core

import cairn.kernel.*

/** Explicit causal patch DAG — patch nodes with parent dependency edges,
  * beyond ordered digest lists alone.
  *
  * Thin advance toward Pijul-style patch theory: nodes are ValidatedChangeSet
  * digests; edges are causal parents (linear history or multi-parent merge).
  * [[lca]] is a DAG least-common-ancestor over explicit edges. Full Pijul
  * commutation / inverse / conflict algebra remains on ChangeAlgebra / Merge;
  * this graph is the causal substrate those engines can consult.
  */
object PatchGraph:

  final case class Node(
      id: Digest,
      parents: List[Digest],
      base: Digest,
      result: Digest,
  ):
    def canon: Canon = Canon.cmap(
      "id" -> Canon.CStr(id.hex),
      "parents" -> Canon.cstrs(parents.map(_.hex)),
      "base" -> Canon.CStr(base.hex),
      "result" -> Canon.CStr(result.hex))

  object Node:
    def fromCanon(c: Canon): Node =
      Node(
        Digest(c.field("id").asStr),
        c.field("parents").asList.map(x => Digest(x.asStr)),
        Digest(c.field("base").asStr),
        Digest(c.field("result").asStr))

  final case class Graph(nodes: Map[Digest, Node] = Map.empty):
    def contains(id: Digest): Boolean = nodes.contains(id)

    def get(id: Digest): Option[Node] = nodes.get(id)

    /** Add a node; parents must already exist (or be empty). Rejects cycles. */
    def add(n: Node): Either[String, Graph] =
      if nodes.contains(n.id) then Left(s"patch ${n.id.short} already present")
      else
        val missing = n.parents.filterNot(p => nodes.contains(p))
        if missing.nonEmpty then
          Left(s"patch ${n.id.short} missing parents ${missing.map(_.short).mkString(",")}")
        else
          val next = copy(nodes = nodes + (n.id -> n))
          if next.wouldCycle(n.id) then Left(s"patch ${n.id.short} introduces a cycle")
          else Right(next)

    private def wouldCycle(id: Digest): Boolean =
      def reach(from: Digest, seen: Set[Digest]): Boolean =
        if seen.contains(from) then true
        else nodes.get(from).exists(_.parents.exists(p => reach(p, seen + from)))
      // cycle if id is reachable from itself via parents (id ∈ ancestors(id))
      nodes.get(id).exists(_.parents.exists(p => ancestors(p).contains(id) || p == id))

    /** All ancestor ids of `id` (not including `id`). */
    def ancestors(id: Digest): Set[Digest] =
      def go(cur: Digest, acc: Set[Digest]): Set[Digest] =
        nodes.get(cur) match
          case None => acc
          case Some(n) =>
            n.parents.foldLeft(acc) { (a, p) =>
              if a.contains(p) then a else go(p, a + p)
            }
      go(id, Set.empty)

    def withSelf(id: Digest): Set[Digest] = ancestors(id) + id

    /** Deterministic LCA: common ancestors (incl. self) with maximum depth;
      * tie-break by hex. Empty intersection ⇒ None.
      */
    def lca(a: Digest, b: Digest): Option[Digest] =
      val common = withSelf(a).intersect(withSelf(b))
      if common.isEmpty then None
      else
        def depth(id: Digest): Int = ancestors(id).size
        Some(common.maxBy(id => (depth(id), id.hex)))

    def canon: Canon =
      Canon.CTag(
        "patch-graph",
        Canon.cmap(
          "nodes" -> Canon.CList(
            nodes.values.toList.sortBy(_.id.hex).map(_.canon))))

    def artifact: Artifact = Artifact(ArtifactKind.Certificate, canon)

  object Graph:
    def empty: Graph = Graph()

    def fromCanon(c: Canon): Either[String, Graph] =
      c match
        case Canon.CTag("patch-graph", body) =>
          try
            val ns = body.field("nodes").asList.map(Node.fromCanon)
            def loop(remaining: List[Node], g: Graph): Either[String, Graph] =
              if remaining.isEmpty then Right(g)
              else
                val (ready, blocked) = remaining.partition(n => n.parents.forall(g.contains))
                if ready.isEmpty then
                  Left(s"patch-graph: unsatisfiable parents among ${blocked.map(_.id.short).mkString(",")}")
                else
                  ready.foldLeft[Either[String, Graph]](Right(g))((acc, n) => acc.flatMap(_.add(n)))
                    .flatMap(ng => loop(blocked, ng))
            loop(ns, empty)
          catch case e: Exception => Left(Option(e.getMessage).getOrElse(e.toString))
        case _ => Left("patch-graph: missing tag")

    /** Build a linear chain from ordered ValidatedChangeSet digests.
      * Each entry's sole parent is the previous change (except the first).
      */
    def linear(
        entries: List[(Digest, Digest, Digest)] // (id, base, result)
    ): Either[String, Graph] =
      entries.zipWithIndex.foldLeft[Either[String, Graph]](Right(empty)) {
        case (Left(e), _) => Left(e)
        case (Right(g), ((id, base, result), i)) =>
          val parents = if i == 0 then Nil else List(entries(i - 1)._1)
          g.add(Node(id, parents, base, result))
      }
