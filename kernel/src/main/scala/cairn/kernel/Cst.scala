package cairn.kernel

/** Universal untyped syntax tree (S8, S14). Every parsed or constructed term is
  * a Cst; typed elaboration happens above the kernel. Leaves carry token text.
  */
enum Cst:
  case Leaf(text: String)
  case Node(ctor: String, children: List[Cst])

object Cst:
  def node(ctor: String, children: Cst*): Cst = Node(ctor, children.toList)

  def toCanon(c: Cst): Canon = c match
    case Leaf(t)        => Canon.CTag("leaf", Canon.CStr(t))
    case Node(ctor, cs) => Canon.CTag("node", Canon.cmap(
      "ctor" -> Canon.CStr(ctor),
      "children" -> Canon.CList(cs.map(toCanon))))

  def fromCanon(c: Canon): Cst =
    import Canon.*
    c match
      case CTag("leaf", CStr(t)) => Leaf(t)
      case CTag("node", body)    =>
        Node(body.field("ctor").asStr, body.field("children").asList.map(fromCanon))
      case other => throw CodecError(s"not a cst: $other")

  extension (c: Cst)
    def render: String = c match
      case Leaf(t)        => t
      case Node(ctor, cs) => s"$ctor(${cs.map(_.render).mkString(", ")})"

/** Binder specification (S14): which child positions of a constructor bind a
  * name (the leaf at `binderIndex`) within the children at `scopeIndices`.
  * Drives generic alpha-renaming and capture-avoiding substitution — no
  * object-language-specific binding code anywhere (§4.2 spirit).
  */
final case class BinderSpec(bindings: Map[String, List[(Int, List[Int])]]):
  def bindersOf(ctor: String): List[(Int, List[Int])] = bindings.getOrElse(ctor, Nil)

object Binding:
  /** Free variables of a term: any Leaf in variable position. A leaf is treated
    * as a variable occurrence when it appears as a child of a `varCtor` node.
    */
  def freeVars(spec: BinderSpec, varCtor: String)(t: Cst): Set[String] = t match
    case Cst.Leaf(_) => Set.empty
    case Cst.Node(c, List(Cst.Leaf(x))) if c == varCtor => Set(x)
    case Cst.Node(c, cs) =>
      val binders = spec.bindersOf(c)
      cs.zipWithIndex.flatMap { (child, i) =>
        val fv = freeVars(spec, varCtor)(child)
        val bound = binders.collect { case (bi, scope) if scope.contains(i) =>
          cs(bi) match { case Cst.Leaf(x) => x; case _ => "" } }.toSet
        fv -- bound
      }.toSet

  private var fresh = 0
  private def freshName(base: String, avoid: Set[String]): String =
    var candidate = base
    while avoid.contains(candidate) do { fresh += 1; candidate = s"$base$$$fresh" }
    candidate

  /** Capture-avoiding substitution [x := v]t, generic over the binder spec. */
  def subst(spec: BinderSpec, varCtor: String)(t: Cst, x: String, v: Cst): Cst = t match
    case Cst.Leaf(_) => t
    case Cst.Node(c, List(Cst.Leaf(y))) if c == varCtor =>
      if y == x then v else t
    case Cst.Node(c, cs) =>
      val binders = spec.bindersOf(c)
      if binders.isEmpty then Cst.Node(c, cs.map(subst(spec, varCtor)(_, x, v)))
      else
        // rename binders that would capture free vars of v, or that shadow x
        var children = cs
        for (bi, scope) <- binders do
          children(bi) match
            case Cst.Leaf(y) =>
              if y == x then
                () // x is shadowed inside scope children: skip substitution there (handled below)
              else if freeVars(spec, varCtor)(v).contains(y) then
                val fvAll = freeVars(spec, varCtor)(v) ++ scope.flatMap(i => freeVars(spec, varCtor)(children(i))) + x
                val y2 = freshName(y, fvAll)
                children = children.zipWithIndex.map { (ch, i) =>
                  if i == bi then Cst.Leaf(y2)
                  else if scope.contains(i) then rename(spec, varCtor)(ch, y, y2)
                  else ch }
            case _ => ()
        val shadowed = binders.flatMap { (bi, scope) =>
          children(bi) match { case Cst.Leaf(y) if y == x => scope; case _ => Nil } }.toSet
        Cst.Node(c, children.zipWithIndex.map { (ch, i) =>
          if shadowed.contains(i) then ch else subst(spec, varCtor)(ch, x, v) })

  /** Rename free occurrences of `from` to `to`. */
  def rename(spec: BinderSpec, varCtor: String)(t: Cst, from: String, to: String): Cst =
    subst(spec, varCtor)(t, from, Cst.Node(varCtor, List(Cst.Leaf(to))))

/** M2: alpha-invariant canonical form. Binder names are replaced by de Bruijn
  * LEVEL names (`_0`, `_1`, ...: number of binders enclosing the binding site),
  * driven entirely by the binder-spec table. Two α-equivalent terms normalize
  * to the identical tree, so `Alpha.digest` is a name-independent identity.
  * Surfaces keep user names; identity does not.
  */
object Alpha:
  def normalize(spec: BinderSpec, varCtor: String)(t: Cst): Cst =
    def go(t: Cst, env: Map[String, String], depth: Int): Cst = t match
      case Cst.Leaf(_) => t
      case Cst.Node(c, List(Cst.Leaf(x))) if c == varCtor =>
        Cst.Node(c, List(Cst.Leaf(env.getOrElse(x, x))))
      case Cst.Node(c, cs) =>
        val binders = spec.bindersOf(c)
        if binders.isEmpty then Cst.Node(c, cs.map(go(_, env, depth)))
        else
          // assign level names in binder-position order
          val assignments: List[(Int, List[Int], String, String)] =
            binders.zipWithIndex.flatMap { case ((bi, scope), k) =>
              cs(bi) match
                case Cst.Leaf(old) => List((bi, scope, old, s"_${depth + k}"))
                case _             => Nil }
          val newDepth = depth + assignments.length
          Cst.Node(c, cs.zipWithIndex.map { (ch, i) =>
            assignments.find(_._1 == i) match
              case Some((_, _, _, fresh)) => Cst.Leaf(fresh)
              case None =>
                val envHere = assignments.foldLeft(env) { case (e, (_, scope, old, fresh)) =>
                  if scope.contains(i) then e + (old -> fresh) else e }
                go(ch, envHere, newDepth) })
    go(t, Map.empty, 0)

  /** Name-independent content identity of a term in a given binding discipline. */
  def digest(spec: BinderSpec, varCtor: String)(t: Cst): Digest =
    Artifact(ArtifactKind.Term, Cst.toCanon(normalize(spec, varCtor)(t))).digest

  def equivalent(spec: BinderSpec, varCtor: String)(a: Cst, b: Cst): Boolean =
    normalize(spec, varCtor)(a) == normalize(spec, varCtor)(b)
