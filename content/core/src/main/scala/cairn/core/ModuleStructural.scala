package cairn.core

import cairn.kernel.*

/** Generic whole-module structural checks — list folds Search cannot express.
  *
  * Domain packs must not embed these walks; they pass [[Spec]]s (or call the
  * helpers) from a thin gate. Arithmetic aggregates, uniqueness, and ordered
  * ref lists belong here — not in SDS/Law/PKI classes.
  */
object ModuleStructural:

  enum Spec:
    /** Sum of integer leaves at `leafPath` under each `ctor` binding ≤ `max`. */
    case SumLeavesAtMost(ctor: String, leafPath: List[Int], max: Long, label: String)
    /** Values at `keyPaths` under `ctor` must be unique across the module. */
    case UniqueTuples(ctor: String, keyPaths: List[List[Int]], label: String)
    /** Non-empty string leaves at indices under `ctor`. */
    case NonEmptyLeaves(ctor: String, indices: List[Int], labels: List[String])
    /** Refs in a list field must exist and satisfy `ok`; collected ints must be unique+ascending. */
    case OutlineNums(
        ctor: String,
        refsField: Int,
        resolveNum: (Module, String) => Either[String, Int],
        label: String,
    )

  def run(m: Module, specs: List[Spec]): List[String] =
    specs.flatMap(check(m, _))

  def check(m: Module, spec: Spec): List[String] = spec match
    case Spec.SumLeavesAtMost(ctor, leafPath, max, label) =>
      m.defs.collect {
        case (name, Cst.Node(c, fields)) if c == ctor =>
          sumAt(fields, leafPath) match
            case Left(e) => Some(s"$label '$name': $e")
            case Right(s) if s > max =>
              Some(s"$label '$name' percentages sum to $s > $max")
            case Right(_) => None
      }.flatten

    case Spec.UniqueTuples(ctor, keyPaths, label) =>
      val seen = scala.collection.mutable.HashSet.empty[List[String]]
      val errs = List.newBuilder[String]
      for (name, term) <- m.defs do term match
        case Cst.Node(c, fields) if c == ctor =>
          traverse(keyPaths)(p => leafAt(fields, p)) match
            case Left(e) => errs += s"$label '$name': $e"
            case Right(keys) =>
              if keys.exists(_.isEmpty) then
                errs += s"$label '$name': empty key field"
              else if !seen.add(keys) then
                errs += s"$label '$name' duplicate mark for ${keys.mkString("/")}"
        case _ => ()
      errs.result()

    case Spec.NonEmptyLeaves(ctor, indices, labels) =>
      m.defs.collect {
        case (name, Cst.Node(c, fields)) if c == ctor =>
          indices.zip(labels).flatMap { (i, lab) =>
            fields.lift(i) match
              case Some(Cst.Leaf(v)) if v.isEmpty =>
                Some(s"$ctor '$name': empty $lab")
              case Some(Cst.Leaf(_)) => None
              case Some(other) => Some(s"$ctor '$name': bad $lab ${other.render}")
              case None => Some(s"$ctor '$name': missing $lab")
          }
      }.flatten

    case Spec.OutlineNums(ctor, refsField, resolveNum, label) =>
      m.defs.collect {
        case (name, Cst.Node(c, fields)) if c == ctor =>
          val refs = fields.lift(refsField).map(refList).getOrElse(Left("missing refs"))
          refs match
            case Left(e) => List(s"$label '$name': $e")
            case Right(rs) =>
              val nums = List.newBuilder[Int]
              val errs = List.newBuilder[String]
              if rs.isEmpty then errs += s"$label '$name' has no sections"
              for ref <- rs do
                resolveNum(m, ref) match
                  case Left(e) => errs += s"$label '$name': $e"
                  case Right(n) => nums += n
              val ns = nums.result()
              if ns.distinct.sizeIs != ns.size then
                errs += s"$label '$name' has duplicate section numbers"
              if ns != ns.sorted then
                errs += s"$label '$name' section numbers not ascending: ${ns.mkString(",")}"
              errs.result()
      }.flatten

  private def refList(field: Cst): Either[String, List[String]] = field match
    case Cst.Node("none", _) => Right(Nil)
    case Cst.Node("some", List(Cst.Node("list", rs))) =>
      Right(rs.collect { case Cst.Leaf(r) => r })
    case Cst.Node("list", rs) => Right(rs.collect { case Cst.Leaf(r) => r })
    case other => Left(s"bad sections ${other.render}")

  private def leafAt(fields: List[Cst], path: List[Int]): Either[String, String] =
    path match
      case Nil => Left("empty path")
      case i :: Nil =>
        fields.lift(i) match
          case Some(Cst.Leaf(v)) => Right(v)
          case Some(other) => Left(s"not a leaf: ${other.render}")
          case None => Left(s"missing field $i")
      case i :: rest =>
        fields.lift(i) match
          case Some(Cst.Node(_, kids)) => leafAt(kids, rest)
          case Some(other) => Left(s"not a node: ${other.render}")
          case None => Left(s"missing field $i")

  /** Sum integer leaves under list components: fields(0)=list of nodes, each
    * node has an int leaf at `leafPath` relative to that node.
    */
  private def sumAt(fields: List[Cst], leafPath: List[Int]): Either[String, Long] =
    fields.headOption match
      case Some(Cst.Node("list", comps)) =>
        comps.foldLeft[Either[String, Long]](Right(0L)) { (acc, comp) =>
          acc.flatMap { s =>
            comp match
              case Cst.Node(_, kids) =>
                leafAt(kids, leafPath).flatMap { v =>
                  v.toLongOption.toRight(s"not an int: $v").map(s + _)
                }
              case other => Left(s"bad component ${other.render}")
          }
        }
      case Some(other) => Left(s"expected list: ${other.render}")
      case None => Left("missing components")

  private def traverse[A, B](xs: List[A])(f: A => Either[String, B]): Either[String, List[B]] =
    xs.foldLeft[Either[String, List[B]]](Right(Nil)) { (acc, a) =>
      acc.flatMap(bs => f(a).map(bs :+ _))
    }
