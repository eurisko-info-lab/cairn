package cairn.core

import cairn.kernel.*

/** Generic whole-module structural checks — list folds Search cannot express.
  *
  * Domain packs must not embed these walks; they pass [[Spec]]s (or call the
  * helpers) from a thin gate. Arithmetic aggregates, uniqueness, ref
  * resolution, and ordered ref lists belong here — not in SDS/Law/PKI classes.
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
    /** Leaf at `idx` must name a defined binding. */
    case DefinedRef(ctor: String, idx: Int, label: String)
    /** Leaf at each index must name a defined binding. */
    case DefinedRefs(ctor: String, idxs: List[Int], label: String)
    /** List field of leaves; each leaf must name a defined binding. */
    case DefinedLeafList(ctor: String, listIdx: Int, label: String)
    /** List field of nodes; leaf at `refPath` in each node must be defined. */
    case DefinedNodeListRefs(
        ctor: String, listIdx: Int, refPath: List[Int], label: String)
    /** Leaf at `idx` must be non-empty and pass `ok`. */
    case LeafOk(
        ctor: String, idx: Int, ok: String => Boolean, detail: String => String)
    /** Leaf value equals some target ctor's field (value match, not binding name). */
    case LeafValueInCtorField(
        ctor: String,
        leafIdx: Int,
        targetCtors: Set[String],
        targetFieldIdx: Int,
        label: String,
    )
    /** Ref leaf resolves to a term whose tag is in `tags`. */
    case RefTagIn(ctor: String, refIdx: Int, tags: Set[String], label: String)
    /** Unique key-tuples among children of a list field. */
    case UniqueTuplesInList(
        ctor: String,
        listIdx: Int,
        keyPaths: List[List[Int]],
        label: String,
        childTags: Option[Set[String]] = None,
    )
    /** For matching child tags in a list, require defined refs at paths. */
    case ListChildDefinedRefs(
        ctor: String,
        listIdx: Int,
        /** childTag → paths to leaf refs that must be defined */
        byTag: Map[String, List[List[Int]]],
        label: String,
    )
    /** Optional/list overlay of keyed rows: unique (key,lang), keys ⊆ allowed,
      * ref children must resolve. `fieldIdx` may be past arity (skipped).
      */
    case KeyedLocaleOverlay(
        ctor: String,
        fieldIdx: Int,
        allowedKeys: Set[String],
        plainTag: String,
        refTag: String,
        keyIdx: Int,
        langIdx: Int,
        valueIdx: Int,
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

    case Spec.DefinedRef(ctor, idx, label) =>
      check(m, Spec.DefinedRefs(ctor, List(idx), label))

    case Spec.DefinedRefs(ctor, idxs, label) =>
      m.defs.collect {
        case (name, Cst.Node(c, fields)) if c == ctor =>
          idxs.flatMap { i =>
            fields.lift(i) match
              case Some(Cst.Leaf(ref)) if ref.nonEmpty && m.get(ref).isEmpty =>
                Some(s"$label '$name' references unknown '$ref'")
              case Some(Cst.Leaf(_)) => None
              case Some(other) => Some(s"$label '$name': bad ref ${other.render}")
              case None => Some(s"$label '$name': missing ref field $i")
          }
      }.flatten

    case Spec.DefinedLeafList(ctor, listIdx, label) =>
      m.defs.collect {
        case (name, Cst.Node(c, fields)) if c == ctor =>
          fields.lift(listIdx).map(asList).getOrElse(Left("missing list")) match
            case Left(e) => List(s"$label '$name': $e")
            case Right(xs) =>
              xs.flatMap {
                case Cst.Leaf(ref) if ref.nonEmpty && m.get(ref).isEmpty =>
                  Some(s"$label '$name' references unknown '$ref'")
                case Cst.Leaf(_) => None
                case other => Some(s"$label '$name': bad list item ${other.render}")
              }
      }.flatten

    case Spec.DefinedNodeListRefs(ctor, listIdx, refPath, label) =>
      m.defs.collect {
        case (name, Cst.Node(c, fields)) if c == ctor =>
          fields.lift(listIdx).map(asList).getOrElse(Left("missing list")) match
            case Left(e) => List(s"$label '$name': $e")
            case Right(xs) =>
              xs.flatMap {
                case Cst.Node(_, kids) =>
                  leafAt(kids, refPath) match
                    case Left(e) => Some(s"$label '$name': $e")
                    case Right(ref) if ref.nonEmpty && m.get(ref).isEmpty =>
                      Some(s"$label '$name' references unknown '$ref'")
                    case Right(_) => None
                case other => Some(s"$label '$name': bad component ${other.render}")
              }
      }.flatten

    case Spec.LeafOk(ctor, idx, ok, detail) =>
      m.defs.collect {
        case (name, Cst.Node(c, fields)) if c == ctor =>
          fields.lift(idx) match
            case Some(Cst.Leaf(v)) if v.isEmpty =>
              Some(s"$ctor '$name': empty field $idx")
            case Some(Cst.Leaf(v)) if !ok(v) =>
              Some(detail(s"$ctor '$name': $v"))
            case Some(Cst.Leaf(_)) => None
            case Some(other) => Some(s"$ctor '$name': bad field ${other.render}")
            case None => Some(s"$ctor '$name': missing field $idx")
      }.flatten

    case Spec.LeafValueInCtorField(ctor, leafIdx, targetCtors, targetFieldIdx, label) =>
      val values = m.defs.collect {
        case (_, Cst.Node(c, fields)) if targetCtors.contains(c) =>
          fields.lift(targetFieldIdx).collect { case Cst.Leaf(v) => v }
      }.flatten.toSet
      m.defs.collect {
        case (name, Cst.Node(c, fields)) if c == ctor =>
          fields.lift(leafIdx) match
            case Some(Cst.Leaf(v)) if v.isEmpty =>
              Some(s"$label '$name': empty value")
            case Some(Cst.Leaf(v)) if !values.contains(v) =>
              Some(s"$label '$name' references unknown '$v'")
            case Some(Cst.Leaf(_)) => None
            case Some(other) => Some(s"$label '$name': bad value ${other.render}")
            case None => Some(s"$label '$name': missing value")
      }.flatten

    case Spec.RefTagIn(ctor, refIdx, tags, label) =>
      m.defs.collect {
        case (name, Cst.Node(c, fields)) if c == ctor =>
          fields.lift(refIdx) match
            case Some(Cst.Leaf(ref)) if ref.isEmpty =>
              Some(s"$label '$name': empty ref")
            case Some(Cst.Leaf(ref)) =>
              m.get(ref) match
                case Some(Cst.Node(tag, _)) if tags.contains(tag) => None
                case Some(_) =>
                  Some(s"$label '$name' references '$ref' which is not a valid target")
                case None =>
                  Some(s"$label '$name' references unknown '$ref'")
            case Some(other) => Some(s"$label '$name': bad ref ${other.render}")
            case None => Some(s"$label '$name': missing ref")
      }.flatten

    case Spec.UniqueTuplesInList(ctor, listIdx, keyPaths, label, childTags) =>
      m.defs.collect {
        case (name, Cst.Node(c, fields)) if c == ctor =>
          fields.lift(listIdx).map(asList).getOrElse(Left("missing list")) match
            case Left(e) => List(s"$label '$name': $e")
            case Right(xs) =>
              val seen = scala.collection.mutable.HashSet.empty[List[String]]
              val errs = List.newBuilder[String]
              for x <- xs do x match
                case Cst.Node(tag, kids) if childTags.forall(_.contains(tag)) =>
                  traverse(keyPaths)(p => leafAt(kids, p)) match
                    case Left(e) => errs += s"$label '$name': $e"
                    case Right(keys) =>
                      if keys.exists(_.isEmpty) then
                        errs += s"$label '$name': empty key field"
                      else if !seen.add(keys) then
                        errs += s"$label '$name' duplicate field '${keys.mkString("' lang '")}'"
                case Cst.Node(_, _) => ()
                case other => errs += s"$label '$name': bad item ${other.render}"
              errs.result()
      }.flatten

    case Spec.ListChildDefinedRefs(ctor, listIdx, byTag, label) =>
      m.defs.collect {
        case (name, Cst.Node(c, fields)) if c == ctor =>
          fields.lift(listIdx).map(asList).getOrElse(Left("missing list")) match
            case Left(e) => List(s"$label '$name': $e")
            case Right(xs) =>
              xs.flatMap {
                case Cst.Node(tag, kids) =>
                  byTag.get(tag).toList.flatten.flatMap { path =>
                    leafAt(kids, path) match
                      case Left(e) => Some(s"$label '$name': $e")
                      case Right(ref) if ref.nonEmpty && m.get(ref).isEmpty =>
                        Some(s"$label '$name' references unknown '$ref'")
                      case Right(_) => None
                  }
                case other => List(s"$label '$name': bad item ${other.render}")
              }
      }.flatten

    case Spec.KeyedLocaleOverlay(
        ctor, fieldIdx, allowedKeys, plainTag, refTag, keyIdx, langIdx, valueIdx, label) =>
      m.defs.collect {
        case (name, Cst.Node(c, fields)) if c == ctor =>
          fields.lift(fieldIdx) match
            case None => Nil
            case Some(ov) =>
              val rows = localeRows(ov)
              val seen = scala.collection.mutable.HashSet.empty[(String, String)]
              val errs = List.newBuilder[String]
              for row <- rows do row match
                case Cst.Node(tag, kids) if tag == plainTag || tag == refTag =>
                  (kids.lift(keyIdx), kids.lift(langIdx), kids.lift(valueIdx)) match
                    case (Some(Cst.Leaf(k)), Some(Cst.Leaf(lang)), Some(Cst.Leaf(v))) =>
                      if k.isEmpty then errs += s"$label '$name': empty locale key"
                      else if !allowedKeys.contains(k) then
                        errs += s"$label '$name': locale key '$k' not in typed slots"
                      else if lang.isEmpty then
                        errs += s"$label '$name' locale '$k': empty lang"
                      else if tag == refTag && v.isEmpty then
                        errs += s"$label '$name' locale '$k': empty phrase ref"
                      else if tag == refTag && m.get(v).isEmpty then
                        errs += s"$label '$name' locale '$k' references unknown phrase '$v'"
                      else if !seen.add((k, lang)) then
                        errs += s"$label '$name' duplicate locale '$k' lang '$lang'"
                    case _ => errs += s"$label '$name': bad locale ${row.render}"
                case other => errs += s"$label '$name': bad locale ${other.render}"
              errs.result()
      }.flatten

  private def localeRows(overlays: Cst): List[Cst] = overlays match
    case Cst.Node("none", _) => Nil
    case Cst.Node("some", List(Cst.Node("list", xs))) => xs
    case Cst.Node("list", xs) => xs
    case _ => Nil

  private def asList(field: Cst): Either[String, List[Cst]] = field match
    case Cst.Node("none", _) => Right(Nil)
    case Cst.Node("some", List(Cst.Node("list", xs))) => Right(xs)
    case Cst.Node("list", xs) => Right(xs)
    case other => Left(s"expected list: ${other.render}")

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

  /** Collect leaf refs at given indices under named ctors inside a ΔL change.
    * Domain packs pass ctor→indices; no per-domain match arms.
    */
  def changeLeafRefs(change: Cst, ctorIndices: Map[String, List[Int]]): Set[String] =
    def items(c: Cst): List[Cst] = c match
      case Cst.Node(_, List(Cst.Node("list", xs))) => xs
      case Cst.Node(_, xs) => xs
      case _ => List(c)
    items(change).flatMap {
      case Cst.Node(_, List(_, term)) => term match
        case Cst.Node(tag, fields) =>
          ctorIndices.get(tag).toList.flatten.flatMap { i =>
            fields.lift(i).collect { case Cst.Leaf(r) if r.nonEmpty => r }
          }
        case _ => Nil
      case _ => Nil
    }.toSet
