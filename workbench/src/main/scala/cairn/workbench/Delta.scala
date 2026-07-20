package cairn.workbench

import cairn.kernel.*

/** A module: named definitions in some language L. The unit ΔL edits act on. */
final case class Module(defs: List[(String, Cst)]):
  def sorted: Module = Module(defs.sortBy(_._1))
  def get(name: String): Option[Cst] = defs.find(_._1 == name).map(_._2)
  def canon: Canon = Canon.CList(sorted.defs.map((n, t) =>
    Canon.cmap("name" -> Canon.CStr(n), "term" -> Cst.toCanon(t))))
  def artifact: Artifact = Artifact(ArtifactKind.Ir, canon)
  def digest: Digest = artifact.digest

object Module:
  def fromCanon(c: Canon): Module =
    import Canon.*
    Module(c.asList.map(e => (e.field("name").asStr, Cst.fromCanon(e.field("term")))))

/** Free changes language ΔL (S17, §2b — forced recursive closure).
  *
  * For ANY composed language `L`, [[Delta.deltaOf]] mechanically derives `ΔL`:
  * a real [[ComposedLanguage]] whose terms are change-sets over modules of `L`.
  * Because the result is itself a `ComposedLanguage`, `deltaOf(deltaOf(L))`
  * gives `Δ(ΔL)`, and so on — closure is by construction, not by permission.
  *
  * Edits are terms in ΔL: parsed, validated, then applied (§4.8). Application
  * never mutates; it yields a new module digest plus a ValidatedChangeSet
  * artifact (the kernel-gate record).
  */
object Delta:
  /** Constructor tags are language-qualified so nested Δ levels never collide. */
  def tag(l: ComposedLanguage, op: String): String = s"$op:${l.name}"

  def deltaOf(l: ComposedLanguage): Either[List[ComposeError], ComposedLanguage] =
    val p = l.name
    val termCat = l.grammar.top
    val chg = s"Δ$p.change"
    val chgs = s"Δ$p.changeset"
    val deltaFrag = Fragment(
      name = s"Δ:$p",
      provides = List(s"Δ$p"),
      requires = Nil,
      sorts = List(SortDef(s"Δ$p.Change", SortMode.Tree), SortDef(s"Δ$p.ChangeSet", SortMode.Tree)),
      constructors = List(
        CtorDef(tag(l, "changeset"), s"Δ$p.ChangeSet", List(s"Δ$p.Change")),
        CtorDef(tag(l, "add"), s"Δ$p.Change", List("Name", termCat)),
        CtorDef(tag(l, "replace"), s"Δ$p.Change", List("Name", termCat)),
        CtorDef(tag(l, "remove"), s"Δ$p.Change", List("Name")),
        CtorDef(tag(l, "rename"), s"Δ$p.Change", List("Name", "Name", "Footprint")),
        CtorDef(tag(l, "edit"), s"Δ$p.Change", List("Name", "Path", termCat))),
      grammar = GrammarPart(
        keywords = List("add", "replace", "remove", "rename", "to", "footprint", "edit", "at"),
        puncts = List("{", "}", ";", "=", ",", "[", "]"),
        categories = List(
          CategorySpec(chgs, List(
            ConstructorSpec(tag(l, "changeset"), List(
              Elem.Tok("{"), Elem.Star(Elem.Cat(chg)), Elem.Tok("}"))))),
          CategorySpec(chg, List(
            ConstructorSpec(tag(l, "add"), List(
              Elem.Tok("add"), Elem.NameLeaf, Elem.Tok("="), Elem.Cat(termCat), Elem.Tok(";"))),
            ConstructorSpec(tag(l, "replace"), List(
              Elem.Tok("replace"), Elem.NameLeaf, Elem.Tok("="), Elem.Cat(termCat), Elem.Tok(";"))),
            ConstructorSpec(tag(l, "remove"), List(
              Elem.Tok("remove"), Elem.NameLeaf, Elem.Tok(";"))),
            ConstructorSpec(tag(l, "edit"), List(
              Elem.Tok("edit"), Elem.NameLeaf, Elem.Tok("at"), Elem.Tok("["),
              Elem.Opt(Elem.SepBy1(Elem.NumLeaf, ",")), Elem.Tok("]"),
              Elem.Tok("="), Elem.Cat(termCat), Elem.Tok(";"))),
            ConstructorSpec(tag(l, "rename"), List(
              Elem.Tok("rename"), Elem.NameLeaf, Elem.Tok("to"), Elem.NameLeaf,
              Elem.Tok("footprint"), Elem.Tok("["),
              Elem.Opt(Elem.SepBy1(Elem.NameLeaf, ",")), Elem.Tok("]"), Elem.Tok(";")))))),
        printRules = List(
          PrintRule(tag(l, "changeset"), List(
            PrintSeg.Lit("{"), PrintSeg.Newline, PrintSeg.IndentIn,
            PrintSeg.SepFields(0, "\n"), PrintSeg.Newline,
            PrintSeg.IndentOut, PrintSeg.Lit("}"))),
          PrintRule(tag(l, "add"), List(
            PrintSeg.Lit("add"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
            PrintSeg.Lit("="), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Lit(";"))),
          PrintRule(tag(l, "replace"), List(
            PrintSeg.Lit("replace"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
            PrintSeg.Lit("="), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Lit(";"))),
          PrintRule(tag(l, "remove"), List(
            PrintSeg.Lit("remove"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Lit(";"))),
          PrintRule(tag(l, "edit"), List(
            PrintSeg.Lit("edit"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
            PrintSeg.Lit("at"), PrintSeg.Space, PrintSeg.Lit("["), PrintSeg.Field(1), PrintSeg.Lit("]"),
            PrintSeg.Space, PrintSeg.Lit("="), PrintSeg.Space, PrintSeg.Field(2), PrintSeg.Lit(";"))),
          PrintRule(tag(l, "rename"), List(
            PrintSeg.Lit("rename"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
            PrintSeg.Lit("to"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space,
            PrintSeg.Lit("footprint"), PrintSeg.Space, PrintSeg.Lit("["),
            PrintSeg.Field(2), PrintSeg.Lit("]"), PrintSeg.Lit(";")))),
        top = Some(chgs)))
    // Base language fragments contribute their grammar but yield the top slot to ΔL.
    val demoted = l.fragments.map(f => f.copy(grammar = f.grammar.copy(top = None)))
    Compose.compose(s"Δ${l.name}", demoted :+ deltaFrag)

  /** Record of a kernel-gated, applied change-set (§2 Kernel gate). */
  final case class ValidatedChangeSet(
      language: Digest, base: Digest, change: Cst, result: Digest):
    def canon: Canon = Canon.cmap(
      "language" -> Canon.CStr(language.hex),
      "base" -> Canon.CStr(base.hex),
      "change" -> Cst.toCanon(change),
      "result" -> Canon.CStr(result.hex))
    def artifact: Artifact = Artifact(ArtifactKind.ChangeSet, canon)

  /** Child-index path helpers for structural edits (M15). */
  def pathOf(pathCst: Cst): List[Int] = pathCst match
    case Cst.Node("some", List(Cst.Node("list", items))) => items.collect { case Cst.Leaf(n) => n.toInt }
    case Cst.Node("none", _)                             => Nil
    case Cst.Node("list", items)                         => items.collect { case Cst.Leaf(n) => n.toInt }
    case _                                               => Nil

  def subtreeAt(t: Cst, path: List[Int]): Either[String, Cst] = path match
    case Nil => Right(t)
    case i :: rest => t match
      case Cst.Node(c, cs) if i >= 0 && i < cs.length => subtreeAt(cs(i), rest)
      case Cst.Node(c, cs) => Left(s"path index $i out of range for '$c' (${cs.length} children)")
      case Cst.Leaf(x)     => Left(s"path descends into leaf '$x'")

  def replaceAt(t: Cst, path: List[Int], replacement: Cst): Either[String, Cst] = path match
    case Nil => Right(replacement)
    case i :: rest => t match
      case Cst.Node(c, cs) if i >= 0 && i < cs.length =>
        replaceAt(cs(i), rest, replacement).map(sub => Cst.Node(c, cs.updated(i, sub)))
      case Cst.Node(c, cs) => Left(s"path index $i out of range for '$c' (${cs.length} children)")
      case Cst.Leaf(x)     => Left(s"path descends into leaf '$x'")

  /** Validate + apply a ΔL change-set term to a module. Structured errors;
    * no silent overwrites; renames must carry an exact footprint.
    */
  def apply(l: ComposedLanguage, module: Module, change: Cst): Either[String, (Module, ValidatedChangeSet)] =
    def referencing(m: Module, name: String): Set[String] =
      val spec = l.binderSpec
      val vc = l.varCtor.getOrElse("var")
      m.defs.collect { case (n, t) if n != name && Binding.freeVars(spec, vc)(t).contains(name) => n }.toSet

    def applyOne(m: Module, ch: Cst): Either[String, Module] = ch match
      case Cst.Node(t, List(Cst.Leaf(name), term)) if t == tag(l, "add") =>
        if m.get(name).isDefined then Left(s"ΔL add: '$name' already defined (use replace)")
        else Right(Module(m.defs :+ (name, term)))
      case Cst.Node(t, List(Cst.Leaf(name), term)) if t == tag(l, "replace") =>
        if m.get(name).isEmpty then Left(s"ΔL replace: '$name' not defined")
        else Right(Module(m.defs.map((n, old) => if n == name then (n, term) else (n, old))))
      case Cst.Node(t, List(Cst.Leaf(name))) if t == tag(l, "remove") =>
        if m.get(name).isEmpty then Left(s"ΔL remove: '$name' not defined")
        else
          val refs = referencing(m, name)
          if refs.nonEmpty then Left(s"ΔL remove: '$name' still referenced by ${refs.toList.sorted.mkString(", ")}")
          else Right(Module(m.defs.filterNot(_._1 == name)))
      case Cst.Node(t, List(Cst.Leaf(name), pathCst, term)) if t == tag(l, "edit") =>
        // M15: structural path edit — replace the subtree at child-index path
        val path = pathOf(pathCst)
        m.get(name) match
          case None => Left(s"ΔL edit: '$name' not defined")
          case Some(old) =>
            replaceAt(old, path, term) match
              case Right(updated) => Right(Module(m.defs.map((n, t0) => if n == name then (n, updated) else (n, t0))))
              case Left(err)      => Left(s"ΔL edit '$name': $err")
      case Cst.Node(t, List(Cst.Leaf(from), Cst.Leaf(to), fp)) if t == tag(l, "rename") =>
        val declared: Set[String] = fp match
          case Cst.Node("some", List(Cst.Node("list", items))) => items.collect { case Cst.Leaf(n) => n }.toSet
          case Cst.Node("none", _) => Set.empty
          case Cst.Node("list", items) => items.collect { case Cst.Leaf(n) => n }.toSet
          case other => Set.empty
        if m.get(from).isEmpty then Left(s"ΔL rename: '$from' not defined")
        else if m.get(to).isDefined then Left(s"ΔL rename: target '$to' already defined")
        else
          val actual = referencing(m, from)
          if declared != actual then
            Left(s"ΔL rename footprint mismatch for '$from': declared {${declared.toList.sorted.mkString(",")}}, actual {${actual.toList.sorted.mkString(",")}}")
          else
            val spec = l.binderSpec
            val vc = l.varCtor.getOrElse("var")
            Right(Module(m.defs.map { (n, t0) =>
              val n2 = if n == from then to else n
              val t2 = if actual.contains(n) then Binding.rename(spec, vc)(t0, from, to) else t0
              (n2, t2) }))
      case other => Left(s"not a ΔL change term: ${other.render}")

    val changes = change match
      case Cst.Node(t, List(Cst.Node("list", items))) if t == tag(l, "changeset") => Right(items)
      case Cst.Node(t, items) if t == tag(l, "changeset") => Right(items)
      case single @ Cst.Node(t, _) if t.startsWith("add:") || t.startsWith("replace:") ||
          t.startsWith("remove:") || t.startsWith("rename:") || t.startsWith("edit:") => Right(List(single))
      case other => Left(s"not a ΔL changeset: ${other.render}")

    changes.flatMap { chs =>
      chs.foldLeft[Either[String, Module]](Right(module)) { (acc, ch) => acc.flatMap(applyOne(_, ch)) }
        .map { result =>
          val vcs = ValidatedChangeSet(l.digest, module.digest, change, result.digest)
          (result.sorted, vcs) }
    }
