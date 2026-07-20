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

  /** Accept either a `changeset` node or a single bare change term (same
    * tolerance as [[apply]]) and return its flat list of change items.
    */
  private def changeItems(l: ComposedLanguage, c: Cst): Either[String, List[Cst]] = c match
    case Cst.Node(t, List(Cst.Node("list", xs))) if t == tag(l, "changeset") => Right(xs)
    case Cst.Node(t, xs) if t == tag(l, "changeset")                        => Right(xs)
    case single @ Cst.Node(t, _) if t.startsWith("add:") || t.startsWith("replace:") ||
        t.startsWith("remove:") || t.startsWith("rename:") || t.startsWith("edit:") => Right(List(single))
    case other => Left(s"not a ΔL changeset: ${other.render}")

  private def moduleDefs(cst: Cst): Either[String, List[Cst]] = cst match
    case Cst.Node("moduleFile", List(Cst.Node("list", defs))) => Right(defs)
    case other => Left(s"not a module file: ${other.render}")

  private def findDef(defs: List[Cst], name: String): Option[Cst] =
    defs.find {
      case Cst.Node("moduleDef", List(Cst.Leaf(n), _)) => n == name
      case _ => false
    }

  /** Format-preserving ΔL apply (grammar-as-lens, part b): reprints only the
    * bytes an edit actually touches, splicing into the ORIGINAL source text
    * instead of [[apply]] + `Printer.print`'s whole-module canonical reprint.
    * Built entirely on the existing, independently-tested `Concrete.splice`
    * (M7, `Grammar.scala`) — no kernel or parser changes.
    *
    * Handles `replace` / `edit` (both are 1:1 subtree substitution — exactly
    * what `Concrete.splice` already does) and `add` (pure insertion, no
    * existing span to touch). `remove` and `rename` are NOT supported here,
    * on purpose, and fail with an explicit `Left` rather than a silent
    * fallback to canonical reprint:
    *   - `remove` needs a design decision for the blank-line trivia left
    *     around a deleted def that shouldn't be made unilaterally here.
    *   - `rename` needs the parser to also record spans for bare `Cst.Leaf`
    *     name tokens — today `spanned(...)` only wraps `Cst.Node`
    *     construction, never `Elem.NameLeaf`'s result, so a def's own name
    *     token has no span to splice against yet.
    */
  def applyPreservingFormat(l: ComposedLanguage, moduleGrammar: GrammarSpec,
                            source: String, change: Cst): Either[String, String] =
    changeItems(l, change).flatMap { chs =>
      chs.foldLeft[Either[String, String]](Right(source)) { (acc, ch) =>
        acc.flatMap(applyOnePreserving(l, moduleGrammar, _, ch))
      }
    }

  private def applyOnePreserving(l: ComposedLanguage, mg: GrammarSpec, source: String, ch: Cst): Either[String, String] =
    Parser.parseFull(mg, source).flatMap { out =>
      moduleDefs(out.cst).flatMap { defs =>
        ch match
          case Cst.Node(t, List(Cst.Leaf(name), term)) if t == tag(l, "replace") =>
            findDef(defs, name) match
              case Some(Cst.Node(_, List(_, termInstance))) =>
                Concrete.splice(mg, source, out, termInstance, term)
              case _ => Left(s"ΔL replace (format-preserving): '$name' not defined")

          case Cst.Node(t, List(Cst.Leaf(name), pathCst, term)) if t == tag(l, "edit") =>
            findDef(defs, name) match
              case Some(Cst.Node(_, List(_, termInstance))) =>
                subtreeAt(termInstance, pathOf(pathCst)).flatMap { target =>
                  Concrete.splice(mg, source, out, target, term)
                }
              case _ => Left(s"ΔL edit (format-preserving): '$name' not defined")

          case Cst.Node(t, List(Cst.Leaf(name), term)) if t == tag(l, "add") =>
            if findDef(defs, name).isDefined then
              Left(s"ΔL add (format-preserving): '$name' already defined (use replace)")
            else
              Printer.print(mg, Cst.node("moduleDef", Cst.Leaf(name), term)).map { printedDef =>
                val real = out.tokens.filter(_.kind != TokKind.Eof)
                val insertAt = real.lastOption.fold(0)(t => t.offset + t.rawLen)
                val prefix = source.substring(0, insertAt)
                val sep = if prefix.isEmpty || prefix.endsWith("\n") then "" else "\n"
                prefix + sep + printedDef + "\n" + source.substring(insertAt)
              }

          case Cst.Node(t, _) if t == tag(l, "remove") =>
            Left("format-preserving apply not yet supported for 'remove' — see Delta.applyPreservingFormat doc comment")
          case Cst.Node(t, _) if t == tag(l, "rename") =>
            Left("format-preserving apply not yet supported for 'rename' — see Delta.applyPreservingFormat doc comment")

          case other => Left(s"not a ΔL change term: ${other.render}")
      }
    }

  /** Compose two changesets by sequencing `cs2` after `cs1` — list
    * concatenation, so `{}` is the identity and composition is associative
    * for free. This alone is always correct: [[apply]]'s fold already gives
    * any sequence (including e.g. `remove x ; add x = t`, or two renames
    * chained through an intermediate name) the right semantics one step at a
    * time. See [[collapseAdjacent]] for the separate, optional canonicalization
    * pass that compresses a few adjacent pairs into one equivalent op.
    */
  def compose(l: ComposedLanguage, cs1: Cst, cs2: Cst): Either[String, Cst] =
    for
      xs1 <- changeItems(l, cs1)
      xs2 <- changeItems(l, cs2)
    yield Cst.node(tag(l, "changeset"), Cst.Node("list", xs1 ++ xs2))

  private def footprintNames(fp: Cst): Set[String] = fp match
    case Cst.Node("some", List(Cst.Node("list", items))) => items.collect { case Cst.Leaf(n) => n }.toSet
    case Cst.Node("list", items)                         => items.collect { case Cst.Leaf(n) => n }.toSet
    case _                                                => Set.empty

  /** `SepBy1` requires at least one item — an empty footprint prints as
    * `none`, never `some([])`, or it wouldn't round-trip under Δ's own grammar.
    */
  private def footprintCst(names: Set[String]): Cst =
    val sorted = names.toList.sorted
    if sorted.isEmpty then Cst.node("none")
    else Cst.node("some", Cst.Node("list", sorted.map(Cst.Leaf(_))))

  /** Optional canonicalization pass over a flat change list: collapses two
    * adjacent, purely-cosmetic-to-compress patterns into a single equivalent
    * op. Neither collapse changes what applying the changeset does — both
    * exist so published/rebased changesets read as intent rather than as a
    * derivation trace:
    *   - `rename x→y ; rename y→z`  ⇒  `rename x→z` (footprints unioned, `y` dropped)
    *   - `remove x   ; add x = t`   ⇒  `replace x = t`
    * Anything else is left exactly as sequenced.
    */
  def collapseAdjacent(l: ComposedLanguage, chs: List[Cst]): List[Cst] = chs match
    case Cst.Node(rt1, List(Cst.Leaf(x), Cst.Leaf(y1), fp1)) ::
         Cst.Node(rt2, List(Cst.Leaf(y2), Cst.Leaf(z), fp2)) :: rest
        if rt1 == tag(l, "rename") && rt2 == tag(l, "rename") && y1 == y2 && x != z =>
      val merged = footprintCst((footprintNames(fp1) ++ footprintNames(fp2)) - y1)
      collapseAdjacent(l, Cst.node(tag(l, "rename"), Cst.Leaf(x), Cst.Leaf(z), merged) :: rest)
    case Cst.Node(rt, List(Cst.Leaf(x))) :: Cst.Node(at, List(Cst.Leaf(x2), term)) :: rest
        if rt == tag(l, "remove") && at == tag(l, "add") && x == x2 =>
      collapseAdjacent(l, Cst.node(tag(l, "replace"), Cst.Leaf(x), term) :: rest)
    case head :: rest => head :: collapseAdjacent(l, rest)
    case Nil => Nil

  /** [[collapseAdjacent]] applied to a full changeset term. */
  def collapse(l: ComposedLanguage, cs: Cst): Either[String, Cst] =
    changeItems(l, cs).map(xs => Cst.node(tag(l, "changeset"), Cst.Node("list", collapseAdjacent(l, xs))))
