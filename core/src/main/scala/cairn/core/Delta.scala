package cairn.core

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

  /** Kernel-gated record of an applied change-set. Opaque: mint only via
    * [[apply]] / [[applyTyped]], or [[ValidatedChangeSet.check]] after replay.
    * Public [[ValidatedChangeSet.decodeClaim]] does not mint — forged canon
    * cannot become a [[ValidatedChangeSet]] without `apply(language, base, change) = result`.
    */
  opaque type ValidatedChangeSet = ValidatedChangeSet.Repr
  object ValidatedChangeSet:
    private[Delta] final case class Repr(
        language: Digest, base: Digest, change: Cst, result: Digest)

    private[core] def mint(
        language: Digest, base: Digest, change: Cst, result: Digest
    ): ValidatedChangeSet =
      Repr(language, base, change, result)

    /** Unchecked fields decoded from canon — not a validated change-set. */
    final case class Claim(language: Digest, base: Digest, change: Cst, result: Digest):
      def canon: Canon = Canon.cmap(
        "language" -> Canon.CStr(language.hex),
        "base" -> Canon.CStr(base.hex),
        "change" -> Cst.toCanon(change),
        "result" -> Canon.CStr(result.hex))

    def decodeClaim(c: Canon): Claim =
      Claim(
        Digest(c.field("language").asStr),
        Digest(c.field("base").asStr),
        Cst.fromCanon(c.field("change")),
        Digest(c.field("result").asStr))

    /** Replay [[apply]]; accept only when the result digest matches the claim. */
    def check(
        l: ComposedLanguage, baseMod: Module, claim: Claim
    ): Either[String, ValidatedChangeSet] =
      if l.digest != claim.language then
        Left(s"ValidatedChangeSet language mismatch: claim ${claim.language.short} ≠ ${l.digest.short}")
      else if baseMod.digest != claim.base then
        Left(s"ValidatedChangeSet base mismatch: claim ${claim.base.short} ≠ ${baseMod.digest.short}")
      else
        apply(l, baseMod, claim.change).flatMap { (result, vcs) =>
          if result.digest != claim.result then
            Left(s"forged ValidatedChangeSet: claimed result ${claim.result.short}, apply yielded ${result.digest.short}")
          else Right(vcs)
        }

    extension (v: ValidatedChangeSet)
      def language: Digest = v.language
      def base: Digest = v.base
      def change: Cst = v.change
      def result: Digest = v.result
      def canon: Canon = Claim(v.language, v.base, v.change, v.result).canon
      def artifact: Artifact = Artifact(ArtifactKind.ChangeSet, v.canon)
      def claim: Claim = Claim(v.language, v.base, v.change, v.result)


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

  /** Typed rejection reasons for [[apply]] (scoped to this one gate, not a
    * kernel-wide error-type migration): every `Left` site in `applyTyped`
    * constructs one of these instead of an ad-hoc string. `render` produces
    * the EXACT text `apply`'s public `Either[String, _]` contract has always
    * returned — this is a refactor of the single source of truth, not a
    * parallel, driftable reimplementation of the same checks.
    */
  enum Rejection:
    case AlreadyDefined(op: String, name: String)
    case NotDefined(op: String, name: String)
    case StillReferenced(name: String, by: Set[String])
    case FootprintMismatch(name: String, declared: Set[String], actual: Set[String])
    case PathError(name: String, detail: String)
    case Malformed(detail: String)

    def render: String = this match
      case AlreadyDefined("add", name)            => s"ΔL add: '$name' already defined (use replace)"
      case AlreadyDefined("rename-target", name)  => s"ΔL rename: target '$name' already defined"
      case AlreadyDefined(op, name)               => s"ΔL $op: '$name' already defined"
      case NotDefined(op, name)                   => s"ΔL $op: '$name' not defined"
      case StillReferenced(name, by)              => s"ΔL remove: '$name' still referenced by ${by.toList.sorted.mkString(", ")}"
      case FootprintMismatch(name, declared, actual) =>
        s"ΔL rename footprint mismatch for '$name': declared {${declared.toList.sorted.mkString(",")}}, actual {${actual.toList.sorted.mkString(",")}}"
      case PathError(name, detail) => s"ΔL edit '$name': $detail"
      case Malformed(detail)       => detail

  /** [[apply]], but with [[Rejection]] left unstringified — the typed view. */
  def applyTyped(l: ComposedLanguage, module: Module, change: Cst): Either[Rejection, (Module, ValidatedChangeSet)] =
    def referencing(m: Module, name: String): Set[String] =
      val spec = l.binderSpec
      val vc = l.varCtor.getOrElse("var")
      m.defs.collect { case (n, t) if n != name && Binding.freeVars(spec, vc)(t).contains(name) => n }.toSet

    def applyOne(m: Module, ch: Cst): Either[Rejection, Module] = ch match
      case Cst.Node(t, List(Cst.Leaf(name), term)) if t == tag(l, "add") =>
        if m.get(name).isDefined then Left(Rejection.AlreadyDefined("add", name))
        else Right(Module(m.defs :+ (name, term)))
      case Cst.Node(t, List(Cst.Leaf(name), term)) if t == tag(l, "replace") =>
        if m.get(name).isEmpty then Left(Rejection.NotDefined("replace", name))
        else Right(Module(m.defs.map((n, old) => if n == name then (n, term) else (n, old))))
      case Cst.Node(t, List(Cst.Leaf(name))) if t == tag(l, "remove") =>
        if m.get(name).isEmpty then Left(Rejection.NotDefined("remove", name))
        else
          val refs = referencing(m, name)
          if refs.nonEmpty then Left(Rejection.StillReferenced(name, refs))
          else Right(Module(m.defs.filterNot(_._1 == name)))
      case Cst.Node(t, List(Cst.Leaf(name), pathCst, term)) if t == tag(l, "edit") =>
        // M15: structural path edit — replace the subtree at child-index path
        val path = pathOf(pathCst)
        m.get(name) match
          case None => Left(Rejection.NotDefined("edit", name))
          case Some(old) =>
            replaceAt(old, path, term) match
              case Right(updated) => Right(Module(m.defs.map((n, t0) => if n == name then (n, updated) else (n, t0))))
              case Left(err)      => Left(Rejection.PathError(name, err))
      case Cst.Node(t, List(Cst.Leaf(from), Cst.Leaf(to), fp)) if t == tag(l, "rename") =>
        val declared: Set[String] = fp match
          case Cst.Node("some", List(Cst.Node("list", items))) => items.collect { case Cst.Leaf(n) => n }.toSet
          case Cst.Node("none", _) => Set.empty
          case Cst.Node("list", items) => items.collect { case Cst.Leaf(n) => n }.toSet
          case other => Set.empty
        if m.get(from).isEmpty then Left(Rejection.NotDefined("rename", from))
        else if m.get(to).isDefined then Left(Rejection.AlreadyDefined("rename-target", to))
        else
          val actual = referencing(m, from)
          if declared != actual then
            Left(Rejection.FootprintMismatch(from, declared, actual))
          else
            val spec = l.binderSpec
            val vc = l.varCtor.getOrElse("var")
            Right(Module(m.defs.map { (n, t0) =>
              val n2 = if n == from then to else n
              val t2 = if actual.contains(n) then Binding.rename(spec, vc)(t0, from, to) else t0
              (n2, t2) }))
      case other => Left(Rejection.Malformed(s"not a ΔL change term: ${other.render}"))

    val changes = change match
      case Cst.Node(t, List(Cst.Node("list", items))) if t == tag(l, "changeset") => Right(items)
      case Cst.Node(t, items) if t == tag(l, "changeset") => Right(items)
      case single @ Cst.Node(t, _) if t.startsWith("add:") || t.startsWith("replace:") ||
          t.startsWith("remove:") || t.startsWith("rename:") || t.startsWith("edit:") => Right(List(single))
      case other => Left(Rejection.Malformed(s"not a ΔL changeset: ${other.render}"))

    changes.flatMap { chs =>
      chs.foldLeft[Either[Rejection, Module]](Right(module)) { (acc, ch) => acc.flatMap(applyOne(_, ch)) }
        .map { result =>
          val vcs = ValidatedChangeSet.mint(l.digest, module.digest, change, result.digest)
          (result.sorted, vcs) }
    }

  /** Validate + apply a ΔL change-set term to a module. Structured errors;
    * no silent overwrites; renames must carry an exact footprint. See
    * [[applyTyped]] for the same check with [[Rejection]] left unstringified.
    */
  def apply(l: ComposedLanguage, module: Module, change: Cst): Either[String, (Module, ValidatedChangeSet)] =
    applyTyped(l, module, change).left.map(_.render)

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

  /** Pairs of (original leaf instance, its replacement) wherever `orig` and
    * `renamed` — same shape throughout, since `Binding.rename`/`subst` only
    * ever swaps whole `varCtor`-shaped nodes at matching positions, never
    * changes arity or tag elsewhere — differ at a leaf. Reuses the already
    * shadowing-aware `Binding.rename` instead of re-deriving that logic:
    * whatever it legitimately changed (occurrences of `from`, and any
    * incidental capture-avoidance renames) is exactly what gets spliced.
    */
  private def diffLeaves(orig: Cst, renamed: Cst): List[(Cst, Cst)] = (orig, renamed) match
    case (Cst.Leaf(a), Cst.Leaf(b)) => if a != b then List((orig, renamed)) else Nil
    case (Cst.Node(_, cs1), Cst.Node(_, cs2)) if cs1.length == cs2.length =>
      cs1.zip(cs2).flatMap(diffLeaves)
    case _ => Nil // shapes always match for a pure rename; defensive no-op

  /** Format-preserving ΔL apply (grammar-as-lens, part b): reprints only the
    * bytes an edit actually touches, splicing into the ORIGINAL source text
    * instead of [[apply]] + `Printer.print`'s whole-module canonical reprint.
    * Built entirely on the existing, independently-tested `Concrete.put`/
    * `putMany` (M7, `Grammar.scala`) — no kernel changes; the parser change
    * this needed (leaf spans for `Elem.NameLeaf`) already landed there.
    *
    * Handles `replace` / `edit` (1:1 subtree substitution — exactly what
    * `Concrete.put` does), `add` (pure insertion, no existing span to touch),
    * `rename` (the def's own name leaf plus every footprint reference,
    * located via [[diffLeaves]] against `Binding.rename`'s already-trusted,
    * shadowing-aware output, spliced together in one pass via `Concrete.putMany`),
    * and `remove` (deletes the def's own span, extended BACKWARD through its
    * own leading trivia — comment/blank-line — since a lexer's trivia belongs
    * to the token that follows it, never to the one before; the trivia
    * following the removed def, i.e. leading the NEXT def, is left untouched.
    * That convention alone avoids both an orphaned "about the thing I just
    * deleted" comment and a doubled blank line, with no separate collapse
    * heuristic needed — see the case body for a worked-through example).
    * `rename`/`remove` validate by delegating to the trusted [[apply]] first
    * (footprint exactness, still-referenced checks, etc.), so format-preserving
    * apply can never succeed where a plain apply would reject the edit;
    * `replace`/`edit`/`add`'s validation (defined/not-defined) is simple
    * enough to check directly, matching `applyOne`'s own checks.
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

          case Cst.Node(t, List(Cst.Leaf(from), Cst.Leaf(to), fpCst)) if t == tag(l, "rename") =>
            // Validate via the trusted semantic path first (footprint exactness,
            // 'to' not already defined, etc. — see apply's own rename case) —
            // reused rather than re-derived, so format-preserving rename can
            // never succeed where a plain apply would reject it.
            ModuleSurface.toModule(out.cst).flatMap { module =>
              apply(l, module, ch).left.map(e => s"ΔL rename (format-preserving): $e").flatMap { _ =>
                findDef(defs, from) match
                  case None => Left(s"ΔL rename (format-preserving): '$from' not defined")
                  case Some(Cst.Node(_, List(ownNameLeaf, _))) =>
                    val footprint = fpCst match
                      case Cst.Node("some", List(Cst.Node("list", items))) => items.collect { case Cst.Leaf(n) => n }
                      case _ => Nil
                    val vc = l.varCtor.getOrElse("var")
                    val refEdits = footprint.foldLeft[Either[String, List[(Cst, Cst)]]](Right(Nil)) { (acc, fname) =>
                      acc.flatMap { pairs =>
                        findDef(defs, fname) match
                          case Some(Cst.Node(_, List(_, fTermInstance))) =>
                            val renamedTerm = Binding.rename(l.binderSpec, vc)(fTermInstance, from, to)
                            Right(pairs ++ diffLeaves(fTermInstance, renamedTerm))
                          case _ => Left(s"ΔL rename (format-preserving): footprint '$fname' not defined")
                      }
                    }
                    refEdits.flatMap { refs =>
                      Concrete.putMany(mg, source, out, (ownNameLeaf, Cst.Leaf(to)) :: refs)
                    }
                  case Some(_) => Left(s"ΔL rename (format-preserving): malformed def '$from'")
              }
            }

          case Cst.Node(t, List(Cst.Leaf(name))) if t == tag(l, "remove") =>
            // Validated via the trusted semantic path first (still-referenced
            // check, same as apply's own remove case) — same reuse pattern as
            // rename above.
            ModuleSurface.toModule(out.cst).flatMap { module =>
              apply(l, module, ch).left.map(e => s"ΔL remove (format-preserving): $e").flatMap { _ =>
                findDef(defs, name) match
                  case None => Left(s"ΔL remove (format-preserving): '$name' not defined")
                  case Some(defNode) =>
                    out.spans.get(defNode) match
                      case None => Left("ΔL remove (format-preserving): target def has no recorded span (not from this parse)")
                      case Some((startTok, endTok)) =>
                        // Extend the deletion BACKWARD through the def's own leading
                        // trivia (its comment/blank-line, per the lexer's convention
                        // that trivia belongs to the FOLLOWING token) — but never
                        // forward into whatever follows: that trivia belongs to the
                        // NEXT def, not this one, and must survive untouched. This
                        // is what avoids leaving an orphaned "-- about the thing I
                        // just deleted" comment, or a doubled blank line, without any
                        // separate collapse heuristic.
                        val startOff =
                          if startTok == 0 then 0
                          else { val prev = out.tokens(startTok - 1); prev.offset + prev.rawLen }
                        val endOff =
                          if endTok == 0 then startOff
                          else { val last = out.tokens(endTok - 1); last.offset + last.rawLen }
                        Right(source.substring(0, startOff) + source.substring(endOff))
              }
            }

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

  /** flatten: Δ(ΔL) → ΔL — the monad multiplication μ for the recursive
    * closure `deltaOf(deltaOf(L))` forced by §2b. `deltaOf`/`apply` are
    * already fully generic over ANY `ComposedLanguage`, so applying an edit
    * to `ΔL` itself needs no new machinery to BUILD: a Δ(ΔL) `Module` is
    * just named ΔL changesets (`Delta.apply(deltaOf(L), patches, ddlEdit)`
    * already works, unmodified), and its result is already ΔL-shaped —
    * there is nothing left to "multiply" except extracting the (possibly
    * edited) changeset back out by name. That near-triviality — not a
    * missing primitive — was the actual gap: `deltaOf(deltaOf(L))` was
    * asserted in this doc comment but never exercised anywhere; see
    * `DeltaFlattenSuite` for the closed loop L ← ΔL ← Δ(ΔL).
    */
  def flatten(patched: Module, name: String): Either[String, Cst] =
    patched.get(name).toRight(s"flatten: '$name' not present in the Δ(ΔL)-patched module")
