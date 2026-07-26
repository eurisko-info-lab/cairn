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

  /** `model.operations` drives the change-term category's `CtorDef`s,
    * `ConstructorSpec`s, and `PrintRule`s generically ([[ChangeModelInterp]])
    * — adding an operation is adding [[ChangeOpDef]] data to a `model`, not
    * editing this method. The `changeset` wrapper (list-of-changes syntax)
    * stays hand-written: it's structural, not one of the operations.
    */
  def deltaOf(l: ComposedLanguage, model: ChangeModel = ChangeModel.default): Either[List[ComposeError], ComposedLanguage] =
    val p = l.name
    // termCat: the grammar CATEGORY name (e.g. "term", or Search's
    // "searchObj") — used both for Elem.Cat(termCat) in the ΔL grammar below
    // (which must reference a real CategorySpec name) AND, deliberately, as
    // the CtorDefs' argSorts for add/replace/edit: LanguageChecker.checkTerm
    // interprets a grammar-category argSort by tag membership rather than by
    // resolving it to one semantic sort, since a category can legitimately
    // span multiple sorts (Search's "searchObj" admits both "Fact"- and
    // "Intent"-sorted constructors — there is no single correct sort name to
    // put here).
    val termCat = l.grammar.top
    val chg = s"Δ$p.change"
    val chgs = s"Δ$p.changeset"
    val changeSort = s"Δ$p.Change"
    val opCtors = model.operations.map(op => ChangeModelInterp.ctorDefFor(l, op, changeSort, termCat))
    val opSpecs = model.operations.map(op => ChangeModelInterp.constructorSpecFor(l, op, termCat))
    val opPrintRules = model.operations.map(op => ChangeModelInterp.printRuleFor(l, op))
    val deltaFrag = Fragment(
      name = s"Δ:$p",
      provides = List(s"Δ$p"),
      requires = Nil,
      sorts = List(SortDef(changeSort, SortMode.Tree), SortDef(s"Δ$p.ChangeSet", SortMode.Tree)),
      constructors = CtorDef(tag(l, "changeset"), s"Δ$p.ChangeSet", List(changeSort)) :: opCtors,
      grammar = GrammarPart(
        keywords = ChangeModelInterp.keywordsFor(model),
        puncts = ChangeModelInterp.punctsFor(model),
        categories = List(
          CategorySpec(chgs, List(
            ConstructorSpec(tag(l, "changeset"), List(
              Elem.Tok("{"), Elem.Star(Elem.Cat(chg)), Elem.Tok("}"))))),
          CategorySpec(chg, opSpecs)),
        printRules = PrintRule(tag(l, "changeset"), List(
          PrintSeg.Lit("{"), PrintSeg.Newline, PrintSeg.IndentIn,
          PrintSeg.SepFields(0, "\n"), PrintSeg.Newline,
          PrintSeg.IndentOut, PrintSeg.Lit("}"))) :: opPrintRules,
        top = Some(chgs)))
    // Base language fragments contribute their grammar but yield the top slot to ΔL.
    val demoted = l.fragments.map(f => f.copy(grammar = f.grammar.copy(top = None)))
    Compose.compose(s"Δ${l.name}", demoted :+ deltaFrag)

  def deltaOf(l: ComposedLanguage, capability: ChangeCapability): Either[List[ComposeError], ComposedLanguage] =
    capability.model match
      case Left(e)      => Left(List(ComposeError("change-capability", "semantics", "surface", e)))
      case Right(model) => deltaOf(l, model)

  /** Kernel-gated record of an applied change-set. Opaque: mint only via
    * [[apply]] / [[applyTyped]], or [[ValidatedChangeSet.check]] after replay.
    * Public [[ValidatedChangeSet.decodeClaim]] does not mint — forged canon
    * cannot become a [[ValidatedChangeSet]] without `apply(language, base, change) = result`.
    */
  opaque type ValidatedChangeSet = ValidatedChangeSet.Repr
  object ValidatedChangeSet:
    private[Delta] final case class Repr(
        language: Digest, base: Digest, change: Cst, result: Digest, changeModel: Digest)

    private[Delta] def mint(
        language: Digest, base: Digest, change: Cst, result: Digest, changeModel: Digest
    ): ValidatedChangeSet =
      Repr(language, base, change, result, changeModel)

    /** Unchecked fields decoded from canon — not a validated change-set. */
    final case class Claim(language: Digest, base: Digest, change: Cst, result: Digest, changeModel: Digest):
      def canon: Canon = Canon.cmap(
        "language" -> Canon.CStr(language.hex),
        "base" -> Canon.CStr(base.hex),
        "change" -> Cst.toCanon(change),
        "result" -> Canon.CStr(result.hex),
        "changeModel" -> Canon.CStr(changeModel.hex))

    /** `changeModel` defaults to [[ChangeModel.default]]'s digest when absent
      * — canon minted before this field existed never recorded a model.
      */
    def decodeClaim(c: Canon): Claim =
      Claim(
        Digest(c.field("language").asStr),
        Digest(c.field("base").asStr),
        Cst.fromCanon(c.field("change")),
        Digest(c.field("result").asStr),
        c.asMap.get("changeModel").map(v => Digest(v.asStr)).getOrElse(ChangeModel.default.digest))

    /** Replay [[apply]]; accept only when the model and result digest match
      * the claim. The model check runs BEFORE replay — same reason the
      * language check does: both are "does this claim even belong to this
      * grammar/semantics" checks against caller-supplied static values,
      * cheaper and more informative than letting a wrong model fail deep
      * inside `apply` (or, worse, silently succeed under different semantics
      * that happen to share tag names).
      */
    def check(
        l: ComposedLanguage, model: ChangeModel, baseMod: Module, claim: Claim
    ): Either[String, ValidatedChangeSet] =
      if l.digest != claim.language then
        Left(s"ValidatedChangeSet language mismatch: claim ${claim.language.short} ≠ ${l.digest.short}")
      else if model.digest != claim.changeModel then
        Left(s"ValidatedChangeSet model mismatch: claim ${claim.changeModel.short} ≠ ${model.digest.short}")
      else if baseMod.digest != claim.base then
        Left(s"ValidatedChangeSet base mismatch: claim ${claim.base.short} ≠ ${baseMod.digest.short}")
      else
        apply(l, baseMod, claim.change, model).flatMap { (result, vcs) =>
          if result.digest != claim.result then
            Left(s"forged ValidatedChangeSet: claimed result ${claim.result.short}, apply yielded ${result.digest.short}")
          else Right(vcs)
        }

    extension (v: ValidatedChangeSet)
      def language: Digest = v.language
      def base: Digest = v.base
      def change: Cst = v.change
      def result: Digest = v.result
      def changeModel: Digest = v.changeModel
      def canon: Canon = Claim(v.language, v.base, v.change, v.result, v.changeModel).canon
      def artifact: Artifact = Artifact(ArtifactKind.ChangeSet, v.canon)
      def claim: Claim = Claim(v.language, v.base, v.change, v.result, v.changeModel)


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
    case InvalidTerm(name: String, errors: List[LanguageChecker.TermError])

    def canon: Canon = this match
      case AlreadyDefined(op, name) =>
        Canon.CTag("already-defined", Canon.cmap("op" -> Canon.CStr(op), "name" -> Canon.CStr(name)))
      case NotDefined(op, name) =>
        Canon.CTag("not-defined", Canon.cmap("op" -> Canon.CStr(op), "name" -> Canon.CStr(name)))
      case StillReferenced(name, by) =>
        Canon.CTag("still-referenced", Canon.cmap(
          "name" -> Canon.CStr(name),
          "by" -> Canon.cstrs(by.toList.sorted)))
      case FootprintMismatch(name, declared, actual) =>
        Canon.CTag("footprint-mismatch", Canon.cmap(
          "name" -> Canon.CStr(name),
          "declared" -> Canon.cstrs(declared.toList.sorted),
          "actual" -> Canon.cstrs(actual.toList.sorted)))
      case PathError(name, detail) =>
        Canon.CTag("path-error", Canon.cmap("name" -> Canon.CStr(name), "detail" -> Canon.CStr(detail)))
      case Malformed(detail) =>
        Canon.CTag("malformed", Canon.CStr(detail))
      case InvalidTerm(name, errors) =>
        Canon.CTag("invalid-term", Canon.cmap(
          "name" -> Canon.CStr(name),
          "errors" -> Canon.CList(errors.map(_.canon))))

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
      case InvalidTerm(name, errors) =>
        s"ΔL '$name': invalid term (${errors.map(_.render).mkString("; ")})"

  /** [[apply]], but with [[Rejection]] left unstringified — the typed view.
    * `applyOne` looks an item's [[ChangeOpDef]] up by tag once, then runs its
    * `program` via [[ChangeModelInterp.run]] — no operation-name switch here
    * any more; adding an operation to `model` needs no change to this method.
    */
  def applyTyped(
      l: ComposedLanguage, module: Module, change: Cst, model: ChangeModel = ChangeModel.default,
  ): Either[Rejection, (Module, ValidatedChangeSet)] =
    def applyOne(m: Module, ch: Cst): Either[Rejection, Module] = ch match
      case Cst.Node(t, children) if model.operations.exists(o => t == tag(l, o.name)) =>
        val op = model.operations.find(o => t == tag(l, o.name)).get
        ChangeModelInterp.run(l, m, op, children)
      case other => Left(Rejection.Malformed(s"not a ΔL change term: ${other.render}"))

    val changes = change match
      case Cst.Node(t, List(Cst.Node("list", items))) if t == tag(l, "changeset") => Right(items)
      case Cst.Node(t, items) if t == tag(l, "changeset") => Right(items)
      case single @ Cst.Node(t, _) if model.operations.exists(o => t == tag(l, o.name)) => Right(List(single))
      case other => Left(Rejection.Malformed(s"not a ΔL changeset: ${other.render}"))

    changes.flatMap { chs =>
      chs.foldLeft[Either[Rejection, Module]](Right(module)) { (acc, ch) => acc.flatMap(applyOne(_, ch)) }
        .map { result =>
          val vcs = ValidatedChangeSet.mint(l.digest, module.digest, change, result.digest, model.digest)
          (result.sorted, vcs) }
    }

  /** Validate + apply a ΔL change-set term to a module. Structured errors;
    * no silent overwrites; renames must carry an exact footprint. See
    * [[applyTyped]] for the same check with [[Rejection]] left unstringified.
    */
  def apply(
      l: ComposedLanguage, module: Module, change: Cst, model: ChangeModel = ChangeModel.default,
  ): Either[String, (Module, ValidatedChangeSet)] =
    applyTyped(l, module, change, model).left.map(_.render)

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
    * Built on the existing, independently-tested `Concrete.splice`/`putMany`
    * offset math (M7, `Grammar.scala`) — no kernel changes.
    *
    * Runs the SAME generic interpreter [[applyTyped]] does
    * ([[ChangeModelInterp.runTraced]]) against `model`'s operations — no
    * per-operation-name switch here, and no separate validation pass either:
    * every check (defined/not-defined, footprint exactness, still-referenced,
    * ...) happens once, inside the op's own `program`, exactly as it does for
    * ordinary (non-format-preserving) apply. The resulting resolved
    * [[AppliedMutation]] trace is then spliced against the parsed source via
    * [[mutationEdits]]/[[applyEdits]], which switch on the fixed, never-growing
    * 5-case trace vocabulary — never on an operation name — so a data-defined
    * operation (e.g. a `copy` op resolving to `InsertedDef`) is format-preserving
    * automatically, with no change to this method.
    */
  def applyPreservingFormat(l: ComposedLanguage, moduleGrammar: GrammarSpec,
                            source: String, change: Cst, model: ChangeModel = ChangeModel.default): Either[String, String] =
    changeItems(l, change).flatMap { chs =>
      chs.foldLeft[Either[String, String]](Right(source)) { (acc, ch) =>
        acc.flatMap(applyOnePreserving(l, moduleGrammar, model, _, ch))
      }
    }

  private def applyOnePreserving(l: ComposedLanguage, mg: GrammarSpec, model: ChangeModel, source: String, ch: Cst): Either[String, String] =
    Parser.parseFull(mg, source).flatMap { out =>
      moduleDefs(out.cst).flatMap { defs =>
        ModuleSurface.toModule(out.cst).flatMap { module =>
          ch match
            case Cst.Node(t, children) if model.operations.exists(o => t == tag(l, o.name)) =>
              val op = model.operations.find(o => t == tag(l, o.name)).get
              ChangeModelInterp.runTraced(l, module, op, children).left.map(_.render).flatMap { applied =>
                mutationEdits(l, mg, out, defs, module, source, applied.trace).flatMap(applyEdits(source, _))
              }
            case other => Left(s"not a ΔL change term: ${other.render}")
        }
      }
    }

  /** `(startOffset, endOffset, replacementText)` — a `Concrete.splice`-shaped
    * edit computed once from the ORIGINAL parse (`out.tokens`/`out.spans`),
    * generalized to also cover pure insertion (a zero-width range) and
    * trivia-aware deletion (an empty replacement), so every [[AppliedMutation]]
    * case reduces to the same shape and [[applyEdits]] can combine them in one
    * pass, exactly like `Concrete.putMany` does for ordinary replacements.
    */
  private def spanOffsets(out: ParseOut, startTok: Int, endTok: Int): (Int, Int) =
    val startOff = out.tokens(startTok).offset
    val endOff =
      if endTok == 0 then startOff
      else { val last = out.tokens(endTok - 1); last.offset + last.rawLen }
    (startOff, endOff)

  private def mutationEdits(
      l: ComposedLanguage, mg: GrammarSpec, out: ParseOut, defs: List[Cst], module: Module, source: String,
      trace: List[AppliedMutation],
  ): Either[String, List[(Int, Int, String)]] =
    trace.foldLeft[Either[String, List[(Int, Int, String)]]](Right(Nil)) { (acc, mut) =>
      acc.flatMap(xs => mutationEdit(l, mg, out, defs, module, source, mut).map(xs ++ _))
    }

  private def mutationEdit(
      l: ComposedLanguage, mg: GrammarSpec, out: ParseOut, defs: List[Cst], module: Module, source: String,
      mut: AppliedMutation,
  ): Either[String, List[(Int, Int, String)]] = mut match

    case AppliedMutation.InsertedDef(name, term) =>
      Printer.print(mg, Cst.node("moduleDef", Cst.Leaf(name), term)).map { printedDef =>
        val real = out.tokens.filter(_.kind != TokKind.Eof)
        val insertAt = real.lastOption.fold(0)(t => t.offset + t.rawLen)
        val prefix = source.substring(0, insertAt)
        val sep = if prefix.isEmpty || prefix.endsWith("\n") then "" else "\n"
        List((insertAt, insertAt, sep + printedDef + "\n"))
      }

    case AppliedMutation.ReplacedDef(name, oldTerm, newTerm) =>
      out.spans.get(oldTerm) match
        case None => Left(s"ΔL replace (format-preserving): '$name' has no recorded span (not from this parse)")
        case Some((startTok, endTok)) =>
          Printer.print(mg, newTerm).map { printed =>
            val (startOff, endOff) = spanOffsets(out, startTok, endTok)
            List((startOff, endOff, printed))
          }

    case AppliedMutation.ReplacedSubtree(name, _, oldSubtree, newSubtree) =>
      out.spans.get(oldSubtree) match
        case None => Left(s"ΔL edit (format-preserving): '$name' subtree has no recorded span (not from this parse)")
        case Some((startTok, endTok)) =>
          Printer.print(mg, newSubtree).map { printed =>
            val (startOff, endOff) = spanOffsets(out, startTok, endTok)
            List((startOff, endOff, printed))
          }

    case AppliedMutation.DeletedDef(name, _) =>
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
              // NEXT def, not this one, and must survive untouched. This is
              // what avoids leaving an orphaned "-- about the thing I just
              // deleted" comment, or a doubled blank line, without any
              // separate collapse heuristic.
              val startOff =
                if startTok == 0 then 0
                else { val prev = out.tokens(startTok - 1); prev.offset + prev.rawLen }
              val endOff =
                if endTok == 0 then startOff
                else { val last = out.tokens(endTok - 1); last.offset + last.rawLen }
              Right(List((startOff, endOff, "")))

    case AppliedMutation.RenamedOccurrences(from, to, affectedDefs) =>
      findDef(defs, from) match
        case None => Left(s"ΔL rename (format-preserving): '$from' not defined")
        case Some(Cst.Node(_, List(ownNameLeaf, _))) =>
          val vc = l.varCtor.getOrElse("var")
          val refPairs = affectedDefs.toList.sorted.foldLeft[Either[String, List[(Cst, Cst)]]](Right(Nil)) { (acc, fname) =>
            acc.flatMap { pairs =>
              module.get(fname) match
                case Some(fTermInstance) =>
                  val renamedTerm = Binding.rename(l.binderSpec, vc)(fTermInstance, from, to)
                  Right(pairs ++ diffLeaves(fTermInstance, renamedTerm))
                case None => Left(s"ΔL rename (format-preserving): footprint '$fname' not defined")
            }
          }
          refPairs.flatMap { refs =>
            ((ownNameLeaf, Cst.Leaf(to)) :: refs).foldLeft[Either[String, List[(Int, Int, String)]]](Right(Nil)) {
              case (acc, (target, replacement)) =>
                acc.flatMap { xs =>
                  out.spans.get(target) match
                    case None => Left("ΔL rename (format-preserving): target has no recorded span (not from this parse)")
                    case Some((startTok, endTok)) =>
                      Printer.print(mg, replacement).map { printed =>
                        val (startOff, endOff) = spanOffsets(out, startTok, endTok)
                        xs :+ (startOff, endOff, printed)
                      }
                }
            }
          }
        case Some(_) => Left(s"ΔL rename (format-preserving): malformed def '$from'")

  /** Combine every `(start,end,text)` edit against the SAME original parse in
    * ONE pass — generalizing `Concrete.putMany`'s rightmost-first fold
    * (`putMany` sorts by token index; here the sort key is the actual byte
    * offset, since a pure insertion has no token index of its own). Adjacent
    * edits (`e1 == s2`) compose correctly by the same invariant `putMany`
    * relies on; only genuinely OVERLAPPING edits (`e1 > s2`), which none of
    * the default operations or `copy` ever produce, are rejected rather than
    * silently corrupting text.
    */
  private def applyEdits(source: String, edits: List[(Int, Int, String)]): Either[String, String] =
    val sorted = edits.sortBy(_._1)
    val overlap = sorted.zip(sorted.tail).collectFirst {
      case ((s1, e1, _), (s2, e2, _)) if e1 > s2 => (s1, e1, s2, e2)
    }
    overlap match
      case Some((s1, e1, s2, e2)) =>
        Left(s"format-preserving apply: overlapping edits [$s1,$e1) and [$s2,$e2)")
      case None =>
        val ordered = sorted.sortBy(-_._1) // rightmost (highest start offset) first
        Right(ordered.foldLeft(source) { case (acc, (s, e, text)) => acc.substring(0, s) + text + acc.substring(e) })

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
