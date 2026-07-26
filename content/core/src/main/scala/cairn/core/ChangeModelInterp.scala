package cairn.core

import cairn.kernel.*

/** Generic evaluators shared by [[Delta]] (grammar generation, interpretation)
  * and [[ChangeAlgebra]] (footprint, inversion) — the only place any of the
  * [[ChangeModel]] primitives are actually evaluated. Neither `Delta` nor
  * `ChangeAlgebra` pattern-matches an operation NAME anywhere after this file
  * exists; they look an operation up by tag once, then call these.
  */
object ChangeModelInterp:

  // ---- grammar generation (Delta.deltaOf) ----

  private def isAlnumToken(s: String): Boolean = s.headOption.exists(_.isLetter)

  def elemsFor(op: ChangeOpDef, termCat: String): List[Elem] =
    Elem.Tok(op.name) :: op.params.flatMap { p =>
      val prefix = p.precedingToken.map(Elem.Tok(_)).toList
      val body = p.kind match
        case ChangeParamKind.NameK => List(Elem.NameLeaf)
        case ChangeParamKind.TermK => List(Elem.Cat(termCat))
        case ChangeParamKind.PathK => List(Elem.Tok("["), Elem.Opt(Elem.SepBy1(Elem.NumLeaf, ",")), Elem.Tok("]"))
        case ChangeParamKind.FootprintK => List(Elem.Tok("["), Elem.Opt(Elem.SepBy1(Elem.NameLeaf, ",")), Elem.Tok("]"))
      prefix ++ body
    } ::: List(Elem.Tok(";"))

  def argSortFor(k: ChangeParamKind, termCat: String): String = k match
    case ChangeParamKind.NameK      => "Name"
    case ChangeParamKind.TermK      => termCat
    case ChangeParamKind.PathK      => "Path"
    case ChangeParamKind.FootprintK => "Footprint"

  def ctorDefFor(l: ComposedLanguage, op: ChangeOpDef, sortName: String, termCat: String): CtorDef =
    CtorDef(Delta.tag(l, op.name), sortName, op.params.map(p => argSortFor(p.kind, termCat)))

  def constructorSpecFor(l: ComposedLanguage, op: ChangeOpDef, termCat: String): ConstructorSpec =
    ConstructorSpec(Delta.tag(l, op.name), elemsFor(op, termCat))

  def printRuleFor(l: ComposedLanguage, op: ChangeOpDef): PrintRule =
    PrintRule(Delta.tag(l, op.name), op.printSegs)

  /** Every keyword/punct token any operation's surface syntax references,
    * partitioned the same way today's hand-written lists happen to split:
    * alphabetic-leading tokens are keywords, everything else is a punct.
    */
  def keywordsFor(model: ChangeModel): List[String] =
    model.operations.flatMap(op => op.name :: op.params.flatMap(_.precedingToken))
      .distinct.filter(isAlnumToken)

  def punctsFor(model: ChangeModel): List[String] =
    val fromParams = model.operations.flatMap(_.params.flatMap(_.precedingToken)).filterNot(isAlnumToken)
    val hasBracketed = model.operations.exists(_.params.exists(p =>
      p.kind == ChangeParamKind.PathK || p.kind == ChangeParamKind.FootprintK))
    (List("{", "}", ";") ++ fromParams ++ (if hasBracketed then List("[", "]", ",") else Nil)).distinct

  // ---- value resolution shared by the program interpreter, footprint, and inverse ----

  private final case class EvalCtx(children: List[Cst], bound: Map[String, BoundValue])

  private def internalError(msg: String): Nothing =
    throw RuntimeException(s"ChangeModel: $msg (this indicates a malformed ChangeOpDef.program, not user input)")

  private def resolveName(ctx: EvalCtx, v: ValueRef): String = v match
    case ValueRef.Param(i) => ctx.children(i) match
      case Cst.Leaf(s) => s
      case other       => internalError(s"expected Name at param $i, got $other")
    case ValueRef.Bound(n) => ctx.bound.get(n) match
      case Some(BoundValue.VName(s)) => s
      case other => internalError(s"expected bound Name '$n', got $other")

  private def resolveTerm(ctx: EvalCtx, v: ValueRef): Cst = v match
    case ValueRef.Param(i) => ctx.children(i)
    case ValueRef.Bound(n) => ctx.bound.get(n) match
      case Some(BoundValue.VTerm(t)) => t
      case other => internalError(s"expected bound Term '$n', got $other")

  private def resolvePath(ctx: EvalCtx, v: ValueRef): List[Int] = v match
    case ValueRef.Param(i) => Delta.pathOf(ctx.children(i))
    case ValueRef.Bound(n) => ctx.bound.get(n) match
      case Some(BoundValue.VPath(p)) => p
      case other => internalError(s"expected bound Path '$n', got $other")

  /** Decodes a Footprint-kind param's bracketed name list — same shape
    * `ChangeAlgebra.footprint`/`Delta.applyTyped`'s rename case have always
    * decoded independently; consolidated here as the one place that does it.
    */
  private def footprintNamesOf(c: Cst): Set[String] = c match
    case Cst.Node("some", List(Cst.Node("list", items))) => items.collect { case Cst.Leaf(n) => n }.toSet
    case Cst.Node("list", items)                         => items.collect { case Cst.Leaf(n) => n }.toSet
    case _                                                => Set.empty

  private def resolveNames(ctx: EvalCtx, v: ValueRef): Set[String] = v match
    case ValueRef.Param(i) => footprintNamesOf(ctx.children(i))
    case ValueRef.Bound(n) => ctx.bound.get(n) match
      case Some(BoundValue.VNames(s)) => s
      case other => internalError(s"expected bound Names '$n', got $other")

  private def resolveFocusSort(ctx: EvalCtx, ref: SortRef, l: ComposedLanguage): String = ref match
    case SortRef.TopSort => l.grammar.top
    case SortRef.FromBound(n) => ctx.bound.get(n) match
      case Some(BoundValue.VFocusSort(s)) => s
      case other => internalError(s"expected bound FocusSort '$n', got $other")

  /** Every OTHER def in `m` that free-references `name` — the same scan
    * `remove`/`rename` have always used, now shared.
    */
  private def referencing(l: ComposedLanguage, m: Module, name: String): Set[String] =
    val spec = l.binderSpec
    val vc = l.varCtor.getOrElse("var")
    m.defs.collect { case (n, t) if n != name && Binding.freeVars(spec, vc)(t).contains(name) => n }.toSet

  private def materializeRejection(ctx: EvalCtx, spec: RejectionSpec): Delta.Rejection = spec match
    case RejectionSpec.AlreadyDefinedR(opLabel, v) => Delta.Rejection.AlreadyDefined(opLabel, resolveName(ctx, v))
    case RejectionSpec.NotDefinedR(opLabel, v)      => Delta.Rejection.NotDefined(opLabel, resolveName(ctx, v))
    case RejectionSpec.StillReferencedR(name, refs) =>
      Delta.Rejection.StillReferenced(resolveName(ctx, name), resolveNames(ctx, refs))
    case RejectionSpec.FootprintMismatchR(name, declared, actual) =>
      Delta.Rejection.FootprintMismatch(resolveName(ctx, name), resolveNames(ctx, declared), resolveNames(ctx, actual))

  private def evalBool(ctx: EvalCtx, m: Module, e: BoolExpr): Boolean = e match
    case BoolExpr.IsDefined(v)      => m.get(resolveName(ctx, v)).isDefined
    case BoolExpr.Not(inner)        => !evalBool(ctx, m, inner)
    case BoolExpr.NamesEmpty(v)     => resolveNames(ctx, v).isEmpty
    case BoolExpr.NamesEqual(a, b)  => resolveNames(ctx, a) == resolveNames(ctx, b)

  private def evalQuery(l: ComposedLanguage, m: Module, ctx: EvalCtx, q: ChangeQuery): Either[Delta.Rejection, BoundValue] = q match
    case ChangeQuery.ReferencingNames(nameRef) =>
      Right(BoundValue.VNames(referencing(l, m, resolveName(ctx, nameRef))))
    case ChangeQuery.ReadTerm(nameRef) =>
      val name = resolveName(ctx, nameRef)
      m.get(name) match
        case Some(t) => Right(BoundValue.VTerm(t))
        case None    => internalError(s"ReadTerm on undefined '$name' (missing prior IsDefined check)")
    case ChangeQuery.ResolveSemanticPath(nameRef, pathRef) =>
      val name = resolveName(ctx, nameRef)
      val path = resolvePath(ctx, pathRef)
      m.get(name) match
        case None => internalError(s"ResolveSemanticPath on undefined '$name' (missing prior IsDefined check)")
        case Some(old) =>
          SemanticPath.fromLegacyPath(l, old, path) match
            case Right(sp) => Right(BoundValue.VFocusSort(sp.focusSort))
            case Left(e)   => Left(Delta.Rejection.PathError(name, e))

  private def evalMutation(l: ComposedLanguage, m: Module, ctx: EvalCtx, mut: Mutation): Either[Delta.Rejection, Module] = mut match
    case Mutation.InsertDef(nameRef, termRef) =>
      Right(Module(m.defs :+ (resolveName(ctx, nameRef), resolveTerm(ctx, termRef))))
    case Mutation.ReplaceDef(nameRef, termRef) =>
      val name = resolveName(ctx, nameRef); val term = resolveTerm(ctx, termRef)
      Right(Module(m.defs.map((n, old) => if n == name then (n, term) else (n, old))))
    case Mutation.DeleteDef(nameRef) =>
      Right(Module(m.defs.filterNot(_._1 == resolveName(ctx, nameRef))))
    case Mutation.ReplaceSubtreeAt(nameRef, pathRef, termRef) =>
      val name = resolveName(ctx, nameRef); val path = resolvePath(ctx, pathRef); val term = resolveTerm(ctx, termRef)
      m.get(name) match
        case None => internalError(s"ReplaceSubtreeAt on undefined '$name' (missing prior IsDefined check)")
        case Some(old) =>
          Delta.replaceAt(old, path, term) match
            case Right(updated) => Right(Module(m.defs.map((n, t0) => if n == name then (n, updated) else (n, t0))))
            case Left(e)        => Left(Delta.Rejection.PathError(name, e))
    case Mutation.RenameOccurrences(fromRef, toRef, footprintRef) =>
      val from = resolveName(ctx, fromRef); val to = resolveName(ctx, toRef); val actual = resolveNames(ctx, footprintRef)
      val spec = l.binderSpec; val vc = l.varCtor.getOrElse("var")
      Right(Module(m.defs.map { (n, t0) =>
        val n2 = if n == from then to else n
        val t2 = if actual.contains(n) then Binding.rename(spec, vc)(t0, from, to) else t0
        (n2, t2)
      }))

  /** Runs `op`'s program against `module`, given the parsed change term's raw
    * children. The one place that walks a [[ChangeOpDef.program]] generically
    * — no operation name is ever inspected here, only `ChangeStep` shapes.
    */
  def run(l: ComposedLanguage, module: Module, op: ChangeOpDef, children: List[Cst]): Either[Delta.Rejection, Module] =
    def go(steps: List[ChangeStep], m: Module, ctx: EvalCtx): Either[Delta.Rejection, Module] =
      steps match
        case Nil => Right(m)
        case ChangeStep.Check(require, onFail) :: rest =>
          if evalBool(ctx, m, require) then go(rest, m, ctx)
          else Left(materializeRejection(ctx, onFail))
        case ChangeStep.CheckTerm(termRef, sortRef, nameRef) :: rest =>
          val term = resolveTerm(ctx, termRef)
          val sort = resolveFocusSort(ctx, sortRef, l)
          LanguageChecker.checkTerm(l, sort, term) match
            case Right(_)      => go(rest, m, ctx)
            case Left(errors) => Left(Delta.Rejection.InvalidTerm(resolveName(ctx, nameRef), errors))
        case ChangeStep.Bind(as, query) :: rest =>
          evalQuery(l, m, ctx, query) match
            case Right(bv)  => go(rest, m, ctx.copy(bound = ctx.bound + (as -> bv)))
            case Left(rej)  => Left(rej)
        case ChangeStep.Mutate(mutation) :: rest =>
          evalMutation(l, m, ctx, mutation) match
            case Right(m2) => go(rest, m2, ctx)
            case Left(rej) => Left(rej)
    go(op.program, module, EvalCtx(children, Map.empty))

  /** Purely syntactic over the parsed change term's raw children — no module
    * needed, matching [[ChangeAlgebra.footprint]]'s current nature.
    */
  def footprintOf(op: ChangeOpDef, children: List[Cst]): Set[String] =
    def go(e: FootprintExpr): Set[String] = e match
      case FootprintExpr.NameOf(i) => children(i) match
        case Cst.Leaf(s) => Set(s)
        case _           => Set.empty
      case FootprintExpr.NamesOf(i)  => footprintNamesOf(children(i))
      case FootprintExpr.Union(parts) => parts.flatMap(go).toSet
    go(op.footprint)

  /** Builds `op`'s inverse change term against `m` — the PRE-state module,
    * i.e. before `op` itself has been applied (matches [[ChangeAlgebra.invert]]'s
    * existing forward-fold-and-collect walk, which reads old state before
    * advancing).
    */
  def inverseOf(l: ComposedLanguage, model: ChangeModel, op: ChangeOpDef, children: List[Cst], m: Module): Either[String, Cst] =
    model.find(op.inverse.targetOp) match
      case None => Left(s"ChangeModel: inverse target op '${op.inverse.targetOp}' not found in model")
      case Some(targetOp) =>
        op.inverse.args.foldLeft[Either[String, List[Cst]]](Right(Nil)) { (acc, a) =>
          acc.flatMap { xs =>
            a match
              case InverseArg.Copy(i) => Right(xs :+ children(i))
              case InverseArg.LookupOld(nameParam) =>
                val name = children(nameParam) match
                  case Cst.Leaf(s) => s
                  case other       => internalError(s"expected Name at param $nameParam, got $other")
                m.get(name).toRight(s"invert ${op.name}: '$name' not in module").map(t => xs :+ t)
              case InverseArg.SubtreeAtOld(nameParam, pathParam) =>
                val name = children(nameParam) match
                  case Cst.Leaf(s) => s
                  case other       => internalError(s"expected Name at param $nameParam, got $other")
                val path = Delta.pathOf(children(pathParam))
                for
                  old <- m.get(name).toRight(s"invert ${op.name}: '$name' not in module")
                  sp  <- SemanticPath.fromLegacyPath(l, old, path)
                  sub <- Delta.subtreeAt(old, sp.indices)
                yield xs :+ sub
          }
        }.map(args => Cst.Node(Delta.tag(l, targetOp.name), args))
