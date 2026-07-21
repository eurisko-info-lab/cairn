package cairn.kernel

/** L2 proof kernel (S19–S23). MIGRATION-PLAN.md Phase 2 (first slice): the
  * independent validator half of the old `proof.Proof.scala` — Kernel, not
  * Core, because `Checker.check` never depends on the untrusted proposers
  * (`core.Search`/`core.Tactics`) that call into it.
  *
  * A [[Derivation]] is a proof term: a tree of named rule instances. The
  * independent [[Checker.check]] driver is small and decidable: for each node
  * it matches the rule's conclusion pattern against the claimed conclusion and
  * the rule's premise patterns against the sub-derivations' conclusions,
  * threading one metavariable environment (non-linear metavariables must agree).
  * No tactic engine is required to CHECK a proof (§6 Phase 2).
  *
  * Untrusted machinery (search, elaborators, UIs) may PROPOSE derivations;
  * only this checker certifies them (§4.6).
  */
final case class Derivation(rule: String, conclusion: Cst, premises: List[Derivation]):
  def canon: Canon = Canon.cmap(
    "rule" -> Canon.CStr(rule),
    "conclusion" -> Cst.toCanon(conclusion),
    "premises" -> Canon.CList(premises.map(_.canon)))
  def artifact: Artifact = Artifact(ArtifactKind.ProofTerm, canon)

object Derivation:
  def fromCanon(c: Canon): Derivation =
    Derivation(
      c.field("rule").asStr,
      Cst.fromCanon(c.field("conclusion")),
      c.field("premises").asList.map(fromCanon))

final case class CheckError(path: List[String], msg: String):
  def render: String = s"check failure at [${path.mkString(" > ")}]: $msg"

/** Checker configuration (M19/M20/M46): declarative judgments plus
  * side-condition EXTENSIONS. Extensions are injected evaluators (pure
  * functions on resolved arguments); the kernel checker remains the sole
  * certifier — extensions only answer yes/no questions it poses.
  */
final case class CheckerCfg(
    judgments: List[JudgmentDef],
    extensions: Map[String, List[Cst] => Either[String, Boolean]] = Map.empty,
    binderSpec: BinderSpec = BinderSpec(Map.empty),
    varCtor: String = "var")

object Checker:
  type Env = Map[String, Cst]

  def matchPat(pat: Cst, actual: Cst, env: Env): Option[Env] = (pat, actual) match
    case (Cst.Leaf(p), _) if p.startsWith("$") =>
      env.get(p.drop(1)) match
        case Some(bound) => if bound == actual then Some(env) else None
        case None        => Some(env + (p.drop(1) -> actual))
    case (Cst.Leaf(p), Cst.Leaf(a)) => if p == a then Some(env) else None
    case (Cst.Node(pc, ps), Cst.Node(ac, as)) if pc == ac && ps.length == as.length =>
      ps.zip(as).foldLeft[Option[Env]](Some(env)) { case (acc, (p, a)) => acc.flatMap(matchPat(p, a, _)) }
    case _ => None

  def instantiate(t: Cst, env: Env): Cst = t match
    case Cst.Leaf(p) if p.startsWith("$") => env.getOrElse(p.drop(1), t)
    case Cst.Leaf(_)                      => t
    case Cst.Node(c, cs)                  => Cst.Node(c, cs.map(instantiate(_, env)))

  def isGround(t: Cst): Boolean = t match
    case Cst.Leaf(p)     => !p.startsWith("$")
    case Cst.Node(_, cs) => cs.forall(isGround)

  /** Computational premise (M20): `$ctx-lookup(ctx, name, valuePattern)` walks
    * a `ctxCons`/`ctxNil` context with correct shadowing (first hit wins) and
    * BINDS the value pattern — no sub-derivation needed for lookups.
    */
  private def ctxLookup(args: List[Cst], env: Env, path: List[String]): Either[CheckError, Env] =
    args.map(instantiate(_, env)) match
      case List(ctx, Cst.Leaf(name), valuePat) if !name.startsWith("$") =>
        def walk(c: Cst): Either[CheckError, Env] = c match
          case Cst.Node("ctxCons", List(Cst.Leaf(n), v, rest)) =>
            if n == name then
              matchPat(args(2), v, env).toRight(CheckError(path,
                s"$$ctx-lookup: '$name' bound to ${v.render}, which does not match ${valuePat.render}"))
            else walk(rest) // shadowing: only the FIRST occurrence is visible
          case Cst.Node("ctxNil", _) => Left(CheckError(path, s"$$ctx-lookup: '$name' not in context"))
          case other => Left(CheckError(path, s"$$ctx-lookup: not a context: ${other.render}"))
        walk(ctx)
      case other => Left(CheckError(path, s"$$ctx-lookup: bad arguments ${other.map(_.render).mkString(", ")}"))

  /** Side-condition evaluation (M19). Built-ins: $neq, $fresh, $lt, $le;
    * anything else must be a registered extension.
    */
  private def condition(cfg: CheckerCfg, cond: Cst, env: Env, path: List[String]): Either[CheckError, Unit] =
    val inst = instantiate(cond, env)
    if !isGround(inst) then Left(CheckError(path, s"side condition not ground: ${inst.render}"))
    else inst match
      case Cst.Node("$neq", List(a, b)) =>
        if a != b then Right(()) else Left(CheckError(path, s"$$neq failed: ${a.render} == ${b.render}"))
      case Cst.Node("$fresh", List(Cst.Leaf(x), t)) =>
        if !Binding.freeVars(cfg.binderSpec, cfg.varCtor)(t).contains(x) then Right(())
        else Left(CheckError(path, s"$$fresh failed: '$x' occurs free in ${t.render}"))
      case Cst.Node("$lt", List(Cst.Leaf(a), Cst.Leaf(b))) =>
        if a.toLongOption.zip(b.toLongOption).exists(_ < _) then Right(())
        else Left(CheckError(path, s"$$lt failed: $a >= $b"))
      case Cst.Node("$le", List(Cst.Leaf(a), Cst.Leaf(b))) =>
        if a.toLongOption.zip(b.toLongOption).exists(_ <= _) then Right(())
        else Left(CheckError(path, s"$$le failed: $a > $b"))
      case Cst.Node(op, args) if cfg.extensions.contains(op) =>
        cfg.extensions(op)(args) match
          case Right(true)  => Right(())
          case Right(false) => Left(CheckError(path, s"$op failed on ${args.map(_.render).mkString(", ")}"))
          case Left(e)      => Left(CheckError(path, s"$op error: $e"))
      case other => Left(CheckError(path, s"unknown side condition ${other.render}"))

  def isComputational(premise: Cst): Boolean = premise match
    case Cst.Node("$ctx-lookup", _) => true
    case _                          => false

  def check(judgments: List[JudgmentDef], d: Derivation): Either[CheckError, Unit] =
    check(CheckerCfg(judgments), d)

  /** Independent, decidable check driver (S21, M19, M20). */
  def check(cfg: CheckerCfg, d: Derivation): Either[CheckError, Unit] =
    val rules: Map[String, InferRule] = cfg.judgments.flatMap(_.rules).map(r => r.name -> r).toMap
    def go(d: Derivation, path: List[String]): Either[CheckError, Unit] =
      rules.get(d.rule) match
        case None => Left(CheckError(path, s"unknown inference rule '${d.rule}'"))
        case Some(rule) =>
          val derivational = rule.premises.filterNot(isComputational)
          if derivational.length != d.premises.length then
            Left(CheckError(path, s"rule '${d.rule}' expects ${derivational.length} derivational premises, derivation has ${d.premises.length}"))
          else
            matchPat(rule.conclusion, d.conclusion, Map.empty) match
              case None => Left(CheckError(path,
                s"conclusion ${d.conclusion.render} is not an instance of rule '${d.rule}' (${rule.conclusion.render})"))
              case Some(env0) =>
                // thread the env through premises in declaration order; the
                // computational ones evaluate, the rest match sub-derivations
                var env = env0
                var subs = d.premises
                var err: Option[CheckError] = None
                for (pat, i) <- rule.premises.zipWithIndex if err.isEmpty do
                  if isComputational(pat) then
                    pat match
                      case Cst.Node("$ctx-lookup", args) =>
                        ctxLookup(args, env, path :+ s"${d.rule}#$i") match
                          case Right(env2) => env = env2
                          case Left(e)     => err = Some(e)
                      case _ => err = Some(CheckError(path, s"unhandled computational premise ${pat.render}"))
                  else
                    subs match
                      case sub :: rest =>
                        matchPat(pat, sub.conclusion, env) match
                          case Some(env2) => env = env2; subs = rest
                          case None => err = Some(CheckError(path :+ s"${d.rule}#$i",
                            s"premise ${sub.conclusion.render} is not an instance of ${pat.render} under bindings so far"))
                      case Nil => err = Some(CheckError(path, "premise/sub-derivation mismatch"))
                err match
                  case Some(e) => Left(e)
                  case None =>
                    rule.conditions.foldLeft[Either[CheckError, Unit]](Right(())) { (acc, cond) =>
                      acc.flatMap(_ => condition(cfg, cond, env, path :+ d.rule))
                    }.flatMap { _ =>
                      d.premises.zipWithIndex.foldLeft[Either[CheckError, Unit]](Right(())) {
                        case (acc, (sub, i)) => acc.flatMap(_ => go(sub, path :+ s"${d.rule}#$i")) }
                    }
    go(d, List("root"))
