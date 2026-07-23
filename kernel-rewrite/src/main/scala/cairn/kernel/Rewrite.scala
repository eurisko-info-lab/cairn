package cairn.kernel

/** MIGRATION-PLAN.md Phase 2 (second slice): the pattern-match/instantiate
  * core of `core.TreeEngine`'s rewrite-rule interpretation, plus the
  * independent trace checker built on it — hoisted out of `object
  * TreeEngine` (where they used to live nested) so `TraceChecker` doesn't
  * have to depend on Core's proposer/search half (`stepRoot`/`step`/
  * `normalize`) to reach them. `object Rewrite` and `TraceChecker` touch
  * only `Cst`/`ComposedLanguage`/`Digest`/`Canon`/`Artifact`/`Binding` —
  * already-kernel types — so this needed no new dependency, just relocation.
  */
object Rewrite:
  type Env = Map[String, Cst]

  def matchPattern(pattern: Cst, term: Cst, env: Env = Map.empty): Option[Env] =
    (pattern, term) match
      case (Cst.Leaf(p), _) if p.startsWith("$") =>
        env.get(p.drop(1)) match
          case Some(bound) => if bound == term then Some(env) else None
          case None        => Some(env + (p.drop(1) -> term))
      case (Cst.Leaf(p), Cst.Leaf(t)) => if p == t then Some(env) else None
      case (Cst.Node(pc, pcs), Cst.Node(tc, tcs)) if pc == tc && pcs.length == tcs.length =>
        pcs.zip(tcs).foldLeft[Option[Env]](Some(env)) { case (acc, (p, t)) =>
          acc.flatMap(matchPattern(p, t, _)) }
      case _ => None

  def instantiate(l: ComposedLanguage, template: Cst, env: Env): Either[String, Cst] =
    template match
      case Cst.Leaf(t) if t.startsWith("$") =>
        env.get(t.drop(1)).toRight(s"unbound metavariable '$t' in template")
      case Cst.Leaf(_) => Right(template)
      case Cst.Node("$subst", List(body, name, value)) =>
        for
          b <- instantiate(l, body, env)
          n <- instantiate(l, name, env)
          v <- instantiate(l, value, env)
          x <- n match
            case Cst.Leaf(x) => Right(x)
            case Cst.Node(vc, List(Cst.Leaf(x))) if l.varCtor.contains(vc) => Right(x)
            case other => Left(s"$$subst name is not a binder leaf: ${other.render}")
        yield Binding.subst(l.binderSpec, l.varCtor.getOrElse("var"))(b, x, v)
      case Cst.Node(c, cs) =>
        cs.foldLeft[Either[String, List[Cst]]](Right(Nil)) { (acc, ch) =>
          for xs <- acc; x <- instantiate(l, ch, env) yield xs :+ x
        }.map(xs => Cst.Node(c, xs))

/** M24: certified evaluation traces. */
final case class TraceStep(rule: String, path: List[Int], before: Cst, after: Cst)

final case class EvalTrace(language: Digest, initial: Cst, steps: List[TraceStep], result: Cst):
  def canon: Canon = Canon.CTag("eval-trace", Canon.cmap(
    "language" -> Canon.CStr(language.hex),
    "initial" -> Cst.toCanon(initial),
    "steps" -> Canon.CList(steps.map(s => Canon.cmap(
      "rule" -> Canon.CStr(s.rule),
      "path" -> Canon.CList(s.path.map(i => Canon.CInt(i))),
      "before" -> Cst.toCanon(s.before),
      "after" -> Cst.toCanon(s.after)))),
    "result" -> Cst.toCanon(result)))
  def artifact: Artifact = Artifact(ArtifactKind.Trace, canon)

/** M24: independent trace checker — replays every step by re-applying the
  * NAMED rule at the RECORDED path; any tampering (wrong rule, wrong site,
  * altered intermediate term) fails. Evaluation thus becomes certifiable.
  */
object TraceChecker:
  def check(l: ComposedLanguage, trace: EvalTrace): Either[String, Unit] =
    if trace.language != l.digest then Left(s"trace is for language ${trace.language.short}, not ${l.digest.short}")
    else
      def applyRuleAt(t: Cst, path: List[Int], ruleName: String): Either[String, Cst] =
        path match
          case Nil =>
            l.rewriteRules.find(_.name == ruleName).toRight(s"unknown rule '$ruleName'").flatMap { rule =>
              Rewrite.matchPattern(rule.pattern, t, Map.empty) match
                case Some(env) => Rewrite.instantiate(l, rule.template, env)
                case None      => Left(s"rule '$ruleName' does not match at recorded site: ${t.render}")
            }
          case i :: rest => t match
            case Cst.Node(c, cs) if i >= 0 && i < cs.length =>
              applyRuleAt(cs(i), rest, ruleName).map(sub => Cst.Node(c, cs.updated(i, sub)))
            case other => Left(s"trace path descends into ${other.render}")
      val zero: Either[String, Cst] = Right(trace.initial)
      trace.steps.zipWithIndex.foldLeft(zero) { case (acc, (step, i)) =>
        acc.flatMap { current =>
          if current != step.before then Left(s"step $i: recorded 'before' differs from replayed state")
          else applyRuleAt(current, step.path, step.rule).flatMap { next =>
            if next != step.after then Left(s"step $i: recorded 'after' differs from rule application")
            else Right(next) }
        }
      }.flatMap { fin =>
        if fin == trace.result then Right(())
        else Left("trace result differs from replayed final state") }
