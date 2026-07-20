package cairn.compute

import cairn.kernel.*

/** Generic tree reduction driver (S15). Interprets a language's rewrite rules
  * as data; contains zero object-language-specific code. Metavariables are
  * `Leaf("$x")` in patterns; `Node("$subst",[body,name,value])` in templates
  * invokes the kernel's generic capture-avoiding substitution.
  */
object TreeEngine:
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

  /** One step at the root, first matching rule wins (rules in canonical order). */
  def stepRoot(l: ComposedLanguage, term: Cst): Either[String, Option[Cst]] =
    l.rewriteRules.foldLeft[Either[String, Option[Cst]]](Right(None)) { (acc, rule) =>
      acc.flatMap {
        case some @ Some(_) => Right(some)
        case None =>
          matchPattern(rule.pattern, term) match
            case Some(env) => instantiate(l, rule.template, env).map(Some(_))
            case None      => Right(None)
      }
    }

  /** Normal-order (leftmost-outermost) single step anywhere in the term. */
  def step(l: ComposedLanguage, term: Cst): Either[String, Option[Cst]] =
    stepRoot(l, term).flatMap {
      case Some(t2) => Right(Some(t2))
      case None =>
        term match
          case Cst.Leaf(_) => Right(None)
          case Cst.Node(c, cs) =>
            def loop(i: Int): Either[String, Option[Cst]] =
              if i >= cs.length then Right(None)
              else step(l, cs(i)).flatMap {
                case Some(ch2) => Right(Some(Cst.Node(c, cs.updated(i, ch2))))
                case None      => loop(i + 1)
              }
            loop(0)
    }

  /** Fuel-bounded normalization. */
  def normalize(l: ComposedLanguage, term: Cst, fuel: Int = 10_000): Either[String, Cst] =
    if fuel <= 0 then Left(s"out of fuel normalizing ${term.render}")
    else step(l, term).flatMap {
      case Some(t2) => normalize(l, t2, fuel - 1)
      case None     => Right(term)
    }

  // ---- M24: certified evaluation traces ----

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

  /** One normal-order step that also reports WHICH rule fired WHERE. */
  def stepLocated(l: ComposedLanguage, term: Cst): Either[String, Option[(Cst, String, List[Int])]] =
    def atRoot(t: Cst): Either[String, Option[(Cst, String)]] =
      l.rewriteRules.foldLeft[Either[String, Option[(Cst, String)]]](Right(None)) { (acc, rule) =>
        acc.flatMap {
          case some @ Some(_) => Right(some)
          case None =>
            matchPattern(rule.pattern, t) match
              case Some(env) => instantiate(l, rule.template, env).map(r => Some((r, rule.name)))
              case None      => Right(None)
        }
      }
    def go(t: Cst, path: List[Int]): Either[String, Option[(Cst, String, List[Int])]] =
      atRoot(t).flatMap {
        case Some((t2, rule)) => Right(Some((t2, rule, path.reverse)))
        case None =>
          t match
            case Cst.Leaf(_) => Right(None)
            case Cst.Node(c, cs) =>
              def loop(i: Int): Either[String, Option[(Cst, String, List[Int])]] =
                if i >= cs.length then Right(None)
                else go(cs(i), i :: path).flatMap {
                  case Some((ch2, rule, p)) =>
                    Right(Some((Cst.Node(c, cs.updated(i, ch2)), rule, p)))
                  case None => loop(i + 1)
                }
              loop(0)
      }
    go(term, Nil)

  /** Normalize while recording a checkable trace (M24). */
  def normalizeTraced(l: ComposedLanguage, term: Cst, fuel: Int = 10_000): Either[String, (Cst, EvalTrace)] =
    def loop(t: Cst, steps: List[TraceStep], fuel: Int): Either[String, (Cst, List[TraceStep])] =
      if fuel <= 0 then Left(s"out of fuel normalizing ${term.render}")
      else stepLocated(l, t).flatMap {
        case Some((t2, rule, path)) => loop(t2, steps :+ TraceStep(rule, path, t, t2), fuel - 1)
        case None                   => Right((t, steps))
      }
    loop(term, Nil, fuel).map((result, steps) =>
      (result, EvalTrace(l.digest, term, steps, result)))

/** M24: independent trace checker — replays every step by re-applying the
  * NAMED rule at the RECORDED path; any tampering (wrong rule, wrong site,
  * altered intermediate term) fails. Evaluation thus becomes certifiable.
  */
object TraceChecker:
  import TreeEngine.EvalTrace

  def check(l: ComposedLanguage, trace: EvalTrace): Either[String, Unit] =
    if trace.language != l.digest then Left(s"trace is for language ${trace.language.short}, not ${l.digest.short}")
    else
      def applyRuleAt(t: Cst, path: List[Int], ruleName: String): Either[String, Cst] =
        path match
          case Nil =>
            l.rewriteRules.find(_.name == ruleName).toRight(s"unknown rule '$ruleName'").flatMap { rule =>
              TreeEngine.matchPattern(rule.pattern, t, Map.empty) match
                case Some(env) => TreeEngine.instantiate(l, rule.template, env)
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
