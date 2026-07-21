package cairn.core

import cairn.kernel.*

/** Generic tree reduction driver (S15). Interprets a language's rewrite rules
  * as data; contains zero object-language-specific code. Metavariables are
  * `Leaf("$x")` in patterns; `Node("$subst",[body,name,value])` in templates
  * invokes the kernel's generic capture-avoiding substitution.
  *
  * MIGRATION-PLAN.md Phase 2 (second slice): the untrusted proposer half —
  * `matchPattern`/`instantiate` (the pure pattern machinery) and
  * `TraceStep`/`EvalTrace` (what `stepLocated`/`normalizeTraced` produce)
  * moved to `kernel.Rewrite`/`kernel` directly, since the independent
  * `kernel.TraceChecker` needs them without depending on Core.
  */
object TreeEngine:
  /** One step at the root, first matching rule wins (rules in canonical order). */
  def stepRoot(l: ComposedLanguage, term: Cst): Either[String, Option[Cst]] =
    l.rewriteRules.foldLeft[Either[String, Option[Cst]]](Right(None)) { (acc, rule) =>
      acc.flatMap {
        case some @ Some(_) => Right(some)
        case None =>
          Rewrite.matchPattern(rule.pattern, term) match
            case Some(env) => Rewrite.instantiate(l, rule.template, env).map(Some(_))
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

  /** One normal-order step that also reports WHICH rule fired WHERE. */
  def stepLocated(l: ComposedLanguage, term: Cst): Either[String, Option[(Cst, String, List[Int])]] =
    def atRoot(t: Cst): Either[String, Option[(Cst, String)]] =
      l.rewriteRules.foldLeft[Either[String, Option[(Cst, String)]]](Right(None)) { (acc, rule) =>
        acc.flatMap {
          case some @ Some(_) => Right(some)
          case None =>
            Rewrite.matchPattern(rule.pattern, t) match
              case Some(env) => Rewrite.instantiate(l, rule.template, env).map(r => Some((r, rule.name)))
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
