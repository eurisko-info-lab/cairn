package cairn.compute

import cairn.kernel.*

/** M28: threaded/compiled rule dispatch. Patterns compile ONCE to nested
  * closures (decision-tree style: constructor tags checked outside-in) and
  * rules are indexed by root tag, replacing the interpretive walk of pattern
  * data on every visit. The interpretive [[TreeEngine]] remains the certifying
  * reference; [[WaveESuite]] asserts agreement over the whole corpus.
  */
final class CompiledTreeEngine(l: ComposedLanguage):
  private type Env = Map[String, Cst]
  private type Matcher = (Cst, Env) => Option[Env]

  private def compilePattern(p: Cst): Matcher = p match
    case Cst.Leaf(m) if m.startsWith("$") =>
      val name = m.drop(1)
      (t, env) => env.get(name) match
        case Some(bound) => if bound == t then Some(env) else None
        case None        => Some(env + (name -> t))
    case Cst.Leaf(lit) =>
      (t, env) => t match
        case Cst.Leaf(x) if x == lit => Some(env)
        case _                       => None
    case Cst.Node(tag, kids) =>
      val kidMatchers = kids.map(compilePattern)
      val arity = kids.length
      (t, env) => t match
        case Cst.Node(tg, cs) if tg == tag && cs.length == arity =>
          var e: Option[Env] = Some(env)
          var i = 0
          while i < arity && e.isDefined do
            e = kidMatchers(i)(cs(i), e.get)
            i += 1
          e
        case _ => None

  private val byRootTag: Map[String, List[(RewriteRule, Matcher)]] =
    l.rewriteRules.map(r => (r, compilePattern(r.pattern))).groupBy {
      case (r, _) => r.pattern match
        case Cst.Node(tag, _) => tag
        case Cst.Leaf(x)      => x
    }

  var visits = 0L // instrumentation for benchmarks

  private def stepRoot(t: Cst): Either[String, Option[Cst]] =
    visits += 1
    val tag = t match { case Cst.Node(tg, _) => tg; case Cst.Leaf(x) => x }
    byRootTag.get(tag) match
      case None => Right(None)
      case Some(rules) =>
        rules.foldLeft[Either[String, Option[Cst]]](Right(None)) { (acc, rm) =>
          acc.flatMap {
            case some @ Some(_) => Right(some)
            case None =>
              val (rule, matcher) = rm
              matcher(t, Map.empty) match
                case Some(env) => TreeEngine.instantiate(l, rule.template, env).map(Some(_))
                case None      => Right(None)
          }
        }

  def step(term: Cst): Either[String, Option[Cst]] =
    stepRoot(term).flatMap {
      case Some(t2) => Right(Some(t2))
      case None =>
        term match
          case Cst.Leaf(_) => Right(None)
          case Cst.Node(c, cs) =>
            def loop(i: Int): Either[String, Option[Cst]] =
              if i >= cs.length then Right(None)
              else step(cs(i)).flatMap {
                case Some(ch2) => Right(Some(Cst.Node(c, cs.updated(i, ch2))))
                case None      => loop(i + 1)
              }
            loop(0)
    }

  def normalize(term: Cst, fuel: Int = 10_000): Either[String, Cst] =
    if fuel <= 0 then Left(s"out of fuel normalizing ${term.render}")
    else step(term).flatMap {
      case Some(t2) => normalize(t2, fuel - 1)
      case None     => Right(term)
    }
