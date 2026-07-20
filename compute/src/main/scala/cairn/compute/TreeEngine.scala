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
