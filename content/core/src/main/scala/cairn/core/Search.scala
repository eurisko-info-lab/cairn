package cairn.core

import cairn.kernel.*

/** M21: bounded, syntax-directed derivation SEARCH (type inference). An
  * untrusted proposer: whatever it finds must still pass [[Checker.check]].
  * MIGRATION-PLAN.md Phase 2 (first slice): Core, not Kernel — calls into
  * `kernel.Checker` (`isComputational`/`isGround`), never the reverse.
  */
object Search:
  private type Subst = Map[String, Cst]

  private def resolve(t: Cst, s: Subst): Cst = t match
    case Cst.Leaf(p) if p.startsWith("$") =>
      s.get(p.drop(1)).map(resolve(_, s)).getOrElse(t)
    case Cst.Leaf(_)     => t
    case Cst.Node(c, cs) => Cst.Node(c, cs.map(resolve(_, s)))

  private def unify(a: Cst, b: Cst, s: Subst): Option[Subst] =
    (resolve(a, s), resolve(b, s)) match
      case (x, y) if x == y => Some(s)
      case (Cst.Leaf(p), y) if p.startsWith("$") => Some(s + (p.drop(1) -> y))
      case (x, Cst.Leaf(p)) if p.startsWith("$") => Some(s + (p.drop(1) -> x))
      case (Cst.Node(c1, cs1), Cst.Node(c2, cs2)) if c1 == c2 && cs1.length == cs2.length =>
        cs1.zip(cs2).foldLeft[Option[Subst]](Some(s)) { case (acc, (x, y)) => acc.flatMap(unify(x, y, _)) }
      case _ => None

  private var freshCounter = 0
  private def freshen(t: Cst, suffix: String): Cst = t match
    case Cst.Leaf(p) if p.startsWith("$") => Cst.Leaf(p + suffix)
    case Cst.Leaf(_)                      => t
    case Cst.Node(c, cs)                  => Cst.Node(c, cs.map(freshen(_, suffix)))

  def infer(cfg: CheckerCfg, goal: Cst, depth: Int = 64): Either[String, Derivation] =
    val rules = cfg.judgments.flatMap(_.rules)
    def solve(goal: Cst, s: Subst, d: Int): Option[(Derivation, Subst)] =
      if d <= 0 then None
      else
        val g = resolve(goal, s)
        rules.view.flatMap { rule =>
          freshCounter += 1
          val fx = s"?$freshCounter"
          val conclusion = freshen(rule.conclusion, fx)
          unify(conclusion, g, s).flatMap { s1 =>
            // premises in order: computational evaluate, others recurse
            val start: Option[(List[Derivation], Subst)] = Some((Nil, s1))
            rule.premises.foldLeft(start) { (acc, prem) =>
              acc.flatMap { (subs, sAcc) =>
                val p = freshen(prem, fx)
                if Checker.isComputational(p) then
                  p match
                    case Cst.Node("$ctx-lookup", List(ctxP, nameP, valP)) =>
                      val ctx = resolve(ctxP, sAcc)
                      resolve(nameP, sAcc) match
                        case Cst.Leaf(n) if !n.startsWith("$") =>
                          def walk(c: Cst): Option[Subst] = c match
                            case Cst.Node("ctxCons", List(Cst.Leaf(m), v, rest)) =>
                              if m == n then unify(valP, v, sAcc) else walk(rest)
                            case _ => None
                          walk(ctx).map(s2 => (subs, s2))
                        case _ => None
                    case _ => None
                else solve(p, sAcc, d - 1).map((sub, s2) => (subs :+ sub, s2))
              }
            }.flatMap { (subs, s2) =>
              // side conditions must hold on resolved args
              val condsOk = rule.conditions.forall { cond =>
                val inst = resolve(freshen(cond, fx), s2)
                Checker.isGround(inst) && (inst match
                  case Cst.Node("$neq", List(a, b)) => a != b
                  case Cst.Node("$lt", List(Cst.Leaf(a), Cst.Leaf(b))) => a.toLongOption.zip(b.toLongOption).exists(_ < _)
                  case Cst.Node("$le", List(Cst.Leaf(a), Cst.Leaf(b))) => a.toLongOption.zip(b.toLongOption).exists(_ <= _)
                  case Cst.Node(op, args) if cfg.extensions.contains(op) =>
                    cfg.extensions(op)(args).getOrElse(false)
                  case _ => false)
              }
              if condsOk then Some((Derivation(rule.name, resolve(conclusion, s2), subs), s2)) else None
            }
          }
        }.headOption
    solve(goal, Map.empty, depth) match
      case Some((d0, s)) =>
        // final resolution pass so all conclusions are ground for the checker
        def ground(d: Derivation): Derivation =
          Derivation(d.rule, resolve(d.conclusion, s), d.premises.map(ground))
        val result = ground(d0)
        if Checker.isGround(result.conclusion) then Right(result)
        else Left(s"search succeeded but conclusion is not ground: ${result.conclusion.render}")
      case None => Left(s"no derivation found for ${goal.render} within depth $depth")

  /** Infer then certify — the shared goal API for pack judgments.
    * Domain code should call this instead of duplicating infer+check.
    */
  def prove(cfg: CheckerCfg, goal: Cst, depth: Int = 64): Either[String, Derivation] =
    infer(cfg, goal, depth).flatMap { d =>
      Checker.check(cfg, d).left.map(_.render).map(_ => d)
    }
