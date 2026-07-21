package cairn.core

import cairn.kernel.*

/** M22: thin tactic/goal engine. Scripts are artifacts; replay produces a
  * proof term that the INDEPENDENT checker still validates — tactics are
  * proposers, never certifiers. MIGRATION-PLAN.md Phase 2 (first slice):
  * Core, not Kernel — calls into `kernel.Checker`/`core.Search`, never the
  * reverse.
  */
enum Tactic:
  case ApplyRule(name: String)
  case Auto(depth: Int)

final case class TacticScript(name: String, tactics: List[Tactic]):
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name),
    "tactics" -> Canon.CList(tactics.map {
      case Tactic.ApplyRule(r) => Canon.CTag("apply", Canon.CStr(r))
      case Tactic.Auto(d)      => Canon.CTag("auto", Canon.CInt(d)) }))
  def artifact: Artifact = Artifact(ArtifactKind.TacticScript, canon)

object Tactics:
  def replay(cfg: CheckerCfg, script: TacticScript, goal: Cst): Either[String, Derivation] =
    val rules = cfg.judgments.flatMap(_.rules).map(r => r.name -> r).toMap
    def step(goal: Cst, tactics: List[Tactic]): Either[String, (Derivation, List[Tactic])] =
      tactics match
        case Nil => Left(s"no tactics left for goal ${goal.render}")
        case Tactic.Auto(depth) :: rest =>
          Search.infer(cfg, goal, depth).map(d => (d, rest))
        case Tactic.ApplyRule(name) :: rest =>
          rules.get(name).toRight(s"unknown rule '$name'").flatMap { rule =>
            Checker.matchPat(rule.conclusion, goal, Map.empty)
              .toRight(s"rule '$name' does not apply to ${goal.render}") match
              case Left(e) => Left(e)
              case Right(env) =>
                val subgoalPats = rule.premises.filterNot(Checker.isComputational)
                val zero: Either[String, (List[Derivation], List[Tactic])] = Right((Nil, rest))
                subgoalPats.foldLeft(zero) { (acc, pat) =>
                  acc.flatMap { (subs, ts) =>
                    val sub = Checker.instantiate(pat, env)
                    if !Checker.isGround(sub) then
                      // fall back to search for goals the match didn't ground
                      Search.infer(cfg, sub).map(d => (subs :+ d, ts))
                    else step(sub, ts).map((d, ts2) => (subs :+ d, ts2))
                  }
                }.map((subs, ts) => (Derivation(name, goal, subs), ts))
          }
    step(goal, script.tactics).flatMap { (d, leftover) =>
      if leftover.isEmpty then Right(d) else Left(s"${leftover.length} unused tactics") }
