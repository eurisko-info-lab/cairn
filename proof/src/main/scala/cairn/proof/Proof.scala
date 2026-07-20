package cairn.proof

import cairn.kernel.*

/** L2 proof kernel (S19–S23).
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

/** M21: bounded, syntax-directed derivation SEARCH (type inference). An
  * untrusted proposer: whatever it finds must still pass [[Checker.check]].
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

/** M22: thin tactic/goal engine. Scripts are artifacts; replay produces a
  * proof term that the INDEPENDENT checker still validates — tactics are
  * proposers, never certifiers.
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

/** Claims, theorems, test suites and certificates (S19, S24, §2 Claim vs proof).
  * A Claim may exist proof-free; a Theorem carries a checked proof term.
  */
final case class Claim(name: String, statement: Cst, subject: Digest):
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name),
    "statement" -> Cst.toCanon(statement),
    "subject" -> Canon.CStr(subject.hex))
  def artifact: Artifact = Artifact(ArtifactKind.Claim, canon)

final case class Theorem(claim: Claim, proof: Digest):
  def canon: Canon = Canon.cmap("claim" -> claim.canon, "proof" -> Canon.CStr(proof.hex))
  def artifact: Artifact = Artifact(ArtifactKind.Theorem, canon)

final case class TestCase(name: String, input: Cst, expected: Cst)
final case class TestSuite(name: String, subject: Digest, cases: List[TestCase]):
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name),
    "subject" -> Canon.CStr(subject.hex),
    "cases" -> Canon.CList(cases.map(tc => Canon.cmap(
      "name" -> Canon.CStr(tc.name),
      "input" -> Cst.toCanon(tc.input),
      "expected" -> Cst.toCanon(tc.expected)))))
  def artifact: Artifact = Artifact(ArtifactKind.TestSuite, canon)

/** Kernel-issued record that a claim was validated by a given method. */
final case class Certificate(claim: Digest, method: String, evidence: Digest):
  def canon: Canon = Canon.cmap(
    "claim" -> Canon.CStr(claim.hex),
    "method" -> Canon.CStr(method), // "proof-term" | "test-suite"
    "evidence" -> Canon.CStr(evidence.hex))
  def artifact: Artifact = Artifact(ArtifactKind.Certificate, canon)

object Certify:
  /** Certified path: check the proof term, then issue a certificate. */
  def byProof(judgments: List[JudgmentDef], claim: Claim, proof: Derivation): Either[String, (Theorem, Certificate)] =
    if proof.conclusion != claim.statement then
      Left(s"proof concludes ${proof.conclusion.render}, claim states ${claim.statement.render}")
    else
      Checker.check(judgments, proof)
        .left.map(_.render)
        .map { _ =>
          val thm = Theorem(claim, proof.artifact.digest)
          (thm, Certificate(claim.artifact.digest, "proof-term", proof.artifact.digest)) }

  /** Proof-free path: run a test suite with a supplied evaluator (§6 Phase 2). */
  def byTests(claim: Claim, suite: TestSuite, eval: Cst => Either[String, Cst]): Either[String, Certificate] =
    val failures = suite.cases.flatMap { tc =>
      eval(tc.input) match
        case Right(v) if v == tc.expected => Nil
        case Right(v)  => List(s"${tc.name}: got ${v.render}, expected ${tc.expected.render}")
        case Left(err) => List(s"${tc.name}: evaluation failed: $err")
    }
    if failures.nonEmpty then Left(failures.mkString("; "))
    else Right(Certificate(claim.artifact.digest, "test-suite", suite.artifact.digest))
