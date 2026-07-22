package cairn.examples.sds

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.PackLoader
import cairn.systemhandler.EffectContext
import java.nio.file.Path

/** SDS causal workflow as an ordinary Cairn language + checked instance module.
  *
  * Language: [[languages/sds-workflow.cairn]] (`workflowStepOk` /
  * `workflowPhaseOk`). Instance: [[languages/sds-workflow/causal.cairn]] —
  * author → shadow → rebase → conflict → approve → sign → publish.
  *
  * Host [[SdsCausalWorkflow]] runs the *effectful* steps (CAS / Branches /
  * Ed25519 / ledger) under authority; the *scripted sequence* and step/phase
  * legality are disk SoT, loadable via [[PackLoader]] without recompiling Scala.
  */
object SdsWorkflow:
  private lazy val packs = PackLoader(EffectContext.forPackLoader())
  lazy val language: ComposedLanguage = packs.requireClosed("sds-workflow")

  final case class Step(name: String, phase: String)
  final case class Decl(id: String, steps: List[Step])

  lazy val causalModule: Module =
    ChemicalSource.loadModule(language, Path.of("languages/sds-workflow/causal.cairn"))
      .fold(e => throw RuntimeException(e), identity)

  def checkStep(name: String): Boolean =
    val cfg = CheckerCfg(language.judgments.values.toList)
    val goal = Cst.node("workflowStepOk", Cst.Leaf(name))
    Search.infer(cfg, goal).flatMap(d => Checker.check(cfg, d).left.map(_.render)).isRight

  def checkPhase(phase: String): Boolean =
    val cfg = CheckerCfg(language.judgments.values.toList)
    val goal = Cst.node("workflowPhaseOk", Cst.Leaf(phase))
    Search.infer(cfg, goal).flatMap(d => Checker.check(cfg, d).left.map(_.render)).isRight

  /** Decode + judgment-check a workflow module (ordered steps). */
  def decode(m: Module, workflowName: String = "sdsCausal"): Either[String, Decl] =
    val stepDefs = m.defs.collect {
      case (n, Cst.Node("step", List(Cst.Leaf(name), Cst.Leaf(phase)))) =>
        n -> Step(name, phase)
    }.toMap
    m.get(workflowName) match
      case Some(Cst.Node("workflow", List(Cst.Leaf(id), Cst.Node("list", refs)))) =>
        val errs = List.newBuilder[String]
        val steps = List.newBuilder[Step]
        for r <- refs do r match
          case Cst.Leaf(ref) =>
            stepDefs.get(ref) match
              case None => errs += s"workflow '$workflowName' references unknown step '$ref'"
              case Some(s) =>
                if !checkStep(s.name) then errs += s"step '${s.name}' fails workflowStepOk"
                if !checkPhase(s.phase) then errs += s"phase '${s.phase}' fails workflowPhaseOk"
                steps += s
          case other => errs += s"bad step ref ${other.render}"
        val es = errs.result()
        if es.nonEmpty then Left(es.mkString("; "))
        else Right(Decl(id, steps.result()))
      case Some(other) => Left(s"'$workflowName' is not a workflow: ${other.render}")
      case None => Left(s"no workflow '$workflowName'")

  lazy val causal: Decl =
    decode(causalModule).fold(e => throw RuntimeException(e), identity)

  /** Expected causal step names (disk SoT). */
  def causalStepNames: List[String] = causal.steps.map(_.name)
