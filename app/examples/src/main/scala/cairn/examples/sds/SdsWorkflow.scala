package cairn.examples.sds
import cairn.runtime.EffectContexts

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.PackLoader
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
  private lazy val packs = PackLoader(EffectContexts.forPackLoader())
  lazy val language: ComposedLanguage = packs.requireClosed("sds-workflow")

  final case class Step(name: String, phase: String)
  final case class Decl(
      id: String,
      steps: List[Step],
      /** Pack-declared step name → handler id. */
      binds: Map[String, String] = Map.empty,
  )

  lazy val causalModule: Module =
    ChemicalSource.loadModule(language, Path.of("content/languages/sds-workflow/causal.cairn"))
      .fold(e => throw RuntimeException(e), identity)

  private def cfg: CheckerCfg = CheckerCfg(language.judgments.values.toList)

  def checkStep(name: String): Boolean =
    Search.prove(cfg, Cst.node("workflowStepOk", Cst.Leaf(name))).isRight

  def checkPhase(phase: String): Boolean =
    Search.prove(cfg, Cst.node("workflowPhaseOk", Cst.Leaf(phase))).isRight

  def decodeBinds(m: Module): Map[String, String] =
    m.defs.collect {
      case (_, Cst.Node("bind", List(Cst.Leaf(step), Cst.Leaf(handlerId)))) =>
        step -> handlerId
    }.toMap

  /** Decode + judgment-check a workflow module (ordered steps + binds). */
  def decode(m: Module, workflowName: String = "sdsCausal"): Either[String, Decl] =
    val stepDefs = m.defs.collect {
      case (n, Cst.Node("step", List(Cst.Leaf(name), Cst.Leaf(phase)))) =>
        n -> Step(name, phase)
    }.toMap
    val binds = decodeBinds(m)
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
                if !binds.contains(s.name) then
                  errs += s"step '${s.name}' has no bind handler id"
                steps += s
          case other => errs += s"bad step ref ${other.render}"
        val es = errs.result()
        if es.nonEmpty then Left(es.mkString("; "))
        else Right(Decl(id, steps.result(), binds))
      case Some(other) => Left(s"'$workflowName' is not a workflow: ${other.render}")
      case None => Left(s"no workflow '$workflowName'")

  lazy val causal: Decl =
    decode(causalModule).fold(e => throw RuntimeException(e), identity)

  /** Expected causal step names (disk SoT). */
  def causalStepNames: List[String] = causal.steps.map(_.name)
