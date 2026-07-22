package cairn.runtime

/** Generic interpreter for language-defined workflow step lists.
  *
  * Disk SoT packs (e.g. `sds-workflow` causal module) declare ordered steps;
  * Scala supplies effect handlers keyed by step name. The runner sequences
  * steps and fails closed on the first error — it does not invent SDS
  * vocabulary or report surfaces.
  *
  * [[Report]] is **untrusted host telemetry**: completing a workflow does not
  * mint authority, tip validity, or certificates. Privileged accept paths
  * (commitTip / attachCertificate / ledger) must not gate on a forged Report.
  */
object WorkflowRunner:

  final case class Step(name: String, phase: String)

  /** Host sequencing receipt — not a Kernel-checked artifact. */
  final case class Report(
      completed: List[String],
      results: Map[String, String] = Map.empty,
  )

  /** Handler for one step; may return a short result tag for the report. */
  type Handler = Step => Either[String, String]

  /** Run `steps` in order through `handle`. */
  def run(steps: List[Step], handle: Handler): Either[String, Report] =
    steps.foldLeft[Either[String, (List[String], Map[String, String])]](Right((Nil, Map.empty))) {
      case (Left(e), _) => Left(e)
      case (Right((done, results)), step) =>
        handle(step).map { tag =>
          (done :+ step.name, results + (step.name -> tag))
        }
    }.map { case (done, results) => Report(done, results) }

  /** Run a contiguous fragment `[fromName, untilName]` inclusive. */
  def runFragment(
      steps: List[Step],
      fromName: String,
      untilName: String,
      handle: Handler,
  ): Either[String, Report] =
    val fromIdx = steps.indexWhere(_.name == fromName)
    val untilIdx = steps.indexWhere(_.name == untilName)
    if fromIdx < 0 then Left(s"workflow fragment: unknown start step '$fromName'")
    else if untilIdx < 0 then Left(s"workflow fragment: unknown end step '$untilName'")
    else if untilIdx < fromIdx then Left(s"workflow fragment: '$untilName' before '$fromName'")
    else run(steps.slice(fromIdx, untilIdx + 1), handle)
