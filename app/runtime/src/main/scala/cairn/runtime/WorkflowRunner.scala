package cairn.runtime

/** Generic interpreter for language-defined workflow step lists.
  *
  * Disk SoT packs (e.g. `sds-workflow` causal module) declare ordered steps;
  * composition roots supply effect handlers. The runner sequences steps and
  * fails closed on the first error — it does not invent SDS vocabulary.
  *
  * [[Report]] is **untrusted host telemetry**: completing a workflow does not
  * mint authority, tip validity, or certificates. Privileged accept paths
  * (commitTip / attachCertificate / ledger) must not gate on a forged Report.
  *
  * Toward Scala-free domain apps: prefer [[HandlerRegistry]] with stable
  * handler ids (eventually ActionKey digests) over ad-hoc step-name matches.
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

  /** step name → handler id (e.g. `"cas.put"` / ActionKey digest) → implementation. */
  final case class HandlerRegistry(
      stepToHandlerId: Map[String, String],
      handlersById: Map[String, Handler],
  ):
    def resolve(step: Step): Either[String, Handler] =
      for
        id <- stepToHandlerId.get(step.name)
          .toRight(s"workflow: no handler id bound for step '${step.name}'")
        h <- handlersById.get(id)
          .toRight(s"workflow: unknown handler id '$id' for step '${step.name}'")
      yield h

  /** Run `steps` in order through `handle`. */
  def run(steps: List[Step], handle: Handler): Either[String, Report] =
    steps.foldLeft[Either[String, (List[String], Map[String, String])]](Right((Nil, Map.empty))) {
      case (Left(e), _) => Left(e)
      case (Right((done, results)), step) =>
        handle(step).map { tag =>
          (done :+ step.name, results + (step.name -> tag))
        }
    }.map { case (done, results) => Report(done, results) }

  /** Run with an explicit step-name → handler registry (fail closed on miss). */
  def run(steps: List[Step], handlers: Map[String, Handler]): Either[String, Report] =
    run(steps, step =>
      handlers.get(step.name).toRight(s"workflow: no handler registered for step '${step.name}'")
        .flatMap(_(step)))

  /** Run via [[HandlerRegistry]] — step→id→handler indirection for effect binding. */
  def run(steps: List[Step], registry: HandlerRegistry): Either[String, Report] =
    run(steps, step => registry.resolve(step).flatMap(_(step)))

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

  def runFragment(
      steps: List[Step],
      fromName: String,
      untilName: String,
      handlers: Map[String, Handler],
  ): Either[String, Report] =
    runFragment(steps, fromName, untilName, step =>
      handlers.get(step.name).toRight(s"workflow: no handler registered for step '${step.name}'")
        .flatMap(_(step)))
