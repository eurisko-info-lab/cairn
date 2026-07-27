package cairn.core

import cairn.kernel.*

/** The intentionally small native machine. Cases describe generic execution
  * mechanisms, never object-language features. */
enum MachineComponent(val id: String):
  case CanonicalDecoder extends MachineComponent("canonical-decoder")
  case GrammarInterpreter extends MachineComponent("grammar-interpreter")
  case RuleSearchInterpreter extends MachineComponent("rule-search-interpreter")
  case ChangeProgramInterpreter extends MachineComponent("change-program-interpreter")
  case ProofChecker extends MachineComponent("proof-checker")
  case EffectDispatcher extends MachineComponent("effect-dispatcher")

object MachineComponent:
  def parse(id: String): Either[String, MachineComponent] =
    values.find(_.id == id).toRight(s"unknown generic-machine component '$id'")

final case class MachineInterpreter(
    component: MachineComponent,
    interface: Digest,
    implementation: Digest,
):
  def canon: Canon = Canon.cmap(
    "component" -> Canon.CStr(component.id),
    "interface" -> Canon.CStr(interface.hex),
    "implementation" -> Canon.CStr(implementation.hex))
  def identity: InterpreterIdentity =
    InterpreterIdentity(component.id, interface, implementation)

object MachineInterpreter:
  def fromCanon(c: Canon): Either[String, MachineInterpreter] = for
    component <- MachineComponent.parse(c.field("component").asStr)
  yield MachineInterpreter(component, Digest(c.field("interface").asStr),
    Digest(c.field("implementation").asStr))

/** Application-selected TCB closure. `semanticPrograms` binds data-defined
  * query/policy/change languages; `effectRoutes` binds family ids to loaded
  * interface artifacts. Neither map contains host callbacks. */
final case class GenericMachine(
    interpreters: List[MachineInterpreter],
    bootstrapRoots: List[Digest],
    semanticPrograms: Map[String, Digest],
    effectRoutes: Map[String, Digest],
):
  def validate: Either[String, Unit] =
    val components = interpreters.map(_.component)
    if components.toSet != MachineComponent.values.toSet || components.distinct.size != components.size then
      Left("generic machine must select exactly one interpreter for each of the six components")
    else if interpreters.exists(i => i.interface == i.implementation) then
      Left("generic machine interface and implementation identities must be distinct")
    else if bootstrapRoots.isEmpty then Left("generic machine has no artifact bootstrap roots")
    else Right(())

  def semanticProgram(name: String): Either[String, Digest] =
    semanticPrograms.get(name).toRight(s"generic machine has no semantic program '$name'")

  def effectRoute(family: String): Either[String, Digest] =
    effectRoutes.get(family).toRight(s"generic machine has no effect route '$family'")

  def dependencies: List[Digest] =
    (bootstrapRoots ++ semanticPrograms.values ++ effectRoutes.values).distinct.sortBy(_.hex)

  def canon: Canon = Canon.cmap(
    "interpreters" -> Canon.CList(interpreters.sortBy(_.component.id).map(_.canon)),
    "bootstrapRoots" -> Canon.cstrs(bootstrapRoots.distinct.sortBy(_.hex).map(_.hex)),
    "semanticPrograms" -> Canon.cmap(semanticPrograms.toList.sortBy(_._1).map((k, v) => k -> Canon.CStr(v.hex))*),
    "effectRoutes" -> Canon.cmap(effectRoutes.toList.sortBy(_._1).map((k, v) => k -> Canon.CStr(v.hex))*))
  def artifact: Artifact = Artifact(ArtifactKind.GenericMachine, canon)
  def digest: Digest = artifact.digest

object GenericMachine:
  /** Fixture/pack-author helper. Production startup only decodes the artifact. */
  def declare(
      bootstrapRoots: List[Digest],
      semanticPrograms: Map[String, Digest] = Map.empty,
      effectRoutes: Map[String, Digest] = Map.empty,
      version: String = "generic-machine-v1",
  ): GenericMachine =
    def digest(component: MachineComponent, side: String): Digest =
      Digest.of(Canon.cmap("machine" -> Canon.CStr(version),
        "component" -> Canon.CStr(component.id), "side" -> Canon.CStr(side)))
    GenericMachine(MachineComponent.values.toList.sortBy(_.id).map(c =>
      MachineInterpreter(c, digest(c, "interface"), digest(c, "implementation"))),
      bootstrapRoots, semanticPrograms, effectRoutes)

  def fromArtifact(a: Artifact): Either[String, GenericMachine] =
    if a.kind != ArtifactKind.GenericMachine then Left("expected generic-machine artifact")
    else
      val decoded = for
        interpreters <- a.body.field("interpreters").asList.foldLeft[Either[String, List[MachineInterpreter]]](Right(Nil)) {
          (acc, raw) => for xs <- acc; x <- MachineInterpreter.fromCanon(raw) yield xs :+ x }
      yield GenericMachine(interpreters,
        a.body.field("bootstrapRoots").asList.map(x => Digest(x.asStr)),
        a.body.field("semanticPrograms").asMap.map((k, v) => k -> Digest(v.asStr)),
        a.body.field("effectRoutes").asMap.map((k, v) => k -> Digest(v.asStr)))
      decoded.flatMap(m => m.validate.map(_ => m))
