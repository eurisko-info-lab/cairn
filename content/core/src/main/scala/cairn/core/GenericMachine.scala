package cairn.core

import cairn.kernel.*

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

final case class MachineResourceBounds(maxInputBytes: Long, maxSteps: Long, maxMemoryBytes: Long):
  def validate: Either[String, Unit] = Either.cond(
    maxInputBytes > 0 && maxSteps > 0 && maxMemoryBytes > 0, (), "machine resource bounds must be positive")
  def canon: Canon = Canon.cmap("maxInputBytes" -> Canon.CInt(maxInputBytes),
    "maxSteps" -> Canon.CInt(maxSteps), "maxMemoryBytes" -> Canon.CInt(maxMemoryBytes))

final case class MachineCompatibility(interfaceVersion: String, compatibleImplementations: Set[Digest]):
  def accepts(version: Digest): Boolean = interfaceVersion.nonEmpty && compatibleImplementations(version)
  def canon: Canon = Canon.cmap("interfaceVersion" -> Canon.CStr(interfaceVersion),
    "compatibleImplementations" -> Canon.cstrs(compatibleImplementations.toList.sortBy(_.hex).map(_.hex)))

/** Result corpus is deliberately simple enough for the canonical decoder to
  * check without executing the candidate implementation. Each case binds its
  * input and both observed outcome digests. */
final case class ConformanceResults(cases: List[(Digest, Digest, Digest)]):
  def valid: Boolean = cases.nonEmpty && cases.forall((_, reference, candidate) => reference == candidate)
  def canon: Canon = Canon.CList(cases.sortBy(_._1.hex).map { (input, reference, candidate) => Canon.cmap(
    "input" -> Canon.CStr(input.hex), "reference" -> Canon.CStr(reference.hex),
    "candidate" -> Canon.CStr(candidate.hex)) })
  def artifact: Artifact = Artifact(ArtifactKind.Trace, Canon.CTag("interpreter-conformance-results", canon))

object ConformanceResults:
  def fromArtifact(a: Artifact): Either[String, ConformanceResults] = a.body match
    case Canon.CTag("interpreter-conformance-results", body) =>
      try Right(ConformanceResults(body.asList.map(x => (Digest(x.field("input").asStr),
        Digest(x.field("reference").asStr), Digest(x.field("candidate").asStr)))))
      catch case e: Exception => Left(s"invalid conformance results: ${e.getMessage}")
    case _ => Left("artifact is not interpreter conformance results")

final case class InterpreterConformance(
    implementation: Digest, interface: Digest, corpus: Digest, results: Digest, checker: Digest,
):
  def canon: Canon = Canon.cmap("implementation" -> Canon.CStr(implementation.hex),
    "interface" -> Canon.CStr(interface.hex), "corpus" -> Canon.CStr(corpus.hex),
    "results" -> Canon.CStr(results.hex), "checker" -> Canon.CStr(checker.hex))
  def artifact: Artifact = Artifact(ArtifactKind.InterpreterConformance, canon)
  def dependencies: List[Digest] = List(implementation, interface, corpus, results, checker)

object InterpreterConformance:
  def fromArtifact(a: Artifact): Either[String, InterpreterConformance] =
    if a.kind != ArtifactKind.InterpreterConformance then Left("not interpreter-conformance evidence")
    else try Right(InterpreterConformance(Digest(a.body.field("implementation").asStr),
      Digest(a.body.field("interface").asStr), Digest(a.body.field("corpus").asStr),
      Digest(a.body.field("results").asStr), Digest(a.body.field("checker").asStr)))
    catch case e: Exception => Left(s"invalid interpreter conformance: ${e.getMessage}")

final case class InterpreterImplementation(
    component: MachineComponent,
    interface: Digest,
    executable: Digest,
    conformanceSuite: Digest,
    referenceSemantics: Digest,
    implementationVersion: Digest,
    conformance: Digest,
    bounds: MachineResourceBounds,
    compatibility: MachineCompatibility,
):
  def validate: Either[String, Unit] = for
    _ <- bounds.validate
    _ <- Either.cond(compatibility.accepts(implementationVersion), (),
      s"${component.id}: implementation version is incompatible with machine interface")
  yield ()
  def canon: Canon = Canon.cmap("component" -> Canon.CStr(component.id),
    "interface" -> Canon.CStr(interface.hex), "executable" -> Canon.CStr(executable.hex),
    "conformanceSuite" -> Canon.CStr(conformanceSuite.hex),
    "referenceSemantics" -> Canon.CStr(referenceSemantics.hex),
    "implementationVersion" -> Canon.CStr(implementationVersion.hex),
    "conformance" -> Canon.CStr(conformance.hex), "bounds" -> bounds.canon,
    "compatibility" -> compatibility.canon)
  def artifact: Artifact = Artifact(ArtifactKind.InterpreterImplementation, canon)
  def dependencies: List[Digest] = List(interface, executable, conformanceSuite, referenceSemantics,
    implementationVersion, conformance)
  def identity: InterpreterIdentity = InterpreterIdentity(component.id, interface, artifact.digest)

object InterpreterImplementation:
  def fromArtifact(a: Artifact): Either[String, InterpreterImplementation] =
    if a.kind != ArtifactKind.InterpreterImplementation then Left("not an interpreter-implementation artifact")
    else try
      for
        component <- MachineComponent.parse(a.body.field("component").asStr)
        implementation = InterpreterImplementation(component, Digest(a.body.field("interface").asStr),
          Digest(a.body.field("executable").asStr), Digest(a.body.field("conformanceSuite").asStr),
          Digest(a.body.field("referenceSemantics").asStr), Digest(a.body.field("implementationVersion").asStr),
          Digest(a.body.field("conformance").asStr),
          MachineResourceBounds(a.body.field("bounds").field("maxInputBytes").asInt,
            a.body.field("bounds").field("maxSteps").asInt, a.body.field("bounds").field("maxMemoryBytes").asInt),
          MachineCompatibility(a.body.field("compatibility").field("interfaceVersion").asStr,
            a.body.field("compatibility").field("compatibleImplementations").asList.map(x => Digest(x.asStr)).toSet))
        _ <- implementation.validate
      yield implementation
    catch case e: Exception => Left(s"invalid interpreter implementation: ${e.getMessage}")

final case class GenericMachine(
    implementations: List[Digest], bootstrapRoots: List[Digest],
    semanticPrograms: Map[String, Digest], effectRoutes: Map[String, Digest],
):
  def validate: Either[String, Unit] =
    if implementations.distinct.size != MachineComponent.values.length then
      Left("generic machine must select exactly six distinct implementation artifacts")
    else if bootstrapRoots.isEmpty then Left("generic machine has no artifact bootstrap roots")
    else Right(())
  def semanticProgram(name: String): Either[String, Digest] =
    semanticPrograms.get(name).toRight(s"generic machine has no semantic program '$name'")
  def effectRoute(family: String): Either[String, Digest] =
    effectRoutes.get(family).toRight(s"generic machine has no effect route '$family'")
  def dependencies: List[Digest] =
    (implementations ++ bootstrapRoots ++ semanticPrograms.values ++ effectRoutes.values).distinct.sortBy(_.hex)
  def canon: Canon = Canon.cmap(
    "implementations" -> Canon.cstrs(implementations.distinct.sortBy(_.hex).map(_.hex)),
    "bootstrapRoots" -> Canon.cstrs(bootstrapRoots.distinct.sortBy(_.hex).map(_.hex)),
    "semanticPrograms" -> Canon.cmap(semanticPrograms.toList.sortBy(_._1).map((k, v) => k -> Canon.CStr(v.hex))*),
    "effectRoutes" -> Canon.cmap(effectRoutes.toList.sortBy(_._1).map((k, v) => k -> Canon.CStr(v.hex))*))
  def artifact: Artifact = Artifact(ArtifactKind.GenericMachine, canon)
  def digest: Digest = artifact.digest

final case class DeclaredGenericMachine(machine: GenericMachine, supportArtifacts: List[Artifact]):
  def installInto(put: Artifact => Unit): Unit = (supportArtifacts :+ machine.artifact).foreach(put)

object GenericMachine:
  /** Pack-author fixture helper. Production startup only decodes and verifies
    * the returned artifacts; it never calls this constructor. */
  def declare(
      bootstrapRoots: List[Digest], semanticPrograms: Map[String, Digest] = Map.empty,
      effectRoutes: Map[String, Digest] = Map.empty, version: String = "generic-machine-v2",
  ): DeclaredGenericMachine =
    val bounds = MachineResourceBounds(64L * 1024 * 1024, 10_000_000, 512L * 1024 * 1024)
    val packages = MachineComponent.values.toList.map { component =>
      def versionedArtifact(kind: ArtifactKind, role: String) = Artifact(kind, Canon.cmap(
        "machine" -> Canon.CStr(version), "component" -> Canon.CStr(component.id), "role" -> Canon.CStr(role)))
      def stableArtifact(kind: ArtifactKind, role: String) = Artifact(kind, Canon.cmap(
        "machineInterface" -> Canon.CStr("generic-machine-interface-v1"),
        "component" -> Canon.CStr(component.id), "role" -> Canon.CStr(role)))
      val interface = stableArtifact(ArtifactKind.Capability, "interface")
      val reference = stableArtifact(ArtifactKind.Ir, "canonical-reference-semantics")
      val executable = versionedArtifact(ArtifactKind.VmImage, "executable")
      val corpus = stableArtifact(ArtifactKind.TestSuite, "conformance-corpus")
      val checker = stableArtifact(ArtifactKind.ProofTerm, "conformance-checker")
      val versionArtifact = versionedArtifact(ArtifactKind.Source, "implementation-version")
      val observed = Digest.of(Canon.cmap("component" -> Canon.CStr(component.id), "result" -> Canon.CStr("ok")))
      val results = ConformanceResults(List((corpus.digest, observed, observed)))
      // Conformance binds the executable rather than its enclosing descriptor,
      // avoiding a digest cycle while still pinning the bytes that execute.
      val evidence = InterpreterConformance(executable.digest, interface.digest, corpus.digest,
        results.artifact.digest, checker.digest)
      val selected = InterpreterImplementation(component, interface.digest, executable.digest, corpus.digest,
        reference.digest, versionArtifact.digest, evidence.artifact.digest, bounds,
        MachineCompatibility("generic-machine-interface-v1", Set(versionArtifact.digest)))
      selected -> List(interface, reference, executable, corpus, checker, versionArtifact, results.artifact, evidence.artifact)
    }
    val machine = GenericMachine(packages.map(_._1.artifact.digest).sortBy(_.hex), bootstrapRoots, semanticPrograms, effectRoutes)
    DeclaredGenericMachine(machine, packages.flatMap { (implementation, artifacts) => artifacts :+ implementation.artifact })

  def fromArtifact(a: Artifact): Either[String, GenericMachine] =
    if a.kind != ArtifactKind.GenericMachine then Left("expected generic-machine artifact")
    else try
      val machine = GenericMachine(a.body.field("implementations").asList.map(x => Digest(x.asStr)),
        a.body.field("bootstrapRoots").asList.map(x => Digest(x.asStr)),
        a.body.field("semanticPrograms").asMap.map((k, v) => k -> Digest(v.asStr)),
        a.body.field("effectRoutes").asMap.map((k, v) => k -> Digest(v.asStr)))
      machine.validate.map(_ => machine)
    catch case e: Exception => Left(s"invalid generic machine: ${e.getMessage}")
