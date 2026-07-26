package cairn.core

import cairn.kernel.*

/** Complete content-addressed runtime selection for one language. */
final case class LanguageCapabilities(
    language: Digest,
    changeSemantics: Digest,
    changeSurface: Digest,
    validation: Option[Digest],
    migrations: List[Digest],
    queries: List[Digest],
    policies: List[Digest],
    projections: List[Digest],
):
  def canon: Canon = Canon.cmap(
    "language" -> Canon.CStr(language.hex),
    "changeSemantics" -> Canon.CStr(changeSemantics.hex),
    "changeSurface" -> Canon.CStr(changeSurface.hex),
    "validation" -> validation.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(d =>
      Canon.CTag("some", Canon.CStr(d.hex))),
    "migrations" -> Canon.CList(migrations.map(d => Canon.CStr(d.hex))),
    "queries" -> Canon.CList(queries.map(d => Canon.CStr(d.hex))),
    "policies" -> Canon.CList(policies.map(d => Canon.CStr(d.hex))),
    "projections" -> Canon.CList(projections.map(d => Canon.CStr(d.hex))))

  def artifact: Artifact = Artifact(ArtifactKind.LanguageCapabilities, canon)
  def digest: Digest = artifact.digest

object LanguageCapabilities:
  def fromCanon(c: Canon): LanguageCapabilities =
    def digests(field: String): List[Digest] = c.field(field).asList.map(x => Digest(x.asStr))
    LanguageCapabilities(
      Digest(c.field("language").asStr),
      Digest(c.field("changeSemantics").asStr),
      Digest(c.field("changeSurface").asStr),
      c.field("validation") match
        case Canon.CTag("some", Canon.CStr(d)) => Some(Digest(d))
        case _                                  => None,
      digests("migrations"), digests("queries"), digests("policies"), digests("projections"))

  def standard(language: ComposedLanguage): ResolvedLanguageCapabilities =
    val change = ChangeCapability.standard
    ResolvedLanguageCapabilities(
      LanguageCapabilities(language.digest, change.semantics.digest, change.surface.digest,
        None, Nil, Nil, Nil, Nil),
      language, change, None, Nil, Nil, Nil, Nil)

  /** Resolve an artifact-loaded descriptor against an already resolved
    * language and a caller-supplied CAS slice. Core performs no storage I/O.
    */
  def fromArtifacts(
      descriptorArtifact: Artifact,
      language: ComposedLanguage,
      semanticsArtifact: Artifact,
      surfaceArtifact: Artifact,
      extensions: List[Artifact] = Nil,
  ): Either[String, ResolvedLanguageCapabilities] =
    if descriptorArtifact.kind != ArtifactKind.LanguageCapabilities then
      Left("expected language-capabilities artifact")
    else
      scala.util.Try(fromCanon(descriptorArtifact.body)).toEither
        .left.map(e => s"invalid language capabilities artifact: ${e.getMessage}")
        .flatMap { descriptor =>
          val byDigest = extensions.map(a => a.digest -> a).toMap
          val validation = descriptor.validation.flatMap(byDigest.get).map(a => ValidationModel.fromCanon(a.body))
          for
            change <- ChangeCapability.fromArtifacts(
              semanticsArtifact, surfaceArtifact,
              Artifact(ArtifactKind.ChangeCapability, Canon.cmap(
                "semantics" -> Canon.CStr(descriptor.changeSemantics.hex),
                "surface" -> Canon.CStr(descriptor.changeSurface.hex))))
            resolved <- ResolvedLanguageCapabilities.check(
              descriptor, language, change, validation,
              descriptor.migrations.flatMap(byDigest.get),
              descriptor.queries.flatMap(byDigest.get),
              descriptor.policies.flatMap(byDigest.get),
              descriptor.projections.flatMap(byDigest.get))
          yield resolved
        }

/** Digest-checked runtime view. */
final case class ResolvedLanguageCapabilities(
    descriptor: LanguageCapabilities,
    language: ComposedLanguage,
    change: ChangeCapability,
    validation: Option[ValidationModel],
    migrations: List[Artifact],
    queries: List[Artifact],
    policies: List[Artifact],
    projections: List[Artifact],
):
  def changeModel: ChangeModel = change.model.fold(e => throw IllegalStateException(e), identity)

  def moduleGate(
      resolveProvider: Digest => Option[ComposedLanguage] = _ => None,
  ): ModuleGate = validation.fold(ModuleGate.passthrough)(m =>
    ModuleGate.fromValidationModel("language-validation", m, resolveProvider))

  def migration(digest: Digest): Either[String, LangMigration] =
    migrations.find(_.digest == digest).toRight(s"migration ${digest.short} is not in the language bundle")
      .flatMap(LangMigration.fromArtifact)

  /** A generic closure step. Repetition derives every finite Δ level. */
  def delta: Either[List[ComposeError], ResolvedLanguageCapabilities] =
    Delta.deltaOf(language, change).map { derived =>
      val next = LanguageCapabilities(
        derived.digest, change.semantics.digest, change.surface.digest,
        None, Nil, Nil, Nil, Nil)
      ResolvedLanguageCapabilities(next, derived, change, None, Nil, Nil, Nil, Nil)
    }

object ResolvedLanguageCapabilities:
  def check(
      descriptor: LanguageCapabilities,
      language: ComposedLanguage,
      change: ChangeCapability,
      validation: Option[ValidationModel],
      migrations: List[Artifact],
      queries: List[Artifact],
      policies: List[Artifact],
      projections: List[Artifact],
  ): Either[String, ResolvedLanguageCapabilities] =
    def exact(label: String, expected: List[Digest], actual: List[Artifact]): Either[String, Unit] =
      val got = actual.map(_.digest)
      Either.cond(got == expected, (),
        s"$label digest mismatch: expected ${expected.map(_.short)}, got ${got.map(_.short)}")
    for
      _ <- Either.cond(language.digest == descriptor.language, (), "language capability language digest mismatch")
      _ <- Either.cond(change.semantics.digest == descriptor.changeSemantics, (), "language capability change-semantics digest mismatch")
      _ <- Either.cond(change.surface.digest == descriptor.changeSurface, (), "language capability change-surface digest mismatch")
      _ <- Either.cond(validation.map(_.digest) == descriptor.validation, (), "language capability validation digest mismatch")
      _ <- validation.fold[Either[String, Unit]](Right(()))(v =>
        Either.cond(v.targetLanguage == language.digest, (), "validation model targets a different language"))
      _ <- exact("migration", descriptor.migrations, migrations)
      _ <- exact("query", descriptor.queries, queries)
      _ <- exact("policy", descriptor.policies, policies)
      _ <- exact("projection", descriptor.projections, projections)
      _ <- change.model.map(_ => ())
    yield ResolvedLanguageCapabilities(descriptor, language, change, validation,
      migrations, queries, policies, projections)
