package cairn.core

import cairn.kernel.*

/** Pack form: provider aliases stay unresolved until all language bundles
  * are available. `model` is the canonical rule body without host code. */
final case class MigrationDeclaration(fromProvider: String, toProvider: String, model: Canon):
  def canon: Canon = Canon.cmap(
    "fromProvider" -> Canon.CStr(fromProvider),
    "toProvider" -> Canon.CStr(toProvider),
    "model" -> model)

object MigrationDeclaration:
  def fromCanon(c: Canon): MigrationDeclaration = MigrationDeclaration(
    c.field("fromProvider").asStr, c.field("toProvider").asStr, c.field("model"))

/** Provider-resolved, capability-aware migration. */
final case class ResolvedMigration(
    model: LangMigration,
    source: ResolvedLanguageCapabilities,
    target: ResolvedLanguageCapabilities,
):
  def artifact: Artifact = model.artifact

  def module(value: Module): Either[String, Module] =
    Migrate.module(model, source.language, target.language, value)

  def change(value: Cst): Either[String, Cst] =
    Migrate.changeset(model, source.language, target.language, value)

  def semanticPath(migratedRoot: Cst, value: SemanticPath): Either[String, Migrate.PathTransport] =
    Migrate.path(model, target.language, migratedRoot, value)

  /** Judgment-provider references are migrated by exact digest, never name. */
  def validation(value: ValidationModel): Either[String, ValidationModel] =
    if value.targetLanguage != source.language.digest then
      Left("migration validation model does not target the source language")
    else
      def rewrite(c: Canon): Canon = c match
        case Canon.CStr(s) if s == source.language.digest.hex => Canon.CStr(target.language.digest.hex)
        case Canon.CStr(s) if Digest.parse(s).toOption.exists(model.providerMigrations.contains) =>
          Canon.CStr(model.providerMigrations(Digest(s)).hex)
        case Canon.CList(xs) => Canon.CList(xs.map(rewrite))
        case Canon.CMap(es)   => Canon.cmap(es.map((k, v) => k -> rewrite(v))*)
        case Canon.CTag(t, v) => Canon.CTag(t, rewrite(v))
        case other => other
      val migrated = ValidationModel.fromCanon(rewrite(value.canon))
      Either.cond(migrated.targetLanguage == target.language.digest, migrated,
        "transported validation model does not target the declared language revision")

  /** Change semantics are language-parametric, but the target bundle must
    * explicitly select their exact identities. */
  def changeCapability(value: ChangeCapability): Either[String, ChangeCapability] =
    Either.cond(
      value.semantics.digest == source.descriptor.changeSemantics &&
        value.surface.digest == source.descriptor.changeSurface,
      (), "change capability is not selected by the source language bundle")
      .flatMap(_ => target.change.model.map(_ => target.change))

  def languageCapabilities(value: LanguageCapabilities): Either[String, LanguageCapabilities] =
    if value.language != source.language.digest then Left("capability bundle does not target the migration source")
    else ResolvedMigration.validateTargets(target).map(_ => target.descriptor)

  def pending(value: PendingEdit): Either[String, PendingEdit] =
    if value.language != source.language.digest then Left("pending edit does not target the migration source")
    else
      for
        base <- module(value.base)
        change <- change(value.change)
      yield PendingEdit(target.language.digest, base, change)

  def conflict(value: StoredConflict): Either[String, StoredConflict] =
    if value.language != source.language.digest then Left("stored conflict does not target the migration source")
    else
      for
        base <- module(value.base)
        a <- change(value.changeA)
        b <- change(value.changeB)
        overlap <- value.conflict.overlap.toList.foldLeft[Either[String, Set[SemanticLocation]]](Right(Set.empty)) {
        (acc, location) => acc.flatMap { xs => location match
          case fixed @ (SemanticLocation.Binding(_) | SemanticLocation.WholeDefinition(_)) => Right(xs + fixed)
          case SemanticLocation.Subtree(name, path) =>
            base.get(name).toRight(s"stored conflict path names missing definition '$name'").flatMap { root =>
              semanticPath(root, path).flatMap {
                case Migrate.PathTransport.Transported(p) => Right(xs + SemanticLocation.Subtree(name, p))
                case Migrate.PathTransport.Deleted(field) =>
                  Left(s"stored conflict path field '$field' was deleted")
                case Migrate.PathTransport.Ambiguous(field, positions) =>
                  Left(s"stored conflict path field '$field' became ambiguous at ${positions.mkString(",")}")
              }
            }
        }
        }
        digA = Artifact(ArtifactKind.ChangeSet, Cst.toCanon(a)).digest
        digB = Artifact(ArtifactKind.ChangeSet, Cst.toCanon(b)).digest
        conflict = value.conflict.copy(overlap = overlap, changeA = digA, changeB = digB)
      yield StoredConflict(target.language.digest, base, a, b, conflict)

object ResolvedMigration:
  /** Proves that every transported model exposed by the target bundle names
    * the declared target revision before migration is admitted. */
  def validateTargets(target: ResolvedLanguageCapabilities): Either[String, Unit] =
    for
      _ <- Either.cond(target.descriptor.language == target.language.digest, (), "target bundle language mismatch")
      _ <- target.validation.fold[Either[String, Unit]](Right(()))(v =>
        Either.cond(v.targetLanguage == target.language.digest, (), "target validation model targets another revision"))
      _ <- target.change.model.map(_ => ())
    yield ()

final case class PendingEdit(language: Digest, base: Module, change: Cst):
  def canon: Canon = Canon.cmap(
    "language" -> Canon.CStr(language.hex), "base" -> base.canon, "change" -> Cst.toCanon(change))
  def artifact: Artifact = Artifact(ArtifactKind.ChangeSet, Canon.CTag("pending-edit", canon))

final case class StoredConflict(
    language: Digest, base: Module, changeA: Cst, changeB: Cst, conflict: Merge.Conflict,
):
  def canon: Canon = Canon.cmap(
    "language" -> Canon.CStr(language.hex), "base" -> base.canon,
    "changeA" -> Cst.toCanon(changeA), "changeB" -> Cst.toCanon(changeB),
    "conflict" -> conflict.canon)
  def artifact: Artifact = Artifact(ArtifactKind.ChangeSet, Canon.CTag("stored-conflict", canon))

object StoredConflict:
  def fromArtifact(artifact: Artifact): Either[String, StoredConflict] = artifact.body match
    case Canon.CTag("stored-conflict", body) =>
      val decoded = scala.util.Try((Digest(body.field("language").asStr), Module.fromCanon(body.field("base")),
        Cst.fromCanon(body.field("changeA")), Cst.fromCanon(body.field("changeB")))).toEither
        .left.map(e => s"invalid stored conflict: ${e.getMessage}")
      for
        values <- decoded
        (language, base, a, b) = values
        conflict <- Merge.Conflict.fromArtifact(Artifact(ArtifactKind.ChangeSet,
          Canon.CTag("merge-conflict", body.field("conflict"))))
      yield StoredConflict(language, base, a, b, conflict)
    case _ => Left("expected stored-conflict artifact")

object MigrationModelLoader:
  def resolve(
      owner: ComposedLanguage,
      declaration: MigrationDeclaration,
      resolveProvider: String => Option[ResolvedLanguageCapabilities],
  ): Either[String, ResolvedMigration] =
    def resolve(alias: String): Either[String, ResolvedLanguageCapabilities] =
      for
        pack <- owner.providers.get(alias).toRight(s"migration references undeclared provider alias '$alias'")
        bundle <- resolveProvider(pack).toRight(s"migration provider '$alias' ('$pack') is unavailable")
      yield bundle
    for
      source <- resolve(declaration.fromProvider)
      target <- resolve(declaration.toProvider)
      template <- scala.util.Try(LangMigration.fromCanon(declaration.model)).toEither
        .left.map(e => s"invalid migration model: ${e.getMessage}")
      model = template.copy(fromLang = source.language.digest, toLang = target.language.digest)
      _ <- validate(model, source, target)
    yield ResolvedMigration(model, source, target)

  def fromLanguage(
      owner: ComposedLanguage,
      resolveProvider: String => Option[ResolvedLanguageCapabilities],
  ): Either[String, List[ResolvedMigration]] =
    owner.migrations.foldLeft[Either[String, List[ResolvedMigration]]](Right(Nil)) { (acc, c) =>
      for
        xs <- acc
        decl <- scala.util.Try(MigrationDeclaration.fromCanon(c)).toEither
          .left.map(e => s"invalid migration declaration: ${e.getMessage}")
        resolved <- resolve(owner, decl, resolveProvider)
      yield xs :+ resolved
    }

  def validate(
      model: LangMigration,
      source: ResolvedLanguageCapabilities,
      target: ResolvedLanguageCapabilities,
  ): Either[String, Unit] =
    for
      _ <- Either.cond(model.fromLang == source.language.digest, (), "migration source digest mismatch")
      _ <- Either.cond(model.toLang == target.language.digest, (), "migration target digest mismatch")
      _ <- ResolvedMigration.validateTargets(target)
      _ <- model.providerMigrations.toList.foldLeft[Either[String, Unit]](Right(())) { case (acc, (_, to)) =>
        acc.flatMap(_ => Either.cond(
          target.validation.forall(v => v.providers.isEmpty || v.providers.contains(to)), (),
          s"transported provider ${to.short} is not selected by target validation model"))
      }
    yield ()
