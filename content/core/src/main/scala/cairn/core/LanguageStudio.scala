package cairn.core

import cairn.kernel.*

enum LanguageAssetKind:
  case Language, Grammar, ChangeSemantics, ChangeSurface, LanguageCapabilities,
    ValidationModel, Migration, ForeignSurface, StudioProfileSemantics, StudioProfileSurface

  def artifactKind: ArtifactKind = this match
    case Language => ArtifactKind.Language
    case Grammar => ArtifactKind.Grammar
    case ChangeSemantics => ArtifactKind.ChangeModel
    case ChangeSurface => ArtifactKind.ChangeSurfaceModel
    case LanguageCapabilities => ArtifactKind.LanguageCapabilities
    case ValidationModel => ArtifactKind.ValidationModel
    case Migration => ArtifactKind.Migration
    case ForeignSurface => ArtifactKind.ForeignSurface
    case StudioProfileSemantics => ArtifactKind.StudioProfileSemantics
    case StudioProfileSurface => ArtifactKind.StudioProfileSurface

final case class LanguageAssetId(kind: LanguageAssetKind, name: String):
  require(name.nonEmpty, "language asset name is required")
  def binding: String = s"${kind.toString.toLowerCase}--$name"

final case class LanguageStudioEdit(id: LanguageAssetId, replacement: Artifact)

final case class LanguageStudioProject(targetLanguage: Digest, assets: Map[LanguageAssetId, Artifact]):
  private def encode(a: Artifact): Cst = Cst.Leaf(Canon.encode(a.canon).map(b => f"${b & 0xff}%02x").mkString)
  def metaModule: Module = Module(assets.toList.collect {
    case (id, artifact) if id.kind != LanguageAssetKind.Grammar => id.binding -> encode(artifact) })
  def grammarModule: Module = Module(assets.toList.collect {
    case (id, artifact) if id.kind == LanguageAssetKind.Grammar => id.binding -> encode(artifact) })
  def digest: Digest = Digest.of(Canon.cmap(
    "targetLanguage" -> Canon.CStr(targetLanguage.hex),
    "assets" -> Canon.CList(assets.toList.sortBy(_._1.binding).map { (id, a) => Canon.cmap(
      "binding" -> Canon.CStr(id.binding), "artifact" -> Canon.CStr(a.digest.hex)) })))

  def validate: Either[String, Unit] =
    val digests = assets.values.map(_.digest).toSet
    for
      _ <- assets.toList.foldLeft[Either[String, Unit]](Right(())) { case (acc, (id, artifact)) =>
        acc.flatMap(_ => Either.cond(artifact.kind == id.kind.artifactKind, (),
          s"${id.binding} has ${artifact.kind.name}, expected ${id.kind.artifactKind.name}")) }
      _ <- assets.collectFirst { case (LanguageAssetId(LanguageAssetKind.LanguageCapabilities, _), a) =>
        LanguageCapabilities.fromCanon(a.body) }.fold[Either[String, Unit]](Right(()))(c => for
          _ <- Either.cond(c.language == targetLanguage, (), "language capability bundle targets another language revision")
          selected = List(c.changeSemantics, c.changeSurface) ++ c.validation.toList ++ c.migrations ++ c.projections
          _ <- Either.cond(selected.forall(digests.contains), (), "language capability bundle selects an artifact outside the project")
        yield ())
      _ <- scala.util.Try(assets.collect { case (LanguageAssetId(LanguageAssetKind.ChangeSemantics, _), a) =>
        ChangeSemanticsModel.fromCanon(a.body) }).toEither.left.map(e => s"invalid change semantics: ${e.getMessage}").map(_ => ())
      _ <- scala.util.Try(assets.collect { case (LanguageAssetId(LanguageAssetKind.ChangeSurface, _), a) =>
        ChangeSurfaceModel.fromCanon(a.body) }).toEither.left.map(e => s"invalid change surface: ${e.getMessage}").map(_ => ())
      _ <- assets.collect { case (LanguageAssetId(LanguageAssetKind.Migration, _), a) => a }.toList
        .foldLeft[Either[String, Unit]](Right(())) { (acc, artifact) => for
          _ <- acc
          migration <- LangMigration.fromArtifact(artifact)
          _ <- Either.cond(migration.fromLang == targetLanguage || migration.toLang == targetLanguage, (),
            "migration is unrelated to the project language revision")
        yield () }
      _ <- assets.collectFirst { case (LanguageAssetId(LanguageAssetKind.ValidationModel, _), a) =>
        ValidationModel.fromCanon(a.body) }.fold[Either[String, Unit]](Right(()))(v =>
          Either.cond(v.targetLanguage == targetLanguage, (), "validation model targets another language revision"))
      _ <- assets.collectFirst { case (LanguageAssetId(LanguageAssetKind.StudioProfileSemantics, _), a) => a }
        .fold[Either[String, Unit]](Right(()))(StudioProfileSemantics.fromArtifact(_).flatMap(p =>
          Either.cond(p.language == targetLanguage, (), "Studio profile targets another language revision")))
      semanticDigests = assets.collect { case (LanguageAssetId(LanguageAssetKind.StudioProfileSemantics, _), a) => a.digest }.toSet
      _ <- assets.collect { case (LanguageAssetId(LanguageAssetKind.StudioProfileSurface, _), a) => a }.toList
        .foldLeft[Either[String, Unit]](Right(())) { (acc, artifact) => for
          _ <- acc
          surface <- StudioProfileSurface.fromArtifact(artifact)
          _ <- Either.cond(semanticDigests.contains(surface.semantics), (), "Studio surface has no project semantics profile")
        yield () }
      _ <- assets.collect { case (LanguageAssetId(LanguageAssetKind.ForeignSurface, _), a) => a }.toList
        .foldLeft[Either[String, Unit]](Right(())) { (acc, artifact) => acc.flatMap { _ =>
          artifact.body match
            case Canon.CTag("surface-bundle", body) => Either.cond(
              Digest(body.field("language").asStr) == targetLanguage, (), "foreign surface bundle targets another language revision")
            case body => ForeignSurfaceDescriptor.fromCanon(body).flatMap(d => Either.cond(
              d.language == targetLanguage, (), "foreign surface targets another language revision"))
        }}
    yield ()

final case class LanguageStudioProposal(
    base: Digest, result: LanguageStudioProject,
    deltaMeta: Option[Cst], metaWitness: Option[Delta.ValidatedChangeSet],
    deltaGrammar: Option[Cst], grammarWitness: Option[Delta.ValidatedChangeSet],
):
  def artifact: Artifact = Artifact(ArtifactKind.ChangeSet, Canon.CTag("language-studio-proposal", Canon.cmap(
    "base" -> Canon.CStr(base.hex), "result" -> Canon.CStr(result.digest.hex),
    "deltaMeta" -> deltaMeta.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", Cst.toCanon(x))),
    "metaWitness" -> metaWitness.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", Canon.CStr(x.artifact.digest.hex))),
    "deltaGrammar" -> deltaGrammar.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", Cst.toCanon(x))),
    "grammarWitness" -> grammarWitness.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", Canon.CStr(x.artifact.digest.hex))))))

object LanguageStudio:
  private def encoded(artifact: Artifact): Cst =
    Cst.Leaf(Canon.encode(artifact.canon).map(b => f"${b & 0xff}%02x").mkString)

  def propose(
      project: LanguageStudioProject, edits: List[LanguageStudioEdit],
      meta: ResolvedLanguageCapabilities, grammar: ResolvedLanguageCapabilities,
  ): Either[String, LanguageStudioProposal] =
    val grammarEdits = edits.filter(_.id.kind == LanguageAssetKind.Grammar)
    val metaEdits = edits.filterNot(_.id.kind == LanguageAssetKind.Grammar)
    def applyEdits(base: Module, selected: List[LanguageStudioEdit], capabilities: ResolvedLanguageCapabilities) =
      if selected.isEmpty then Right((base, Option.empty[Cst], Option.empty[Delta.ValidatedChangeSet]))
      else
        val change = ChangeAlgebra.changeset(capabilities.language, selected.map(e =>
          Studio.mutation(capabilities.language, StudioAction.Replace(e.id.binding, encoded(e.replacement)))))
        Delta.apply(capabilities.language, base, change, capabilities.changeModel)
          .map((result, witness) => (result, Some(change), Some(witness)))
    for
      _ <- project.validate
      _ <- edits.foldLeft[Either[String, Unit]](Right(())) { (acc, edit) => acc.flatMap { _ => for
        current <- project.assets.get(edit.id).toRight(s"unknown language asset '${edit.id.binding}'")
        _ <- Either.cond(current.kind == edit.replacement.kind, (), "Language Studio cannot change an asset's artifact kind")
      yield () }}
      metaResult <- applyEdits(project.metaModule, metaEdits, meta)
      grammarResult <- applyEdits(project.grammarModule, grammarEdits, grammar)
      updated = project.copy(assets = project.assets ++ edits.map(e => e.id -> e.replacement))
      _ <- Either.cond(metaResult._1.digest == updated.metaModule.digest, (),
        "ΔMeta replay result does not equal the proposed meta asset module")
      _ <- Either.cond(grammarResult._1.digest == updated.grammarModule.digest, (),
        "ΔGrammar replay result does not equal the proposed grammar asset module")
      _ <- updated.validate
    yield LanguageStudioProposal(project.digest, updated, metaResult._2, metaResult._3,
      grammarResult._2, grammarResult._3)
