package cairn.core

import cairn.kernel.*

final case class StudioField(
    fieldId: Option[String], label: Option[String], sort: String, position: Int,
):
  def canon: Canon = Canon.cmap(
    "fieldId" -> fieldId.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", Canon.CStr(x))),
    "label" -> label.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", Canon.CStr(x))),
    "sort" -> Canon.CStr(sort), "position" -> Canon.CInt(position))

final case class StudioForm(
    constructor: String, sort: String, fields: List[StudioField], keyedBy: Option[String],
):
  def canon: Canon = Canon.cmap(
    "constructor" -> Canon.CStr(constructor), "sort" -> Canon.CStr(sort),
    "fields" -> Canon.CList(fields.map(_.canon)),
    "keyedBy" -> keyedBy.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", Canon.CStr(x))))

enum StudioAction:
  case Add(name: String, term: Cst)
  case Replace(name: String, term: Cst)
  case Remove(name: String)
  case Rename(from: String, to: String)
  case ReplaceAt(name: String, path: SemanticPath, term: Cst)

final case class StudioDiagnostic(message: String, location: Option[SemanticLocation])

/** A Studio proposal is ordinary ΔL plus its replay witness. `result` is a
  * preview, never a directly mutable module reference. */
final case class StudioProposal(
    language: Digest,
    base: Digest,
    change: Cst,
    validatedChange: Delta.ValidatedChangeSet,
    result: Module,
    accesses: AccessTrace,
    diagnostics: List[StudioDiagnostic],
    sourcePreview: Option[String] = None,
):
  def canon: Canon = Canon.cmap(
    "language" -> Canon.CStr(language.hex), "base" -> Canon.CStr(base.hex),
    "change" -> Cst.toCanon(change), "validatedChange" -> Canon.CStr(validatedChange.artifact.digest.hex),
    "result" -> Canon.CStr(result.digest.hex),
    "accesses" -> Canon.CList(accesses.accesses.map(a => Canon.cmap(
      "mode" -> Canon.CStr(a.mode.toString.toLowerCase), "location" -> a.location.canon))),
    "diagnostics" -> Canon.CList(diagnostics.map(d => Canon.cmap(
      "message" -> Canon.CStr(d.message),
      "location" -> d.location.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", x.canon))))),
    "sourcePreview" -> sourcePreview.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", Canon.CStr(x))))
  def artifact: Artifact = Artifact(ArtifactKind.ChangeSet, Canon.CTag("studio-proposal", canon))

object Studio:
  /** Forms are entirely constructor metadata: persistent field identity,
    * grammar label, child sort, and keyed-list identity declaration. */
  def forms(language: ComposedLanguage, sort: String): List[StudioForm] =
    language.constructors.values.filter(_.sort == sort).toList.sortBy(_.name).map { ctor =>
      val fields = ctor.argSorts.indices.toList.map { position =>
        StudioField(ctor.fieldIds.lift(position).flatten, ctor.argLabels.lift(position).flatten,
          ctor.argSorts(position), position)
      }
      StudioForm(ctor.name, sort, fields, language.keys.get(sort).map(_.keyField))
    }

  /** Recover a semantic path from a UI traversal. Keyed list elements become
    * `KeyedElement` steps here; the Studio never treats a position as identity. */
  def pathFromTraversal(
      language: ComposedLanguage, root: Cst, indices: List[Int],
  ): Either[String, SemanticPath] = SemanticPath.fromLegacyPath(language, root, indices)

  private def mutation(language: ComposedLanguage, action: StudioAction): Cst = action match
    case StudioAction.Add(name, term) =>
      Cst.node(Delta.tag(language, "add"), Cst.Leaf(name), term)
    case StudioAction.Replace(name, term) =>
      Cst.node(Delta.tag(language, "replace"), Cst.Leaf(name), term)
    case StudioAction.Remove(name) =>
      Cst.node(Delta.tag(language, "remove"), Cst.Leaf(name))
    case StudioAction.Rename(from, to) =>
      Cst.node(Delta.tag(language, "rename"), Cst.Leaf(from), Cst.Leaf(to),
        Cst.Node("list", List(Cst.Leaf(from))))
    case StudioAction.ReplaceAt(name, path, term) =>
      Cst.node(Delta.tag(language, "edit"), Cst.Leaf(name),
        Cst.Node("list", path.indices.map(i => Cst.Leaf(i.toString))), term)

  private def actionLocation(action: StudioAction): Option[SemanticLocation] = action match
    case StudioAction.Add(name, _)        => Some(SemanticLocation.Binding(name))
    case StudioAction.Replace(name, _)    => Some(SemanticLocation.WholeDefinition(name))
    case StudioAction.Remove(name)        => Some(SemanticLocation.Binding(name))
    case StudioAction.Rename(from, _)     => Some(SemanticLocation.Binding(from))
    case StudioAction.ReplaceAt(name, path, _) => Some(SemanticLocation.Subtree(name, path))

  def propose(
      language: ComposedLanguage,
      base: Module,
      action: StudioAction,
      model: ChangeModel = ChangeModel.default,
      gate: ModuleGate = ModuleGate.passthrough,
  ): Either[String, StudioProposal] =
    val change = ChangeAlgebra.changeset(language, List(mutation(language, action)))
    for
      trace <- ChangeAlgebra.accessTrace(language, base, change, model).left.map(_.render)
      applied <- Delta.apply(language, base, change, model)
      (result, validated) = applied
      _ <- gate.check(result).left.map(_.toString)
    yield StudioProposal(language.digest, base.digest, change, validated, result, trace, Nil)

  /** Source preservation is another view of the same proposal/change; it does
    * not provide a second mutation path. */
  def proposePreservingSource(
      language: ComposedLanguage,
      moduleGrammar: GrammarSpec,
      source: String,
      action: StudioAction,
      model: ChangeModel = ChangeModel.default,
      gate: ModuleGate = ModuleGate.passthrough,
  ): Either[String, StudioProposal] =
    for
      parsed <- Parser.parse(moduleGrammar, source)
      base <- ModuleSurface.toModule(parsed)
      proposal <- propose(language, base, action, model, gate)
      preview <- Delta.applyPreservingFormat(language, moduleGrammar, source, proposal.change, model)
    yield proposal.copy(sourcePreview = Some(preview))

  /** Inline diagnostics use the same replay/gate as submission and retain the
    * semantic widget location that produced the rejected Δ term. */
  def diagnostics(
      language: ComposedLanguage, base: Module, action: StudioAction,
      model: ChangeModel = ChangeModel.default, gate: ModuleGate = ModuleGate.passthrough,
  ): List[StudioDiagnostic] =
    propose(language, base, action, model, gate) match
      case Right(_) => Nil
      case Left(message) => List(StudioDiagnostic(message, actionLocation(action)))

  /** Migration assistance transports the already-emitted ordinary change and
    * replays it against the migrated base under the target capability bundle. */
  def assistMigration(
      migration: ResolvedMigration, base: Module, proposal: StudioProposal,
      gate: ModuleGate = ModuleGate.passthrough,
  ): Either[String, StudioProposal] =
    for
      _ <- Either.cond(proposal.language == migration.source.language.digest, (),
        "Studio proposal targets another migration source")
      migratedBase <- migration.module(base)
      migratedChange <- migration.change(proposal.change)
      trace <- ChangeAlgebra.accessTrace(
        migration.target.language, migratedBase, migratedChange, migration.target.changeModel).left.map(_.render)
      applied <- Delta.apply(
        migration.target.language, migratedBase, migratedChange, migration.target.changeModel)
      (result, vcs) = applied
      _ <- gate.check(result).left.map(_.toString)
    yield StudioProposal(migration.target.language.digest, migratedBase.digest, migratedChange,
      vcs, result, trace, Nil)

final case class StudioBranchStatus(
    branch: String,
    history: List[Digest],
    conflict: Option[Digest],
    overlappingLocations: Set[SemanticLocation],
    migration: Option[Digest],
    acceptanceEvidence: Option[Digest],
    certificates: List[Digest],
)
