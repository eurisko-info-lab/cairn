package cairn.core

import cairn.kernel.*

enum StudioMode:
  case Edit, ReviewProposal, ResolveConflict, AssistMigration, InspectEvidence, CompareHistory, PreviewSurfaces

enum StudioTemplate:
  case Add, Replace, Remove, Rename, ReplaceAt
  def canon: Canon = Canon.CStr(toString.toLowerCase)

final case class StudioCommand(name: String, template: StudioTemplate, targetSort: Option[String] = None):
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name), "template" -> template.canon,
    "targetSort" -> targetSort.fold[Canon](Canon.CTag("none", Canon.CInt(0)))(x => Canon.CTag("some", Canon.CStr(x))))

final case class StudioView(name: String, rootSort: String, fields: List[String]):
  def canon: Canon = Canon.cmap("name" -> Canon.CStr(name), "rootSort" -> Canon.CStr(rootSort),
    "fields" -> Canon.cstrs(fields))

final case class StudioProfileSemantics(
    language: Digest,
    editableRoots: List[String],
    views: List[StudioView],
    commands: List[StudioCommand],
    workflows: List[List[String]],
    preferredProjections: List[Digest],
):
  def canon: Canon = Canon.cmap(
    "language" -> Canon.CStr(language.hex), "editableRoots" -> Canon.cstrs(editableRoots),
    "views" -> Canon.CList(views.map(_.canon)), "commands" -> Canon.CList(commands.map(_.canon)),
    "workflows" -> Canon.CList(workflows.map(Canon.cstrs)),
    "preferredProjections" -> Canon.cstrs(preferredProjections.map(_.hex)))
  def artifact: Artifact = Artifact(ArtifactKind.StudioProfileSemantics, canon)
  def digest: Digest = artifact.digest

object StudioProfileSemantics:
  def fromArtifact(artifact: Artifact): Either[String, StudioProfileSemantics] =
    if artifact.kind != ArtifactKind.StudioProfileSemantics then Left("expected Studio profile semantics artifact")
    else scala.util.Try {
      val c = artifact.body
      def optionalString(x: Canon): Option[String] = x match
        case Canon.CTag("some", Canon.CStr(value)) => Some(value)
        case _ => None
      val views = c.field("views").asList.map(v => StudioView(
        v.field("name").asStr, v.field("rootSort").asStr, v.field("fields").asList.map(_.asStr)))
      val commands = c.field("commands").asList.map { command =>
        val template = StudioTemplate.values.find(_.toString.equalsIgnoreCase(command.field("template").asStr))
          .getOrElse(throw IllegalArgumentException("unknown Studio command template"))
        StudioCommand(command.field("name").asStr, template, optionalString(command.field("targetSort")))
      }
      StudioProfileSemantics(Digest(c.field("language").asStr), c.field("editableRoots").asList.map(_.asStr),
        views, commands, c.field("workflows").asList.map(_.asList.map(_.asStr)),
        c.field("preferredProjections").asList.map(x => Digest(x.asStr)))
    }.toEither.left.map(e => s"invalid Studio profile semantics: ${e.getMessage}")

final case class StudioWidgetHint(sort: String, widget: String):
  def canon: Canon = Canon.cmap("sort" -> Canon.CStr(sort), "widget" -> Canon.CStr(widget))

final case class StudioProfileSurface(
    semantics: Digest,
    grouping: Map[String, String],
    labels: Map[String, String],
    widgetHints: List[StudioWidgetHint],
    helpText: Map[String, String],
    layout: List[String],
):
  private def stringMap(value: Map[String, String]): Canon = Canon.cmap(value.toList.map((k, v) => k -> Canon.CStr(v))* )
  def canon: Canon = Canon.cmap(
    "semantics" -> Canon.CStr(semantics.hex), "grouping" -> stringMap(grouping),
    "labels" -> stringMap(labels), "widgetHints" -> Canon.CList(widgetHints.map(_.canon)),
    "helpText" -> stringMap(helpText), "layout" -> Canon.cstrs(layout))
  def artifact: Artifact = Artifact(ArtifactKind.StudioProfileSurface, canon)
  def digest: Digest = artifact.digest

object StudioProfileSurface:
  def fromArtifact(artifact: Artifact): Either[String, StudioProfileSurface] =
    if artifact.kind != ArtifactKind.StudioProfileSurface then Left("expected Studio profile surface artifact")
    else scala.util.Try {
      val c = artifact.body
      def strings(field: String): Map[String, String] = c.field(field).asMap.map((k, v) => k -> v.asStr)
      StudioProfileSurface(Digest(c.field("semantics").asStr), strings("grouping"), strings("labels"),
        c.field("widgetHints").asList.map(h => StudioWidgetHint(h.field("sort").asStr, h.field("widget").asStr)),
        strings("helpText"), c.field("layout").asList.map(_.asStr))
    }.toEither.left.map(e => s"invalid Studio profile surface: ${e.getMessage}")

final case class StudioProfile(semantics: StudioProfileSemantics, surface: StudioProfileSurface):
  def validate(language: ComposedLanguage): Either[String, Unit] =
    for
      _ <- Either.cond(semantics.language == language.digest, (), "Studio profile targets another language")
      _ <- Either.cond(surface.semantics == semantics.digest, (), "Studio surface targets another semantic profile")
      _ <- semantics.editableRoots.foldLeft[Either[String, Unit]](Right(()))((acc, sort) =>
        acc.flatMap(_ => Either.cond(language.sorts.contains(sort), (), s"unknown Studio editable root '$sort'")))
      names = semantics.commands.map(_.name)
      _ <- Either.cond(names.distinct.size == names.size, (), "duplicate Studio command")
    yield ()

object StudioProfile:
  def fromArtifacts(semantics: Artifact, surface: Artifact, language: ComposedLanguage): Either[String, StudioProfile] =
    for
      semanticProfile <- StudioProfileSemantics.fromArtifact(semantics)
      surfaceProfile <- StudioProfileSurface.fromArtifact(surface)
      profile = StudioProfile(semanticProfile, surfaceProfile)
      _ <- profile.validate(language)
    yield profile

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

final case class StudioNavigationNode(
    label: String, sort: String, location: SemanticLocation, children: List[StudioNavigationNode],
)

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

  private[core] def mutation(language: ComposedLanguage, action: StudioAction): Cst = action match
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
    proposeMany(language, base, List(action), model, gate)

  def proposeMany(
      language: ComposedLanguage,
      base: Module,
      actions: List[StudioAction],
      model: ChangeModel = ChangeModel.default,
      gate: ModuleGate = ModuleGate.passthrough,
  ): Either[String, StudioProposal] =
    val change = ChangeAlgebra.changeset(language, actions.map(mutation(language, _)))
    for
      trace <- ChangeAlgebra.accessTrace(language, base, change, model).left.map(_.render)
      applied <- Delta.apply(language, base, change, model)
      (result, validated) = applied
      _ <- gate.check(result).left.map(_.toString)
    yield StudioProposal(language.digest, base.digest, change, validated, result, trace, Nil)

  /** Generic fallback navigation. Labels are FieldIds and keyed identities;
    * numeric traversal positions are used only while minting SemanticPaths. */
  def navigateDefinition(
      language: ComposedLanguage, name: String, root: Cst,
  ): Either[String, StudioNavigationNode] =
    def walk(term: Cst, indices: List[Int], label: String): Either[String, StudioNavigationNode] =
      pathFromTraversal(language, root, indices).flatMap { path =>
        val location = SemanticLocation.Subtree(name, path)
        val children: Either[String, List[StudioNavigationNode]] = term match
          case Cst.Node(ctor, kids) => kids.zipWithIndex.foldLeft[Either[String, List[StudioNavigationNode]]](Right(Nil)) {
            case (acc, (child, index)) =>
              val childLabel = language.constructors.get(ctor).flatMap(_.fieldIds.lift(index).flatten)
                .orElse(child match
                  case Cst.Node(_, values) => language.keys.values.flatMap(k =>
                    language.constructors.values.find(_.sort == k.sort).flatMap { cd =>
                      cd.fieldIds.indexOf(Some(k.keyField)) match
                        case -1 => None
                        case p => values.lift(p).collect { case Cst.Leaf(v) => s"${k.sort}[$v]" }
                    }).headOption
                  case _ => None)
                .getOrElse(child match
                  case Cst.Node("list", _) => "items"
                  case _ => path.focusSort)
              for xs <- acc; node <- walk(child, indices :+ index, childLabel) yield xs :+ node
            }
          case Cst.Leaf(_) => Right(Nil)
        children.map(StudioNavigationNode(label, path.focusSort, location, _))
      }
    walk(root, Nil, name)

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

final case class StudioUndo(inverse: Cst, restored: Module, validated: Delta.ValidatedChangeSet)

final case class StudioWorkspace(
    language: ComposedLanguage,
    base: Module,
    actions: List[StudioAction] = Nil,
    proposal: Option[StudioProposal] = None,
    model: ChangeModel = ChangeModel.default,
    gate: ModuleGate = ModuleGate.passthrough,
):
  def stage(action: StudioAction): Either[String, StudioWorkspace] =
    val next = actions :+ action
    Studio.proposeMany(language, base, next, model, gate).map(p => copy(actions = next, proposal = Some(p)))

  def undo: Either[String, StudioUndo] = proposal.toRight("Studio workspace has no proposal").flatMap { p =>
    for
      inverse <- ChangeAlgebra.invert(language, base, p.change, model)
      applied <- Delta.apply(language, p.result, inverse, model)
      (restored, vcs) = applied
      _ <- Either.cond(restored.digest == base.digest, (), "Studio inverse did not restore the workspace base")
    yield StudioUndo(inverse, restored, vcs)
  }

  def undoLast: Either[String, StudioWorkspace] =
    for
      _ <- undo // mandatory algebraic proof; no separate UI snapshot semantics
      remaining <- Either.cond(actions.nonEmpty, actions.dropRight(1), "Studio workspace has no staged action")
      next <- if remaining.isEmpty then Right(copy(actions = Nil, proposal = None))
        else Studio.proposeMany(language, base, remaining, model, gate)
          .map(p => copy(actions = remaining, proposal = Some(p)))
    yield next

final case class StudioSession(
    capabilities: ResolvedLanguageCapabilities,
    branch: String,
    constitution: AcceptanceConstitution,
    authority: String,
    base: Module,
    status: StudioBranchStatus,
    workspace: StudioWorkspace,
    profile: Option[StudioProfile],
    mode: StudioMode = StudioMode.Edit,
):
  def withMode(next: StudioMode): StudioSession = copy(mode = next)
