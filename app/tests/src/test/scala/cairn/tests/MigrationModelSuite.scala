package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc

/** PR14: migrations are pack data, resolve provider bundles, and transport
  * every language-associated runtime/stored shape through one model. */
class MigrationModelSuite extends munit.FunSuite:
  private val source = LanguageCapabilities.standard(Stlc.language)
  private val targetLanguage = Compose.compose("stlc-v2",
    Stlc.language.fragments :+ Fragment("stlc-v2-revision", List("stlc-v2"), Nil))
      .fold(es => fail(es.map(_.render).mkString("\n")), identity)
  private val target = LanguageCapabilities.standard(targetLanguage)
  private val zero = Digest.of(Canon.CStr("unresolved-provider"))
  private val template = LangMigration(
    zero, zero, Map.empty, Map.empty,
    wrapperRenames = Map("legacy-envelope" -> "current-envelope"),
    wrapperFieldRemap = Map("current-envelope" -> List(Left(1), Left(0))))
  private val declaration = MigrationDeclaration("old", "new", template.canon)
  private val ownerFragment = Fragment("migration-pack", List("migration"), Nil,
    providers = Map("old" -> "stlc", "new" -> "stlc-v2"),
    migrations = List(declaration.canon))
  private val owner = Compose.compose("migration-pack", List(ownerFragment))
    .fold(es => fail(es.map(_.render).mkString("\n")), identity)

  private def resolve(name: String): Option[ResolvedLanguageCapabilities] = name match
    case "stlc"    => Some(source)
    case "stlc-v2" => Some(target)
    case _          => None

  private val migration = MigrationModelLoader.fromLanguage(owner, resolve)
    .fold(e => fail(e), identity).head

  test("migration declarations round-trip through the Meta pack surface"):
    val text = Meta.printLanguage("migration_pack", List(ownerFragment)).fold(e => fail(e), identity)
    assert(text.contains("migration from old to new model"), text)
    val (_, decoded) = Meta.parseLanguageAst(text).fold(e => fail(e), identity)
    assertEquals(decoded.flatMap(_.migrations), List(declaration.canon))
    assertEquals(LangMigration.fromArtifact(migration.artifact), Right(migration.model))

  test("provider resolution binds source/target digests and fails closed"):
    assertEquals(migration.model.fromLang, source.language.digest)
    assertEquals(migration.model.toLang, target.language.digest)
    assert(MigrationModelLoader.fromLanguage(owner, _ => None).isLeft)

  test("wrapper-shape migration is decoded data, not a term-constructor special case"):
    val old = Cst.Node("legacy-envelope", List(Cst.Leaf("a"), Cst.Leaf("b")))
    val transported = Migrate.changeset(migration.model, source.language, target.language, old)
      .fold(e => fail(e), identity)
    assertEquals(transported, Cst.Node("current-envelope", List(Cst.Leaf("b"), Cst.Leaf("a"))))

  test("resolved migration transports modules, changes, validation and capability bundles"):
    val base = Module(List("x" -> Stlc.tru))
    assertEquals(migration.module(base).map(_.digest), Right(base.digest))
    val validation = ValidationModel(source.language.digest, Nil, Nil)
    assertEquals(migration.validation(validation).map(_.targetLanguage), Right(target.language.digest))
    assertEquals(migration.changeCapability(ChangeCapability.standard), Right(ChangeCapability.standard))
    assertEquals(migration.languageCapabilities(source.descriptor), Right(target.descriptor))

  test("pending edits and stored conflicts retain content while retargeting the revision"):
    val base = Module(List("x" -> Stlc.tru))
    val empty = ChangeAlgebra.changeset(source.language, Nil)
    val pending = migration.pending(PendingEdit(source.language.digest, base, empty))
      .fold(e => fail(e), identity)
    assertEquals(pending.language, target.language.digest)
    val conflict = Merge.Conflict(Set.empty, base.digest, base.digest)
    val stored = migration.conflict(StoredConflict(source.language.digest, base, empty, empty, conflict))
      .fold(e => fail(e), identity)
    assertEquals(stored.language, target.language.digest)
    assert(stored.changeA.render.contains(target.language.name))

  test("migration validation rejects a target model bound to another revision"):
    val badValidation = ValidationModel(source.language.digest, Nil, Nil)
    val badTarget = target.copy(validation = Some(badValidation),
      descriptor = target.descriptor.copy(validation = Some(badValidation.digest)))
    assert(MigrationModelLoader.validate(migration.model, source, badTarget).isLeft)

  test("Studio migration mode transports the pending proposal and records migration evidence"):
    val base = Module(List("x" -> Stlc.tru))
    val workspace = StudioWorkspace(source.language, base, model = source.changeModel)
      .stage(StudioAction.Replace("x", Stlc.fls)).fold(e => fail(e), identity)
    val session = StudioSession(source, "main", AcceptanceConstitution.open(source.changeModel.digest),
      "editor", base, StudioBranchStatus("main", Nil, None, Set.empty, None, None, Nil),
      workspace, None)
    val migrated = session.assistMigration(migration).fold(e => fail(e), identity)
    assertEquals(migrated.mode, StudioMode.AssistMigration)
    assertEquals(migrated.capabilities.language.digest, target.language.digest)
    assertEquals(migrated.workspace.proposal.get.language, target.language.digest)
    assertEquals(migrated.migration.map(_.model.artifact.digest), Some(migration.model.artifact.digest))
    assertEquals(migrated.workspace.proposal.get.result.get("x"), Some(Stlc.fls))
