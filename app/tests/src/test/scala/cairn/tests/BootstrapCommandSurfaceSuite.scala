package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.{EffectContexts, PackLoader}
import cairn.surface.Cli
import cairn.systemhandler.DiskCas
import cairn.user.quicksort.QuickSort2
import java.nio.file.Files

class BootstrapCommandSurfaceSuite extends munit.FunSuite:
  private val language = PackLoader(EffectContexts.forPackLoader()).requireClosed("stlc")
  private val capabilities = LanguageCapabilities.standard(language)
  private val grammarArtifact = Artifact(ArtifactKind.Grammar, GrammarSpec.toCanon(language.grammar))
  private val constitution = AcceptanceConstitution.open(capabilities.changeModel.digest)
  private val runtime = ResolvedDomainRuntime.create(capabilities, constitution).toOption.get
  private val declaredMachine = GenericMachine.declare(
    List(runtime.digest),
    Map("change" -> capabilities.change.semantics.digest),
  )
  private val machine = declaredMachine.machine
  private val manifest = ApplicationManifest(
    "artifact-app",
    machine.digest,
    List(
      ApplicationLanguage(
        "stlc",
        language.digest,
        grammarArtifact.digest,
        capabilities.descriptor.digest,
        Some(runtime.digest),
      ),
    ),
    List(ApplicationEntry("quicksort", QuickSort2.module.artifact.digest, ArtifactKind.RosettaDecl)),
  )

  private val packLoader = PackLoader(EffectContexts.forPackLoader())
  private val ledgerCtx = EffectContexts.forLedger()
  private val processCtx = EffectContexts.forProcess()
  private val lspCtx = EffectContexts.forLsp()
  private val fsCtx = EffectContexts.forFilesystem()

  private def seedSource(path: java.nio.file.Path): Unit =
    val cas = DiskCas(path)
    language.fragments.foreach(f => cas.put(f.artifact))
    List(
      language.artifact,
      grammarArtifact,
      capabilities.change.semantics.artifact,
      capabilities.change.surface.artifact,
      capabilities.descriptor.artifact,
      constitution.artifact,
      runtime.descriptor.artifact,
      machine.artifact,
      QuickSort2.module.artifact,
      manifest.artifact,
    ).foreach(cas.put)
    declaredMachine.installInto(cas.put)

  private def cli(args: List[String]): Either[String, String] =
    Cli.main(args, Map.empty, Map.empty, packLoader, ledgerCtx, processCtx, lspCtx, fsCtx)

  test("top-level verify-application resolves an artifact-rooted app"):
    val source = Files.createTempDirectory("cairn-bootstrap-verify-app")
    seedSource(source)
    val out = cli(List("verify-application", manifest.digest.hex, source.toString))
      .fold(e => fail(e), identity)
    assert(out.contains("verified application artifact-app"), out)

  test("top-level verify-foundation audits trusted closure"):
    val source = Files.createTempDirectory("cairn-bootstrap-verify-foundation")
    seedSource(source)
    val out = cli(List("verify-foundation", manifest.digest.hex, source.toString))
      .fold(e => fail(e), identity)
    assert(out.contains("verified foundation"), out)
    assert(out.contains(s"root=${manifest.digest.hex}"), out)

  test("top-level derive executes CKC resolve query"):
    val source = Files.createTempDirectory("cairn-bootstrap-derive")
    seedSource(source)
    val out = cli(List("derive", "resolve", source.toString, manifest.digest.hex))
      .fold(e => fail(e), identity)
    assert(out.startsWith("valid evidence="), out)

  test("top-level export-closure and import-closure copy complete closure by root"):
    val source = Files.createTempDirectory("cairn-bootstrap-export-source")
    val exported = Files.createTempDirectory("cairn-bootstrap-export-target")
    val imported = Files.createTempDirectory("cairn-bootstrap-import-target")
    seedSource(source)

    val exportedMsg = cli(List("export-closure", manifest.digest.hex, source.toString, exported.toString))
      .fold(e => fail(e), identity)
    assert(exportedMsg.contains("exported closure"), exportedMsg)

    val verifyExported = cli(List("verify-application", manifest.digest.hex, exported.toString))
    assert(verifyExported.isRight, verifyExported.toString)

    val importedMsg = cli(List("import-closure", manifest.digest.hex, exported.toString, imported.toString))
      .fold(e => fail(e), identity)
    assert(importedMsg.contains("imported closure"), importedMsg)

    val verifyImported = cli(List("verify-application", manifest.digest.hex, imported.toString))
    assert(verifyImported.isRight, verifyImported.toString)
