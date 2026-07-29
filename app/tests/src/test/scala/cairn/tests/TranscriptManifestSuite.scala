package cairn.tests

import cairn.runtime.EffectContexts
import cairn.runtime.PackLoader
import cairn.surface.Transcript
import cairn.systemhandler.Filesystem
import cairn.systeminterface.Filesystem as Fs

class TranscriptManifestSuite extends munit.FunSuite:
  private val packs = PackLoader(EffectContexts.forPackLoader())
  private val ledgerCtx = EffectContexts.forLedger()
  private val processCtx = EffectContexts.forProcess()
  private val fsCtx = EffectContexts.forFilesystem()

  private def readFile(path: java.nio.file.Path): String =
    Filesystem.run(Fs.Request.Read(Fs.Path(path.toString)), fsCtx) match
      case Right(Fs.Response.Text(s)) => s
      case Left(e) => fail(e.toString)
      case Right(other) => fail(s"unexpected fs response: $other")

  private def findExisting(candidates: List[String], missing: String): java.nio.file.Path =
    val paths = candidates.map(java.nio.file.Path.of(_))
    paths.view
      .find { p =>
        Filesystem.run(Fs.Request.Exists(Fs.Path(p.toString)), fsCtx) match
          case Right(Fs.Response.Bool(true)) => true
          case _ => false
      }
      .getOrElse(fail(missing))

  test("transcripts listed in manifest run successfully"):
    val manifest = findExisting(
      List("transcripts/test-manifest.tsv", "../transcripts/test-manifest.tsv"),
      "transcripts/test-manifest.tsv not found",
    )
    val entries = readFile(manifest).linesIterator
      .map(_.trim)
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .toList
    assert(entries.nonEmpty, "transcript manifest is empty")

    val loadedPacks = packs.loadClosed()
    entries.foreach { rel =>
      val srcPath = findExisting(List(rel, s"../$rel"), s"$rel not found")
      val src = readFile(srcPath)
      val work = java.nio.file.Files.createTempDirectory(s"cairn-manifest-${srcPath.getFileName.toString}")
      Transcript.run(src, loadedPacks, work, Map.empty, packs, ledgerCtx, processCtx, fsCtx) match
        case Right(report) =>
          assert(report.steps.nonEmpty, s"$rel produced no transcript steps")
        case Left(err) =>
          fail(s"$rel failed: $err")
    }
