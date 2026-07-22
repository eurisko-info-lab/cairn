package cairn.examples

import cairn.runtime.PackLoader
import cairn.surface.Cli
import cairn.systemhandler.{EffectContext, Filesystem}
import cairn.systeminterface.Filesystem as Fs
import java.nio.file.Path

/** The `cairn` command-line entry point: the generic surface CLI wired with
  * the shipped example language packs (packs are injected here so the surface
  * layer stays domain-free, §4.11).
  */
@main def Main(args: String*): Unit =
    // Composition roots use narrow deployment policies (not allow-all bootstrap).
    val workspaceCtx = EffectContext.forPackLoader()
    val packLoader = PackLoader(workspaceCtx)
    val ledgerCtx = EffectContext.forLedger()
    val processCtx = EffectContext.forProcess()
    val lspCtx = EffectContext.forLsp()
    val fsCtx = EffectContext.forFilesystem()
    val pki = cairn.examples.pki.Pki(packLoader)
    val law = cairn.examples.law.Law(packLoader)
    val sds = cairn.examples.sds.Sds(packLoader)
    val search = cairn.examples.search.Search(packLoader)
    val euClp = packLoader.requireClosed("eu-clp")
    val sdsReport = packLoader.requireClosed("sds-report")
    val packs = Map(
      "stlc" -> packLoader.requireClosed("stlc"),
      "pki" -> pki.language,
      "sds" -> sds.language,
      "law" -> law.language,
      "search" -> search.language,
      "eu-clp" -> euClp,
      "sds-report" -> sdsReport,
      "query" -> packLoader.requireClosed("query"),
      "policy" -> packLoader.requireClosed("policy"))
    val portModules = Map(
      "quicksort2" -> cairn.examples.quicksort.QuickSort2.module,
      "quicksortApp" -> cairn.examples.quicksort.QuickSortApp.module)
    args.toList match
      case List("sds-tutorial") =>
        val work = java.nio.file.Files.createTempDirectory("cairn-sds-corpus-tutorial")
        println(cairn.examples.sds.SdsCorpusTutorial.run(work).render)
      case List("digests") =>
        for (name, lang) <- packs.toList.sortBy(_._1) do
          println(s"language $name ${lang.digest.hex}")
          for f <- lang.fragments do println(s"  fragment ${f.name} ${f.digest.hex}")
        println(s"rosetta quicksort2 ${cairn.examples.quicksort.QuickSort2.module.artifact.digest.hex}")
        println(s"rosetta quicksortApp ${cairn.examples.quicksort.QuickSortApp.module.artifact.digest.hex}")
      case List("emit-languages") =>
        // Language definitions as checked-in .cairn text. STLC/meta join the
        // exemplar path: on-disk files are the runtime source of truth
        // (PackLoader), format-preserved against git HEAD so CI's
        // `git diff --exit-code languages/` stays meaningful. Scala
        // `Stlc.fragments` / `Meta.fragment` remain the bootstrap *seed*
        // (digest-equality / fixpoint tests), not the emit authority.
        // Path I/O is gated through Filesystem (`forFilesystem`).
        val dir = Path.of("languages")
        def fsPath(p: Path): Fs.Path = Fs.Path(p.toAbsolutePath.normalize.toString)
        def fsFail(e: Fs.Error): Nothing =
          System.err.println(e); sys.exit(1)
        def fsMkdirs(p: Path): Unit =
          Filesystem.run(Fs.Request.Mkdirs(fsPath(p)), fsCtx).fold(fsFail, _ => ())
        def fsWrite(p: Path, text: String): Unit =
          Filesystem.run(Fs.Request.Write(fsPath(p), text), fsCtx).fold(fsFail, _ => ())
        def onDisk(path: Path): Option[String] =
          Filesystem.run(Fs.Request.Exists(fsPath(path)), fsCtx) match
            case Right(Fs.Response.Bool(true)) =>
              Filesystem.run(Fs.Request.Read(fsPath(path)), fsCtx) match
                case Right(Fs.Response.Text(s)) => Some(s)
                case Left(e)                    => fsFail(e)
                case Right(other)               => System.err.println(s"unexpected fs: $other"); sys.exit(1)
            case Right(Fs.Response.Bool(false)) => None
            case Left(e)                        => fsFail(e)
            case Right(other)                   => System.err.println(s"unexpected fs: $other"); sys.exit(1)
        def gitHeadVersion(path: Path): Option[String] =
          // Quietly miss when the path isn't in HEAD yet (new surface files).
          scala.util.Try(
            scala.sys.process.Process(Seq("git", "show", s"HEAD:$path"))
              .!!(scala.sys.process.ProcessLogger(_ => (), _ => ()))
          ).toOption

        fsMkdirs(dir)

        /** On-disk SoT packs: format-preserve against git HEAD. */
        def writeExemplarPair(name: String): (String, String) =
          val semPath = dir.resolve(s"$name.cairn")
          val surfPath = dir.resolve(name).resolve("surfaces").resolve("default.cairn")
          def rewriteLanguage(path: Path, langName: String): String =
            val working = onDisk(path).getOrElse(
              throw RuntimeException(s"$path must already exist (exemplar packs have no other source)"))
            val text = gitHeadVersion(path) match
              case Some(headText) =>
                cairn.core.Meta.printLanguagePreservingFormatVsReference(langName, working, headText)
                  .fold(e => { System.err.println(e); sys.exit(1) }, identity)
              case None =>
                cairn.core.Meta.parseLanguageAst(working) match
                  case Right((n, fs)) =>
                    cairn.core.Meta.printLanguage(n, fs).fold(e => { System.err.println(e); sys.exit(1) }, identity)
                  case Left(e) => System.err.println(e); sys.exit(1)
            fsWrite(path, text)
            text
          def rewriteSurface(path: Path, style: String, language: String): String =
            val working = onDisk(path).getOrElse(
              throw RuntimeException(s"$path must already exist (exemplar packs have no other source)"))
            val text = gitHeadVersion(path) match
              case Some(headText) =>
                cairn.core.Meta.printSurfacePreservingFormatVsReference(style, language, working, headText)
                  .fold(e => { System.err.println(e); sys.exit(1) }, identity)
              case None =>
                cairn.core.Meta.parseSurfaceAst(working) match
                  case Right((n, l, fs)) =>
                    cairn.core.Meta.printSurface(n, l, fs).fold(e => { System.err.println(e); sys.exit(1) }, identity)
                  case Left(e) => System.err.println(e); sys.exit(1)
            fsWrite(path, text)
            text
          (rewriteLanguage(semPath, name), rewriteSurface(surfPath, "default", name))

        def writeExemplarLanguage(name: String): String =
          val path = dir.resolve(s"$name.cairn")
          val working = onDisk(path).getOrElse(
            throw RuntimeException(s"$path must already exist (language packs have no other emit source)"))
          val text = gitHeadVersion(path) match
            case Some(headText) =>
              cairn.core.Meta.printLanguagePreservingFormatVsReference(name, working, headText)
                .fold(e => { System.err.println(e); sys.exit(1) }, identity)
            case None =>
              cairn.core.Meta.parseLanguageAst(working) match
                case Right((n, fs)) =>
                  cairn.core.Meta.printLanguage(n, fs).fold(e => { System.err.println(e); sys.exit(1) }, identity)
                case Left(e) => System.err.println(e); sys.exit(1)
          fsWrite(path, text)
          text

        // STLC/meta are on-disk SoT (like exemplars); Scala seeds stay for fixpoint tests only.
        // Query/policy join the same path: Core-facing languages described in .cairn.
        for name <- List("pki", "law", "sds", "search", "stlc", "query", "policy") do
          val (sem, surf) = writeExemplarPair(name)
          println(s"wrote languages/$name.cairn (${sem.length} bytes) + surfaces/default.cairn (${surf.length} bytes)")
        val metaText = writeExemplarLanguage("meta")
        println(s"wrote languages/meta.cairn (${metaText.length} bytes) (fused; Meta describes surface tops)")
      case other =>
        Cli.main(other, packs, portModules, packLoader, ledgerCtx, processCtx, lspCtx, fsCtx) match
          case Right(out) => println(out)
          case Left(err)  => System.err.println(s"error: $err"); sys.exit(1)
