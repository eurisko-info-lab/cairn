package cairn.examples

import cairn.surface.Cli

/** The `cairn` command-line entry point: the generic surface CLI wired with
  * the shipped example language packs (packs are injected here so the surface
  * layer stays domain-free, §4.11).
  */
@main def Main(args: String*): Unit =
  val packs = Map(
    "stlc" -> cairn.examples.stlc.Stlc.language,
    "pki" -> cairn.examples.pki.Pki.language,
    "sds" -> cairn.examples.sds.Sds.language,
    "law" -> cairn.examples.law.Law.language,
    "search" -> cairn.examples.search.Search.language,
    "query" -> cairn.workbench.Query.language,
    "policy" -> cairn.ledger.PolicyLang.language)
  val portModules = Map(
    "quicksort2" -> cairn.examples.quicksort.QuickSort2.module,
    "quicksortApp" -> cairn.examples.quicksort.QuickSortApp.module)
  args.toList match
    case List("digests") =>
      for (name, lang) <- packs.toList.sortBy(_._1) do
        println(s"language $name ${lang.digest.hex}")
        for f <- lang.fragments do println(s"  fragment ${f.name} ${f.digest.hex}")
      println(s"rosetta quicksort2 ${cairn.examples.quicksort.QuickSort2.module.artifact.digest.hex}")
      println(s"rosetta quicksortApp ${cairn.examples.quicksort.QuickSortApp.module.artifact.digest.hex}")
    case List("emit-languages") =>
      // M41/M42: language definitions as checked-in .cairn text, regenerated
      // format-preservingly wherever that's sound.
      //
      // stlc/meta have an independent Scala source of truth (Stlc.fragments /
      // Meta.fragment) distinct from the checked-in file: compare that
      // AUTHORITATIVE content against whatever's currently on disk, splicing
      // the on-disk file's own bytes (comments included) for declarations
      // that are unchanged, reprinting canonically only what the Scala side
      // actually changed.
      //
      // pki/law/sds/search have NO separate source — PackLoader.requireOwn
      // parses the SAME file this writes back to, so comparing it against
      // itself would always be a trivial no-op (every declaration "matches
      // itself"), silently defeating CI's canonical-form check
      // (`git diff --exit-code languages/`) for any future hand-edit,
      // however malformed. Comparing the WORKING-TREE file (what's on disk
      // now, possibly just hand-edited) against git HEAD's last-committed,
      // already-CI-validated version instead keeps this sound: a
      // declaration that's actually being edited always gets a fresh
      // canonical reprint regardless of how it was hand-typed — only
      // content that's IDENTICAL to something that already passed CI gets
      // to keep its formatting.
      //
      // Both fall back to a full canonical reprint when there's nothing to
      // compare against (new file, or git/the repo unavailable).
      val dir = java.nio.file.Path.of("languages")
      java.nio.file.Files.createDirectories(dir)

      def onDisk(path: java.nio.file.Path): Option[String] =
        if java.nio.file.Files.exists(path) then Some(java.nio.file.Files.readString(path)) else None
      def gitHeadVersion(path: java.nio.file.Path): Option[String] =
        scala.util.Try(scala.sys.process.Process(Seq("git", "show", s"HEAD:$path")).!!).toOption

      def writeFromFragments(name: String, fragments: List[cairn.kernel.Fragment]): String =
        val path = dir.resolve(s"$name.cairn")
        val text = onDisk(path) match
          case Some(existing) =>
            cairn.workbench.Meta.printLanguagePreservingFormat(name, fragments, existing)
              .fold(e => { System.err.println(e); sys.exit(1) }, identity)
          case None =>
            cairn.workbench.Meta.printLanguage(name, fragments)
              .fold(e => { System.err.println(e); sys.exit(1) }, identity)
        java.nio.file.Files.writeString(path, text)
        text

      def writeExemplar(name: String): String =
        val path = dir.resolve(s"$name.cairn")
        val working = onDisk(path).getOrElse(
          throw RuntimeException(s"languages/$name.cairn must already exist (exemplar packs have no other source)"))
        val text = gitHeadVersion(path) match
          case Some(headText) =>
            cairn.workbench.Meta.printLanguagePreservingFormatVsReference(name, working, headText)
              .fold(e => { System.err.println(e); sys.exit(1) }, identity)
          case None =>
            val fs = cairn.workbench.PackLoader.requireOwn(name)
            cairn.workbench.Meta.printLanguage(name, fs).fold(e => { System.err.println(e); sys.exit(1) }, identity)
        java.nio.file.Files.writeString(path, text)
        text

      val stlcText = writeFromFragments("stlc", cairn.examples.stlc.Stlc.fragments)
      val metaText = writeFromFragments("meta", List(cairn.workbench.Meta.fragment))
      for name <- List("pki", "law", "sds", "search") do
        val text = writeExemplar(name)
        println(s"wrote languages/$name.cairn (${text.length} bytes)")
      println(s"wrote languages/stlc.cairn (${stlcText.length} bytes) and languages/meta.cairn (${metaText.length} bytes)")
    case other =>
      Cli.main(other, packs, portModules) match
        case Right(out) => println(out)
        case Left(err)  => System.err.println(s"error: $err"); sys.exit(1)
