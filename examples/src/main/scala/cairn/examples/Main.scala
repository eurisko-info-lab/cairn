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
      // M41/M42: language definitions as checked-in .cairn text
      val dir = java.nio.file.Path.of("languages")
      java.nio.file.Files.createDirectories(dir)
      val stlcText = cairn.workbench.Meta.printLanguage("stlc", cairn.examples.stlc.Stlc.fragments)
        .fold(e => { System.err.println(e); sys.exit(1) }, identity)
      java.nio.file.Files.writeString(dir.resolve("stlc.cairn"), stlcText)
      val metaText = cairn.workbench.Meta.printLanguage("meta", List(cairn.workbench.Meta.fragment))
        .fold(e => { System.err.println(e); sys.exit(1) }, identity)
      java.nio.file.Files.writeString(dir.resolve("meta.cairn"), metaText)
      // Exemplars: re-print from loaded own fragments (source of truth stays .cairn)
      for name <- List("pki", "law", "sds") do
        val fs = cairn.workbench.PackLoader.requireOwn(name)
        val text = cairn.workbench.Meta.printLanguage(name, fs)
          .fold(e => { System.err.println(e); sys.exit(1) }, identity)
        java.nio.file.Files.writeString(dir.resolve(s"$name.cairn"), text)
        println(s"wrote languages/$name.cairn (${text.length} bytes)")
      println(s"wrote languages/stlc.cairn (${stlcText.length} bytes) and languages/meta.cairn (${metaText.length} bytes)")
    case other =>
      Cli.main(other, packs, portModules) match
        case Right(out) => println(out)
        case Left(err)  => System.err.println(s"error: $err"); sys.exit(1)
