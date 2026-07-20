package cairn.examples

import cairn.surface.Cli

/** The `cairn` command-line entry point: the generic surface CLI wired with
  * the shipped example language packs (packs are injected here so the surface
  * layer stays domain-free, §4.11).
  */
@main def Main(args: String*): Unit =
  val packs = Map(
    "stlc" -> cairn.examples.stlc.Stlc.language,
    "pki" -> cairn.examples.pki.Pki.language)
  args.toList match
    case List("digests") =>
      // golden digests of shipped artifacts (referenced by STATUS.md)
      for (name, lang) <- packs.toList.sortBy(_._1) do
        println(s"language $name ${lang.digest.hex}")
        for f <- lang.fragments do println(s"  fragment ${f.name} ${f.digest.hex}")
      println(s"rosetta quicksort ${cairn.examples.quicksort.QuickSort.module.artifact.digest.hex}")
    case other =>
      Cli.main(other, packs) match
        case Right(out) => println(out)
        case Left(err)  => System.err.println(s"error: $err"); sys.exit(1)
