package cairn.examples.search

import cairn.kernel.*
import cairn.workbench.*
import cairn.ledger.Provenance
import java.nio.file.{Files, Path}

/** Thin Search tutorial: seed origin+goal, ΔL-add Intent and Fact into CAS,
  * record Intent→Fact provenance. No LLM / dispatcher.
  */
object SearchTutorial:
  final case class Report(
      languageProvides: Set[String],
      languageRequiresMet: Boolean,
      wellFormed: Boolean,
      goalMet: Boolean,
      factDigest: String,
      intentDigest: String,
      whyHops: Int,
      whyTools: List[String],
      boardDigest: String
  )

  def run(workDir: Path = Files.createTempDirectory("cairn-search")): Report =
    Files.createDirectories(workDir)
    val cas = DiskCas(workDir.resolve("cas"))
    val lang = Search.language
    val provides = lang.fragments.flatMap(_.provides).toSet
    val requires = lang.fragments.flatMap(_.requires).toSet
    val met = requires.subsetOf(provides)

    val seed = Search.seedBoard
    Search.wellFormed(seed).fold(e => throw RuntimeException(e), identity)

    val dl = Delta.deltaOf(lang).fold(e => throw RuntimeException(e.map(_.render).mkString), identity)
    val addIntent = Parser.parse(dl.grammar,
      """{ add explore = intent "gather evidence for the goal" ; }"""
    ).fold(e => throw RuntimeException(e), identity)
    val (withIntent, _) = Search.applySearch(seed, addIntent)
      .fold(e => throw RuntimeException(e), identity)

    val intentTerm = withIntent.get("explore").get
    val intentDig = Search.putTerm(cas, intentTerm)

    val addFact = Parser.parse(dl.grammar,
      """{ add finding = fact "evidence confirms the finding" ;
          add link = supports explore finding ;
          add spawn = spawns start explore ; }"""
    ).fold(e => throw RuntimeException(e), identity)
    val (board, _) = Search.applySearch(withIntent, addFact)
      .fold(e => throw RuntimeException(e), identity)
    Search.wellFormed(board).fold(e => throw RuntimeException(e), identity)

    val factTerm = board.get("finding").get
    val factDig = Search.putFact(cas, factTerm, intentDig, "explore")
    val edgeCert = Search.certifyEdge(cas, board, "link", factDig)
      .fold(e => throw RuntimeException(e), identity)

    cas.put(lang.artifact)
    val boardArt = cas.put(board.artifact)
    Provenance.record(cas, boardArt.valueHash, List(lang.digest, factDig, intentDig, edgeCert.certDigest), "search-board")

    val hops = Provenance.why(workDir.resolve("cas"), edgeCert.certDigest)
    Report(
      languageProvides = provides,
      languageRequiresMet = met,
      wellFormed = Search.wellFormed(board).isRight,
      goalMet = Search.goalMet(board),
      factDigest = factDig.hex,
      intentDigest = intentDig.hex,
      whyHops = hops.length,
      whyTools = hops.map(_.record.tool),
      boardDigest = boardArt.valueHash.hex
    )
