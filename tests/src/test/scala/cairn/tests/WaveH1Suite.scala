package cairn.tests

import cairn.kernel.*
import cairn.workbench.*
import cairn.core.*
import cairn.examples.stlc.Stlc

/** Wave H part 1 (M41–M42): full meta surface + bootstrap fixpoint. */
class WaveH1Suite extends munit.FunSuite:

  test("M41: complete STLC written in the meta surface is digest-identical"):
    val text = Meta.printLanguage("stlc", Stlc.fragments).fold(e => fail(e), identity)
    val fromText = Meta.parseFile(text).fold(e => fail(e), identity)
    assertEquals(fromText.digest, Stlc.language.digest)
    // grammar lives on the surface pack; semantic print has no syntax
    assertEquals(fromText.rewriteRules, Stlc.language.rewriteRules)
    assertEquals(fromText.judgments.keySet, Stlc.language.judgments.keySet)
    val bound = Compose.compose("stlc",
      PackLoader.bindSurface(fromText.fragments, Stlc.defaultSurface)).fold(e => fail(e.map(_.render).mkString), identity)
    assertEquals(bound.grammar, Stlc.language.grammar)

  test("M41: reconstructed judgments agree with the original — check, not just digest"):
    // Digest/grammar/rule equality (above) is structural. This is the
    // behavioral half of the bootstrap fixpoint: one real typing derivation,
    // checked under BOTH judgment sets, must agree — including on rejection.
    // (Cairn's self-description is a single-engine FIXPOINT — the meta
    // language reproduces itself byte-for-byte — not a second, separately
    // implemented kernel to bisimulate against; this is the "check" analogue
    // of that same fixpoint, on the one language here with real judgments.)
    import cairn.examples.claims.Claims
    val text = Meta.printLanguage("stlc", Stlc.fragments).fold(e => fail(e), identity)
    val fromText = Meta.parseFile(text).fold(e => fail(e), identity)
    assertEquals(fromText.canon, Stlc.language.canon)

    val derivation = Claims.idAppliedDerivation
    assertEquals(Checker.check(Stlc.language.judgments.values.toList, derivation), Right(()))
    assertEquals(Checker.check(fromText.judgments.values.toList, derivation), Right(()))

    // a tampered conclusion (claims the wrong type) must be rejected by BOTH
    val tampered = derivation.copy(conclusion = derivation.conclusion match
      case Cst.Node(t, List(ctx, term, _)) =>
        Cst.Node(t, List(ctx, term, Cst.node("arrow", Cst.node("tyBool"), Cst.node("tyBool"))))
      case other => fail(s"unexpected conclusion shape: ${other.render}"))
    assert(Checker.check(Stlc.language.judgments.values.toList, tampered).isLeft)
    assert(Checker.check(fromText.judgments.values.toList, tampered).isLeft)

  test("M41: grammar productions, print rules, infix, rules, judgments all round-trip encode/elaborate"):
    for f <- Stlc.fragments ++ Stlc.surfaceFragments do
      val cst = Meta.encode(f)
      Meta.elaborateFragment(cst) match
        case Right(back) => assertEquals(back, f, s"fragment ${f.name}")
        case Left(e)     => fail(s"${f.name}: $e")

  test("M41: meta text itself round-trips the grammar law"):
    val text = Meta.printLanguage("stlc", Stlc.fragments).fold(e => fail(e), identity)
    val cst = Parser.parse(Meta.grammar, text).fold(e => fail(e), identity)
    RoundTrip.check(Meta.grammar, cst).fold(e => fail(e), identity)

  test("M42: bootstrap fixpoint — the meta language describes itself"):
    // 1. print the meta fragment in its own surface
    val metaText = Meta.printLanguage("meta", List(Meta.fragment)).fold(e => fail(e), identity)
    // 2. parse it with the SEED grammar and compose
    val l2 = Meta.parseFile(metaText).fold(e => fail(e), identity)
    // 3. the reconstructed language IS the seed, digest for digest
    assertEquals(l2.digest, Meta.language.digest)
    assertEquals(l2.grammar, Meta.grammar)
    // 4. reparse the text under the RECONSTRUCTED grammar: same fragment again
    val cst2 = Parser.parse(l2.grammar, metaText).fold(e => fail(e), identity)
    cst2 match
      case Cst.Node("file", List(Cst.Leaf("meta"), Cst.Node("list", List(fragCst)))) =>
        assertEquals(Meta.elaborateFragment(fragCst), Right(Meta.fragment))
      case other => fail(s"unexpected: ${other.render}")

  test("M42: checked-in language files load at runtime (no recompile)"):
    val lang = PackLoader.requireClosed("stlc")
    assertEquals(lang.digest, Stlc.language.digest)
    assertEquals(lang.grammar, Stlc.language.grammar)

  test("M42: a brand-new toy language as pure text — parse, eval, no Scala"):
    val toySrc = """
      |language toy {
      |  fragment flips provides flips {
      |    sort T tree ;
      |    keyword yes ;
      |    keyword no ;
      |    keyword flip ;
      |    syntax t += yes : tok "yes" ;
      |    syntax t += no : tok "no" ;
      |    syntax t += flip : tok "flip" cat t ;
      |    print yes : lit "yes" ;
      |    print no : lit "no" ;
      |    print flip : lit "flip" sp field 0 ;
      |    rule flip-yes : flip(yes()) => no() ;
      |    rule flip-no : flip(no()) => yes() ;
      |    top t ;
      |  }
      |}""".stripMargin
    val toy = Meta.parseFile(toySrc).fold(e => fail(e), identity)
    val term = Parser.parse(toy.grammar, "flip flip flip yes").fold(e => fail(e), identity)
    assertEquals(cairn.core.TreeEngine.normalize(toy, term), Right(Cst.node("no")))
    // and of course the forced closure applies to a text-born language too
    assert(Delta.deltaOf(toy).isRight)
