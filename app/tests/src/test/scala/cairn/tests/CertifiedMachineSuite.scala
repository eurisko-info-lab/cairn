package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc

class CertifiedMachineSuite extends munit.FunSuite:
  private val implementation = Digest.of(Canon.CStr("alternate-interpreter-v1"))

  test("compiled parser and rewrite engine emit independently decodable equivalence evidence"):
    val source = "true"
    val input = Digest.of(Canon.CStr(source))
    val referenceParse = Parser.parse(Stlc.language.grammar, source).map(Cst.toCanon)
    val compiledParse = IncrementalParser(Stlc.language.grammar).parse(source).map(r => Cst.toCanon(r.out.cst))
    val parserEvidence = SemanticEquivalence.certify(EquivalenceKind.GrammarInterpreter,
      Stlc.language.digest, implementation, input, referenceParse, compiledParse).toOption.get
    assertEquals(SemanticEquivalence.fromArtifact(parserEvidence.artifact), Right(parserEvidence))

    val term = Stlc.app1(Stlc.lam1("x", Stlc.tBool, Stlc.v("x")), Stlc.tru)
    val rewriteEvidence = SemanticEquivalence.certify(EquivalenceKind.RuleInterpreter,
      Stlc.language.digest, implementation, Artifact(ArtifactKind.Term, Cst.toCanon(term)).digest,
      TreeEngine.normalize(Stlc.language, term).map(Cst.toCanon),
      CompiledTreeEngine(Stlc.language).normalize(term).map(Cst.toCanon)).toOption.get
    assertEquals(SemanticEquivalence.fromArtifact(rewriteEvidence.artifact), Right(rewriteEvidence))

  test("every optimized boundary has one canonical evidence vocabulary"):
    val result = Right(Canon.CStr("same"))
    EquivalenceKind.values.foreach { kind =>
      val evidence = SemanticEquivalence.certify(kind, Stlc.language.digest, implementation,
        Digest.of(Canon.CStr(kind.id)), result, result).toOption.get
      assert(evidence.valid)
      assertEquals(evidence.artifact.kind, ArtifactKind.SemanticEquivalence)
    }

  test("disagreement cannot be certified or decoded"):
    assert(SemanticEquivalence.certify(EquivalenceKind.ChangeInterpreter, Stlc.language.digest,
      implementation, Digest.of(Canon.CStr("change")), Right(Canon.CStr("a")), Right(Canon.CStr("b"))).isLeft)
    val forged = SemanticEquivalence(EquivalenceKind.ProofChecker, Stlc.language.digest, implementation,
      Digest.of(Canon.CStr("proof")), Digest.of(Canon.CStr("yes")), Digest.of(Canon.CStr("no")),
      AlternativeSource.Optimized)
    assert(SemanticEquivalence.fromArtifact(forged.artifact).isLeft)
