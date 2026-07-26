package cairn.tests

import cairn.kernel.*
import cairn.core.*

/** Unit tests for [[ModuleStructural.Spec.canon]] — the canonical
  * descriptor [[ModuleGate.fromSpecs]] digests into
  * [[cairn.core.AcceptancePolicy]]'s own identity.
  */
class ModuleStructuralSuite extends munit.FunSuite:
  import ModuleStructural.Spec

  private val langA = Digest.of(Canon.CStr("lang-a"))
  private val langB = Digest.of(Canon.CStr("lang-b"))

  /** A minimal real language declaring one judgment, `myJudgment`, over a
    * single-string goal — enough to prove/reject via `Search.prove` without
    * pulling in a whole exemplar pack.
    */
  private val judgmentLang: ComposedLanguage =
    val jd = JudgmentDef("myJudgment", List(InferRule("ok-rule", Nil, Cst.node("myJudgment", Cst.Leaf("ok")), Nil)))
    val frag = Fragment(name = "judgtest", provides = List("judgtest"), requires = Nil, judgments = List(jd))
    Compose.compose("judgtest", List(frag)).fold(e => fail(e.map(_.render).mkString), identity)

  private val leafModule = Module(List("a" -> Cst.node("leaf", Cst.Leaf("ok"))))
  private def leafOkSpec(lang: Digest) = Spec.LeafOk("leaf", 0, JudgmentRef(lang, "myJudgment"))

  test("run/check: resolver returning None for the referenced provider fails BEFORE Search.prove"):
    val errs = ModuleStructural.run(leafModule, List(leafOkSpec(judgmentLang.digest)), _ => None)
    assert(errs.exists(_.contains("unknown judgment provider")), errs.toString)

  test("run/check: resolver returning a language whose OWN digest differs from the requested key is rejected, not trusted"):
    // A misbehaving/mis-keyed resolver: asked for `judgmentLang.digest`, hands
    // back a language whose digest is actually something else entirely.
    val wrongDigest = Digest.of(Canon.CStr("not-judgtest"))
    val errs = ModuleStructural.run(leafModule, List(leafOkSpec(wrongDigest)), _ => Some(judgmentLang))
    assert(errs.exists(_.contains("provider digest mismatch")), errs.toString)

  test("run/check: a resolved language that doesn't declare the referenced judgment name fails before Search.prove"):
    val bareFrag = Fragment(name = "bare", provides = List("bare"), requires = Nil)
    val bareLang = Compose.compose("bare", List(bareFrag)).fold(e => fail(e.map(_.render).mkString), identity)
    val errs = ModuleStructural.run(leafModule, List(leafOkSpec(bareLang.digest)), _ => Some(bareLang))
    assert(errs.exists(_.contains("does not declare judgment")), errs.toString)

  test("run/check: correct provider digest + declared judgment succeeds"):
    val errs = ModuleStructural.run(leafModule, List(leafOkSpec(judgmentLang.digest)), d => Option.when(d == judgmentLang.digest)(judgmentLang))
    assertEquals(errs, Nil)

  test("Spec.canon is deterministic for identical specs"):
    val a = Spec.DefinedRef("foo", 0, "foo")
    val b = Spec.DefinedRef("foo", 0, "foo")
    assertEquals(a.canon, b.canon)

  test("Spec.canon distinguishes specs differing only in one field"):
    assertNotEquals(
      Spec.DefinedRef("foo", 0, "foo").canon,
      Spec.DefinedRef("foo", 1, "foo").canon)
    assertNotEquals(
      Spec.SumLeavesAtMost("mixture", List(1), 100, "mixture").canon,
      Spec.SumLeavesAtMost("mixture", List(1), 99, "mixture").canon)

  test("Spec.canon distinguishes different spec kinds even with overlapping field values"):
    assertNotEquals(
      Spec.DefinedRef("foo", 0, "foo").canon,
      Spec.DefinedLeafList("foo", 0, "foo").canon)

  test("Spec.canon: Set-valued fields are order-independent"):
    val a = Spec.RefTagIn("ctor", 0, Set("a", "b", "c"), "label")
    val b = Spec.RefTagIn("ctor", 0, Set("c", "b", "a"), "label")
    assertEquals(a.canon, b.canon)

  test("Spec.canon: LeafOk is deterministic and distinguishes different judgments (name or provider)"):
    assertEquals(Spec.LeafOk("ctor", 0, JudgmentRef(langA, "j1")).canon, Spec.LeafOk("ctor", 0, JudgmentRef(langA, "j1")).canon)
    assertNotEquals(Spec.LeafOk("ctor", 0, JudgmentRef(langA, "j1")).canon, Spec.LeafOk("ctor", 0, JudgmentRef(langA, "j2")).canon)
    assertNotEquals(Spec.LeafOk("ctor", 0, JudgmentRef(langA, "j1")).canon, Spec.LeafOk("ctor", 0, JudgmentRef(langB, "j1")).canon,
      "same judgment name under a different provider language must change the descriptor")

  test("Spec.canon: OutlineNums is deterministic and distinguishes different judgments / number sources"):
    import ModuleStructural.NumberSource
    val base = Spec.OutlineNums("ctor", 0, List(NumberSource.FromLeaf("t", 0)), JudgmentRef(langA, "j1"), "label")
    assertEquals(base.canon, Spec.OutlineNums("ctor", 0, List(NumberSource.FromLeaf("t", 0)), JudgmentRef(langA, "j1"), "label").canon)
    assertNotEquals(base.canon,
      Spec.OutlineNums("ctor", 0, List(NumberSource.FromLeaf("t", 0)), JudgmentRef(langA, "j2"), "label").canon,
      "different judgment name must change the descriptor")
    assertNotEquals(base.canon,
      Spec.OutlineNums("ctor", 0, List(NumberSource.FromLeaf("t", 0)), JudgmentRef(langB, "j1"), "label").canon,
      "same judgment name under a different provider language must change the descriptor")
    assertNotEquals(base.canon,
      Spec.OutlineNums("ctor", 0, List(NumberSource.ByTag(Map("t" -> 1))), JudgmentRef(langA, "j1"), "label").canon,
      "different numberSources must change the descriptor")

  test("Spec.canon: a whole spec list's digest matches ModuleGate.fromSpecs's descriptor"):
    val specs = List(
      Spec.DefinedRef("foo", 0, "foo"),
      Spec.NonEmptyLeaves("bar", List(1, 2), List("a", "b")))
    val gate = ModuleGate.fromSpecs("j", specs)(_ => Right(()))
    assertEquals(gate.descriptor, Some(Digest.of(Canon.CList(specs.map(_.canon)))))

  test("NumberSource: canon round-trip, both cases"):
    import ModuleStructural.NumberSource
    assertEquals(NumberSource.fromCanon(NumberSource.FromLeaf("t", 2).canon), NumberSource.FromLeaf("t", 2))
    assertEquals(NumberSource.fromCanon(NumberSource.ByTag(Map("a" -> 1, "b" -> 2)).canon), NumberSource.ByTag(Map("a" -> 1, "b" -> 2)))

  test("Spec: canon round-trip (fromCanon(x.canon) == x) for every case"):
    import ModuleStructural.NumberSource
    val specs = List(
      Spec.SumLeavesAtMost("mixture", List(1), 100, "mixture"),
      Spec.UniqueTuples("ctor", List(List(0), List(1)), "label"),
      Spec.NonEmptyLeaves("ctor", List(1, 2), List("a", "b")),
      Spec.OutlineNums("outline", 2, List(NumberSource.FromLeaf("euSection", 0), NumberSource.ByTag(Map("s1" -> 1))), JudgmentRef(langA, "sectionNumberOk"), "outline"),
      Spec.DefinedRef("product", 1, "product"),
      Spec.DefinedRefs("shadow", List(0, 1), "shadow"),
      Spec.DefinedLeafList("product", 2, "product"),
      Spec.DefinedNodeListRefs("mixture", 0, List(0), "mixture"),
      Spec.LeafOk("euSection", 0, JudgmentRef(langA, "sectionNumberOk")),
      Spec.LeafValueInCtorField("translationState", 0, Set("phrase", "corpusPhrase"), 0, "translationState"),
      Spec.RefTagIn("sectionFieldShadow", 0, Set("euSection", "identificationSection"), "sectionFieldShadow"),
      Spec.UniqueTuplesInList("euSection", 1, List(List(0), List(1)), "euSection", Some(Set("sectionField", "sectionFieldRef"))),
      Spec.UniqueTuplesInList("euSection", 1, List(List(0)), "euSection", None),
      Spec.ListChildDefinedRefs("euSection", 1, Map("sectionFieldRef" -> List(List(2))), "euSection"),
      Spec.KeyedLocaleOverlay("identificationSection", 6, Set("synonyms", "recommendedUse"),
        "fieldLocale", "fieldLocaleRef", 0, 1, 2, "identificationSection"),
    )
    for s <- specs do assertEquals(Spec.fromCanon(s.canon), s, s"round-trip failed for $s")
