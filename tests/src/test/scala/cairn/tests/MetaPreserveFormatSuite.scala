package cairn.tests

import cairn.kernel.*
import cairn.workbench.*

/** Format-preserving meta-surface regeneration: the follow-up to a real CI
  * incident where hand-editing `languages/search.cairn` (real declarative
  * judgments replacing stub rules) added a comment that `emit-languages`'
  * full reprint silently stripped, since comments are pure lexer trivia
  * never captured in the parsed `Fragment` at all. This doesn't change what's
  * canonical (still comment-free, still digest-stable) — it changes HOW
  * regeneration reaches that form: splicing original bytes for declarations
  * whose content didn't change, reprinting only the ones that did.
  *
  * Two variants, for two different "what changed" comparisons:
  *   - [[Meta.printLanguagePreservingFormat]]: Fragment (independent Scala
  *     source, e.g. `Stlc.fragments`) vs. whatever text is currently on disk.
  *   - [[Meta.printLanguagePreservingFormatVsReference]]: working-tree text
  *     vs. a separate reference text (e.g. git HEAD) — for languages with NO
  *     independent source (pki/law/sds/search), where comparing the file
  *     against itself would always be a no-op.
  */
class MetaPreserveFormatSuite extends munit.FunSuite:
  private val name = "search"
  private val fragments = cairn.examples.search.Search.fragments
  private val canonical = Meta.printLanguage(name, fragments).fold(e => fail(e), identity)

  // Stable, currently-emitted declaration text used as a comment-anchor site.
  // (Older Search surface had `keyword origin;` / `top searchObj;` — those
  // forms are gone after the Phase 2 surface split; tests must target real
  // printed decls or the replace is a silent no-op.)
  private val untouchedDecl = "    ctor origin : Fact(Text);"

  test("no-op: fragments unchanged from currentText — output is byte-identical, comment and all"):
    val withComment = canonical.replace(
      untouchedDecl, "    -- kept comment on an untouched declaration\n" + untouchedDecl)
    assert(withComment != canonical, "anchor must exist in canonical print")
    val result = Meta.printLanguagePreservingFormat(name, fragments, withComment).fold(e => fail(e), identity)
    assertEquals(result, withComment)

  test("a declaration whose content changed is reprinted canonically (drops its own comment); untouched ones keep theirs"):
    // Replay the actual incident: an OLDER text with stub judgments (+ a stale
    // comment attached to them) and an UNRELATED comment elsewhere that must survive.
    val stubWF = JudgmentDef("wellFormed", List(InferRule("wf-any", Nil, Cst.node("wellFormed", Cst.Leaf("$x")), Nil)))
    val stubGM = JudgmentDef("goalMet", List(InferRule("gm-any", Nil, Cst.node("goalMet", Cst.Leaf("$x")), Nil)))
    val oldFragment = fragments.head.copy(judgments = fragments.head.judgments.map {
      case j if j.name == "wellFormed" => stubWF
      case j if j.name == "goalMet"    => stubGM
      case j                           => j
    })
    val oldText = Meta.printLanguage(name, List(oldFragment)).fold(e => fail(e), identity)
    val handEdited = oldText
      .replace(untouchedDecl, "    -- unrelated, must survive regeneration\n" + untouchedDecl)
      .replace("    judgment wellFormed {",
        "    -- stale comment about the OLD stub rule, must NOT survive\n    judgment wellFormed {")
    assert(handEdited.contains("-- unrelated, must survive regeneration"),
      "anchor must exist in oldText print")
    val result = Meta.printLanguagePreservingFormat(name, fragments, handEdited).fold(e => fail(e), identity)
    assert(result.contains("-- unrelated, must survive regeneration"), result)
    assert(!result.contains("-- stale comment about the OLD stub rule"), result)
    assert(result.contains("wf-origin"), result) // the real rules, canonically reprinted
    assert(!result.contains("wf-any"), result)

    // and it round-trips to the intended (new) fragments, not the old stub ones
    val (_, back) = Meta.parseLanguageAst(result).fold(e => fail(e), identity)
    assertEquals(back.map(_.digest), fragments.map(_.digest))

  test("fallback: a fragment whose declaration COUNT changed reprints fully (no crash, still correct)"):
    val fewerCtorsFragment = fragments.head.copy(constructors = fragments.head.constructors.init)
    val oldText = Meta.printLanguage(name, List(fewerCtorsFragment)).fold(e => fail(e), identity)
    val result = Meta.printLanguagePreservingFormat(name, fragments, oldText).fold(e => fail(e), identity)
    assertEquals(result, canonical)

  test("fallback: unparseable currentText reprints fully rather than failing"):
    val result = Meta.printLanguagePreservingFormat(name, fragments, "not a cairn file at all {{{").fold(e => fail(e), identity)
    assertEquals(result, canonical)

  test("fallback: multi-fragment languages aren't attempted (scoped to the single-fragment exemplar shape)"):
    val dl = Delta.deltaOf(cairn.examples.stlc.Stlc.language).fold(e => fail(e.map(_.render).mkString), identity)
    // Stlc itself has multiple fragments — printLanguagePreservingFormat must not
    // misapply single-fragment splicing to it, just defer to the full reprint.
    val text = Meta.printLanguage("stlc", cairn.examples.stlc.Stlc.fragments).fold(e => fail(e), identity)
    val result = Meta.printLanguagePreservingFormat("stlc", cairn.examples.stlc.Stlc.fragments, text)
      .fold(e => fail(e), identity)
    assertEquals(result, text)

  // ---- printLanguagePreservingFormatVsReference (working tree vs. e.g. git HEAD) ----

  test("vsReference no-op: working text unchanged from reference — output is byte-identical, comment and all"):
    val withComment = canonical.replace(
      untouchedDecl, "    -- kept comment on an untouched declaration\n" + untouchedDecl)
    assert(withComment != canonical, "anchor must exist in canonical print")
    val result = Meta.printLanguagePreservingFormatVsReference(name, withComment, canonical)
      .fold(e => fail(e), identity)
    assertEquals(result, withComment)

  test("vsReference: a declaration changed since reference is reprinted canonically; untouched ones (with a NEW comment) keep it"):
    // This is the exact real-world shape: `reference` = git HEAD's last-committed,
    // already-CI-validated text (stub judgments); `working` = the file as just
    // hand-edited on disk (real judgments + a brand-new comment elsewhere).
    val stubWF = JudgmentDef("wellFormed", List(InferRule("wf-any", Nil, Cst.node("wellFormed", Cst.Leaf("$x")), Nil)))
    val stubGM = JudgmentDef("goalMet", List(InferRule("gm-any", Nil, Cst.node("goalMet", Cst.Leaf("$x")), Nil)))
    val oldFragment = fragments.head.copy(judgments = fragments.head.judgments.map {
      case j if j.name == "wellFormed" => stubWF
      case j if j.name == "goalMet"    => stubGM
      case j                           => j
    })
    val reference = Meta.printLanguage(name, List(oldFragment)).fold(e => fail(e), identity)
    val working = canonical.replace(
      untouchedDecl, "    -- freshly added while editing, must survive\n" + untouchedDecl)
    assert(working.contains("-- freshly added while editing, must survive"),
      "anchor must exist in canonical print")
    val result = Meta.printLanguagePreservingFormatVsReference(name, working, reference).fold(e => fail(e), identity)
    assert(result.contains("-- freshly added while editing, must survive"), result)
    assert(result.contains("wf-origin"), result) // the real rules, canonically reprinted
    assert(!result.contains("wf-any"), result)
    val (_, back) = Meta.parseLanguageAst(result).fold(e => fail(e), identity)
    assertEquals(back.map(_.digest), fragments.map(_.digest))

  test("vsReference fallback: declaration count changed since reference reprints fully"):
    val fewerCtorsFragment = fragments.head.copy(constructors = fragments.head.constructors.init)
    val reference = Meta.printLanguage(name, List(fewerCtorsFragment)).fold(e => fail(e), identity)
    val result = Meta.printLanguagePreservingFormatVsReference(name, canonical, reference).fold(e => fail(e), identity)
    assertEquals(result, canonical)

  test("vsReference fallback: unparseable reference reprints working text fully rather than failing"):
    val result = Meta.printLanguagePreservingFormatVsReference(name, canonical, "not a cairn file {{{")
      .fold(e => fail(e), identity)
    assertEquals(result, canonical)
