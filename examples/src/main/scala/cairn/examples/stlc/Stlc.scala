package cairn.examples.stlc

import cairn.kernel.*
import cairn.workbench.*

/** STLC as composable fragment DATA (S16, §5). Everything here is values —
  * sorts, grammar alternatives, rewrite rules, typing rules — interpreted by
  * the generic engines. No STLC-specific code exists in L0–L3.
  *
  * Phase 2: semantic fragments are grammar-free; concrete syntax lives in
  * [[surfaceFragments]] / `languages/stlc/surfaces/default.cairn`.
  *
  * Surface syntax (default):
  *   term  ::= fun x : type . term | if term then term else term
  *           | true | false | (term term) | (term) | x
  *   type  ::= type -> type | Bool | (type)
  */
object Stlc:
  private def n(tag: String, cs: Cst*): Cst = Cst.node(tag, cs*)
  private def mv(x: String): Cst = Cst.Leaf(s"$$$x")

  /** Base: the term category, variables, application (semantics). */
  val base: Fragment = Fragment(
    name = "base",
    provides = List("term"),
    requires = Nil,
    sorts = List(SortDef("Term", SortMode.Tree)),
    constructors = List(
      CtorDef("var", "Term", List("Name")),
      CtorDef("app", "Term", List("Term", "Term"))),
    varCtor = Some("var"))

  val baseSurface: Fragment = Fragment(
    name = "base",
    provides = Nil,
    requires = Nil,
    grammar = GrammarPart(
      puncts = List("(", ")"),
      categories = List(CategorySpec("term", List(
        ConstructorSpec("app", List(Elem.Tok("("), Elem.Cat("term"), Elem.Cat("term"), Elem.Tok(")"))),
        ConstructorSpec("$group", List(Elem.Tok("("), Elem.Cat("term"), Elem.Tok(")"))),
        ConstructorSpec("var", List(Elem.NameLeaf))))),
      top = Some("term")))

  /** Simple types: Bool and the arrow (semantics). */
  val types: Fragment = Fragment(
    name = "types",
    provides = List("type"),
    requires = Nil,
    sorts = List(SortDef("Type", SortMode.Tree)),
    constructors = List(
      CtorDef("tyBool", "Type", Nil),
      CtorDef("arrow", "Type", List("Type", "Type"))))

  val typesSurface: Fragment = Fragment(
    name = "types",
    provides = Nil,
    requires = Nil,
    grammar = GrammarPart(
      keywords = List("Bool"),
      puncts = List("->", "(", ")"),
      categories = List(CategorySpec("typeAtom", List(
        ConstructorSpec("tyBool", List(Elem.Tok("Bool"))),
        ConstructorSpec("$group", List(Elem.Tok("("), Elem.Cat("type"), Elem.Tok(")")))))),
      precCategories = List(PrecCategory("type", "typeAtom", List(
        InfixOp("->", "arrow", 1, rightAssoc = true))))))

  /** Lambda: abstraction + β-reduction; binder declared as data. */
  val lambda: Fragment = Fragment(
    name = "lambda",
    provides = List("lambda"),
    requires = List("term", "type"),
    constructors = List(
      CtorDef("lam", "Term", List("Name", "Type", "Term"), binders = List((0, List(2))))),
    rewriteRules = List(
      RewriteRule("beta",
        pattern = n("app", n("lam", mv("x"), mv("T"), mv("b")), mv("v")),
        template = n("$subst", mv("b"), mv("x"), mv("v")))))

  val lambdaSurface: Fragment = Fragment(
    name = "lambda",
    provides = Nil,
    requires = Nil,
    grammar = GrammarPart(
      keywords = List("fun"),
      puncts = List(":", "."),
      categories = List(CategorySpec("term", List(
        ConstructorSpec("lam", List(
          Elem.Tok("fun"), Elem.NameLeaf, Elem.Tok(":"), Elem.Cat("type"),
          Elem.Tok("."), Elem.Cat("term"))))))))

  /** Booleans: literals + if/then/else + ι-reduction. */
  val booleans: Fragment = Fragment(
    name = "booleans",
    provides = List("booleans"),
    requires = List("term", "type"),
    constructors = List(
      CtorDef("true", "Term", Nil),
      CtorDef("false", "Term", Nil),
      CtorDef("if", "Term", List("Term", "Term", "Term"))),
    rewriteRules = List(
      RewriteRule("if-true", n("if", n("true"), mv("a"), mv("b")), mv("a")),
      RewriteRule("if-false", n("if", n("false"), mv("a"), mv("b")), mv("b"))))

  val booleansSurface: Fragment = Fragment(
    name = "booleans",
    provides = Nil,
    requires = Nil,
    grammar = GrammarPart(
      keywords = List("true", "false", "if", "then", "else"),
      categories = List(CategorySpec("term", List(
        ConstructorSpec("true", List(Elem.Tok("true"))),
        ConstructorSpec("false", List(Elem.Tok("false"))),
        ConstructorSpec("if", List(
          Elem.Tok("if"), Elem.Cat("term"), Elem.Tok("then"), Elem.Cat("term"),
          Elem.Tok("else"), Elem.Cat("term"))))))))

  /** STLC typing rules as declarative judgment data (S22). */
  val typing: Fragment = Fragment(
    name = "typing",
    provides = List("typing"),
    requires = List("term", "type", "lambda", "booleans"),
    judgments = List(
      JudgmentDef("lookup", List(
        InferRule("l-here", Nil,
          n("lookup", n("ctxCons", mv("x"), mv("T"), mv("r")), mv("x"), mv("T"))),
        InferRule("l-there", List(n("lookup", mv("r"), mv("x"), mv("T"))),
          n("lookup", n("ctxCons", mv("y"), mv("S"), mv("r")), mv("x"), mv("T")),
          conditions = List(n("$neq", mv("x"), mv("y")))))),
      JudgmentDef("hasType", List(
        InferRule("t-var", List(n("$ctx-lookup", mv("ctx"), mv("x"), mv("T"))),
          n("hasType", mv("ctx"), n("var", mv("x")), mv("T"))),
        InferRule("t-abs", List(n("hasType", n("ctxCons", mv("x"), mv("T"), mv("ctx")), mv("b"), mv("U"))),
          n("hasType", mv("ctx"), n("lam", mv("x"), mv("T"), mv("b")), n("arrow", mv("T"), mv("U")))),
        InferRule("t-app", List(
            n("hasType", mv("ctx"), mv("f"), n("arrow", mv("T"), mv("U"))),
            n("hasType", mv("ctx"), mv("a"), mv("T"))),
          n("hasType", mv("ctx"), n("app", mv("f"), mv("a")), mv("U"))),
        InferRule("t-true", Nil, n("hasType", mv("ctx"), n("true"), n("tyBool"))),
        InferRule("t-false", Nil, n("hasType", mv("ctx"), n("false"), n("tyBool"))),
        InferRule("t-if", List(
            n("hasType", mv("ctx"), mv("c"), n("tyBool")),
            n("hasType", mv("ctx"), mv("a"), mv("T")),
            n("hasType", mv("ctx"), mv("b"), mv("T"))),
          n("hasType", mv("ctx"), n("if", mv("c"), mv("a"), mv("b")), mv("T")))))))

  val fragments: List[Fragment] = List(base, types, lambda, booleans, typing)
  val surfaceFragments: List[Fragment] =
    List(baseSurface, typesSurface, lambdaSurface, booleansSurface)

  val defaultSurface: SurfacePack =
    SurfacePack(PackLoader.DefaultSurface, "stlc", surfaceFragments)

  /** Semantic fragments with default surface grammar bound (for Compose / tests). */
  def boundFragments: List[Fragment] = PackLoader.bindSurface(fragments, defaultSurface)

  def language: ComposedLanguage =
    Compose.compose("stlc", boundFragments) match
      case Right(l)   => l
      case Left(errs) => throw RuntimeException(errs.map(_.render).mkString("\n"))

  // -- convenient golden terms --
  def v(x: String): Cst = n("var", Cst.Leaf(x))
  def lam1(x: String, t: Cst, b: Cst): Cst = n("lam", Cst.Leaf(x), t, b)
  def app1(f: Cst, a: Cst): Cst = n("app", f, a)
  val tBool: Cst = n("tyBool")
  def arrow1(a: Cst, b: Cst): Cst = n("arrow", a, b)
  val tru: Cst = n("true")
  val fls: Cst = n("false")

  /** Church booleans over Bool: tt = λt.λf.t ; ff = λt.λf.f */
  val churchTrue: Cst = lam1("t", tBool, lam1("f", tBool, v("t")))
  val churchFalse: Cst = lam1("t", tBool, lam1("f", tBool, v("f")))
  /** identity at Bool */
  val idBool: Cst = lam1("x", tBool, v("x"))
  /** if false then true else false */
  val node3if: Cst = n("if", fls, tru, fls)
