package cairn.examples.stlc

import cairn.kernel.*

/** STLC as composable fragment DATA (S16, §5). Everything here is values —
  * sorts, grammar alternatives, rewrite rules, typing rules — interpreted by
  * the generic engines. No STLC-specific code exists in L0–L3.
  *
  * Surface syntax:
  *   term  ::= fun x : type . term | if term then term else term
  *           | true | false | (term term) | (term) | x
  *   type  ::= type -> type | Bool | (type)
  */
object Stlc:
  private def n(tag: String, cs: Cst*): Cst = Cst.node(tag, cs*)
  private def mv(x: String): Cst = Cst.Leaf(s"$$$x")

  /** Base: the term category, variables, application, grouping. */
  val base: Fragment = Fragment(
    name = "base",
    provides = List("term"),
    requires = Nil,
    sorts = List(SortDef("Term", SortMode.Tree)),
    constructors = List(
      CtorDef("var", "Term", List("Name")),
      CtorDef("app", "Term", List("Term", "Term"))),
    varCtor = Some("var"),
    grammar = GrammarPart(
      puncts = List("(", ")"),
      categories = List(CategorySpec("term", List(
        ConstructorSpec("app", List(Elem.Tok("("), Elem.Cat("term"), Elem.Cat("term"), Elem.Tok(")"))),
        ConstructorSpec("$group", List(Elem.Tok("("), Elem.Cat("term"), Elem.Tok(")"))),
        ConstructorSpec("var", List(Elem.NameLeaf))))),
      printRules = List(
        PrintRule("app", List(PrintSeg.Lit("("), PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Lit(")"))),
        PrintRule("var", List(PrintSeg.Field(0)))),
      top = Some("term")))

  /** Simple types: Bool and the arrow (right-associative infix). */
  val types: Fragment = Fragment(
    name = "types",
    provides = List("type"),
    requires = Nil,
    sorts = List(SortDef("Type", SortMode.Tree)),
    constructors = List(
      CtorDef("tyBool", "Type", Nil),
      CtorDef("arrow", "Type", List("Type", "Type"))),
    grammar = GrammarPart(
      keywords = List("Bool"),
      puncts = List("->", "(", ")"),
      categories = List(CategorySpec("typeAtom", List(
        ConstructorSpec("tyBool", List(Elem.Tok("Bool"))),
        ConstructorSpec("$group", List(Elem.Tok("("), Elem.Cat("type"), Elem.Tok(")")))))),
      precCategories = List(PrecCategory("type", "typeAtom", List(
        InfixOp("->", "arrow", 1, rightAssoc = true)))),
      printRules = List(
        PrintRule("tyBool", List(PrintSeg.Lit("Bool"))))))

  /** Lambda: abstraction + β-reduction; binder declared as data. */
  val lambda: Fragment = Fragment(
    name = "lambda",
    provides = List("lambda"),
    requires = List("term", "type"),
    constructors = List(
      CtorDef("lam", "Term", List("Name", "Type", "Term"), binders = List((0, List(2))))),
    grammar = GrammarPart(
      keywords = List("fun"),
      puncts = List(":", "."),
      categories = List(CategorySpec("term", List(
        ConstructorSpec("lam", List(
          Elem.Tok("fun"), Elem.NameLeaf, Elem.Tok(":"), Elem.Cat("type"),
          Elem.Tok("."), Elem.Cat("term")))))),
      printRules = List(
        PrintRule("lam", List(
          PrintSeg.Lit("fun"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
          PrintSeg.Lit(":"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space,
          PrintSeg.Lit("."), PrintSeg.Space, PrintSeg.Field(2))))),
    rewriteRules = List(
      RewriteRule("beta",
        pattern = n("app", n("lam", mv("x"), mv("T"), mv("b")), mv("v")),
        template = n("$subst", mv("b"), mv("x"), mv("v")))))

  /** Booleans: literals + if/then/else + ι-reduction. */
  val booleans: Fragment = Fragment(
    name = "booleans",
    provides = List("booleans"),
    requires = List("term", "type"),
    constructors = List(
      CtorDef("true", "Term", Nil),
      CtorDef("false", "Term", Nil),
      CtorDef("if", "Term", List("Term", "Term", "Term"))),
    grammar = GrammarPart(
      keywords = List("true", "false", "if", "then", "else"),
      categories = List(CategorySpec("term", List(
        ConstructorSpec("true", List(Elem.Tok("true"))),
        ConstructorSpec("false", List(Elem.Tok("false"))),
        ConstructorSpec("if", List(
          Elem.Tok("if"), Elem.Cat("term"), Elem.Tok("then"), Elem.Cat("term"),
          Elem.Tok("else"), Elem.Cat("term")))))),
      printRules = List(
        PrintRule("true", List(PrintSeg.Lit("true"))),
        PrintRule("false", List(PrintSeg.Lit("false"))),
        PrintRule("if", List(
          PrintSeg.Lit("if"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
          PrintSeg.Lit("then"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space,
          PrintSeg.Lit("else"), PrintSeg.Space, PrintSeg.Field(2))))),
    rewriteRules = List(
      RewriteRule("if-true", n("if", n("true"), mv("a"), mv("b")), mv("a")),
      RewriteRule("if-false", n("if", n("false"), mv("a"), mv("b")), mv("b"))))

  /** STLC typing rules as declarative judgment data (S22).
    * Judgment forms (as Cst): hasType(ctx, term, type); lookup(ctx, x, type).
    * Contexts: ctxNil | ctxCons(name, type, rest).
    */
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
          // M19: without this side condition a shadowed binding could be typed
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

  def language: ComposedLanguage =
    Compose.compose("stlc", fragments) match
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
