package cairn.core

import cairn.kernel.*

/** The four host ports (M30–M34). Every port emits by BUILDING a file Cst and
  * printing it through its file grammar — so the whole-file byte fixpoint
  * (PortV2.verified) is checked against the very grammar that defines the
  * file's shape. Signatures are fully grammatical; expression bodies are
  * single-line verbatim regions captured by rest-of-line markers.
  */
object Ports2:
  private def leaf(s: String): Cst = Cst.Leaf(s)
  private def some(items: List[Cst]): Cst =
    if items.isEmpty then Cst.node("none") else Cst.node("some", Cst.Node("list", items))
  private def lst(items: List[Cst]): Cst = Cst.Node("list", items)

  /** `isSorted`'s decision procedure, expressed ONCE in the host-neutral expr
    * vocabulary (`rmatch`/`rle`/`rand`/`rtrue`) and rendered per host via each
    * port's own `body()` — replacing three independently hand-written native
    * strings (Scala/Haskell/Rust each used to hardcode their own version) with
    * one shared source of truth. `isPerm` stays per-host: expressing
    * permutation equivalence host-neutrally would need a fold/reduce
    * primitive this vocabulary doesn't have, not worth adding for one relation.
    */
  private val isSortedBody: Cst =
    // Tail-binder reuses the name "xs" at both nesting levels (rather than
    // fresh t/t2 names): Rust's call-arg renderer only skips its `&`-wrapping
    // heuristic for an argument literally named "xs", so recursing on a
    // differently-named tail variable would double-borrow an already-borrowed
    // slice. Shadowing "xs" in each nested match arm is legal in all four
    // hosts and keeps the recursive call exactly `isSorted(xs)` everywhere.
    Cst.node("rmatch", Cst.node("rvar", leaf("xs")), Cst.node("rtrue"), leaf("h"), leaf("xs"),
      Cst.node("rmatch", Cst.node("rvar", leaf("xs")), Cst.node("rtrue"), leaf("h2"), leaf("xs"),
        Cst.node("rand",
          Cst.node("rle", Cst.node("rvar", leaf("h")), Cst.node("rvar", leaf("h2"))),
          Cst.node("rcall", leaf("isSorted"), Cst.node("rvar", leaf("xs"))))))

  // =========================== SCALA ===========================

  object ScalaPort2 extends PortV2:
    val hostName = "scala"

    private def ty(t: RTy): Cst = t match
      case RTy.RInt      => Cst.node("tyInt")
      case RTy.RBool     => Cst.node("tyBool")
      case RTy.RUnit     => Cst.node("tyUnit")
      case RTy.RVar(n)   => Cst.node("tyVar", leaf(n))
      case RTy.RList(of) => Cst.node("tyList", ty(of))

    val fileGrammar: GrammarSpec = GrammarSpec(
      name = "port-scala-file",
      tokens = TokenSpec(
        keywords = List("object", "def", "class", "enum", "using", "Ordering",
          "List", "Int", "Boolean", "Unit", "main"),
        puncts = List("[", "]", "(", ")", ":", ",", "@"),
        lineComment = None,
        restOfLineMarkers = List("//!", "= ", "{ ")),
      categories = List(
        CategorySpec("file", List(
          ConstructorSpec("file", List(Elem.RestOfLine, Elem.Tok("object"), Elem.NameLeaf,
            Elem.Tok(":"), Elem.Star(Elem.Cat("decl")), Elem.Cat("mainDecl"))))),
        CategorySpec("mainDecl", List(
          ConstructorSpec("mainD", List(
            Elem.Tok("@"), Elem.Tok("main"), Elem.Tok("def"), Elem.NameLeaf,
            Elem.Tok("("), Elem.Tok(")"), Elem.Tok(":"), Elem.Tok("Unit"), Elem.RestOfLine)))),
        CategorySpec("decl", List(
          ConstructorSpec("classD", List(Elem.Tok("class"), Elem.NameLeaf, Elem.RestOfLine)),
          ConstructorSpec("enumD", List(Elem.Tok("enum"), Elem.NameLeaf, Elem.RestOfLine)),
          ConstructorSpec("defD", List(
            Elem.Tok("def"), Elem.NameLeaf,
            Elem.Opt(Elem.Cat("tparams")),
            Elem.Tok("("), Elem.Opt(Elem.SepBy1(Elem.Cat("param"), ",")), Elem.Tok(")"),
            Elem.Opt(Elem.Cat("usingClause")),
            Elem.Tok(":"), Elem.Cat("type"), Elem.RestOfLine)))),
        CategorySpec("tparams", List(
          ConstructorSpec("tparams", List(Elem.Tok("["), Elem.SepBy1(Elem.NameLeaf, ","), Elem.Tok("]"))))),
        CategorySpec("usingClause", List(
          ConstructorSpec("usingC", List(
            Elem.Tok("("), Elem.Tok("using"), Elem.Tok("Ordering"), Elem.Tok("["),
            Elem.NameLeaf, Elem.Tok("]"), Elem.Tok(")"))))),
        CategorySpec("param", List(
          ConstructorSpec("param", List(Elem.NameLeaf, Elem.Tok(":"), Elem.Cat("type"))))),
        CategorySpec("type", List(
          ConstructorSpec("tyList", List(Elem.Tok("List"), Elem.Tok("["), Elem.Cat("type"), Elem.Tok("]"))),
          ConstructorSpec("tyInt", List(Elem.Tok("Int"))),
          ConstructorSpec("tyBool", List(Elem.Tok("Boolean"))),
          ConstructorSpec("tyUnit", List(Elem.Tok("Unit"))),
          ConstructorSpec("tyVar", List(Elem.NameLeaf))))),
      precCategories = Nil,
      printRules = List(
        PrintRule("file", List(
          PrintSeg.Lit("//!"), PrintSeg.Field(0), PrintSeg.Newline,
          PrintSeg.Lit("object"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Lit(":"), PrintSeg.Newline,
          PrintSeg.IndentIn, PrintSeg.SepFields(2, "\n"), PrintSeg.Newline,
          PrintSeg.IndentOut, PrintSeg.Field(3))),
        PrintRule("mainD", List(
          PrintSeg.Lit("@main def"), PrintSeg.Space, PrintSeg.Field(0),
          PrintSeg.Lit("(): Unit = "), PrintSeg.Field(1))),
        PrintRule("classD", List(
          PrintSeg.Lit("class"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("{ "), PrintSeg.Field(1))),
        PrintRule("enumD", List(
          PrintSeg.Lit("enum"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("{ "), PrintSeg.Field(1))),
        PrintRule("defD", List(
          PrintSeg.Lit("def"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Field(1),
          PrintSeg.Lit("("), PrintSeg.Field(2), PrintSeg.Lit(")"), PrintSeg.Field(3),
          PrintSeg.Lit(":"), PrintSeg.Space, PrintSeg.Field(4), PrintSeg.Space, PrintSeg.Lit("= "), PrintSeg.Field(5))),
        PrintRule("tparams", List(PrintSeg.Lit("["), PrintSeg.SepFields(0, ", "), PrintSeg.Lit("]"))),
        PrintRule("usingC", List(PrintSeg.Lit("(using Ordering["), PrintSeg.Field(0), PrintSeg.Lit("])"))),
        PrintRule("param", List(PrintSeg.Field(0), PrintSeg.Lit(":"), PrintSeg.Space, PrintSeg.Field(1))),
        PrintRule("tyList", List(PrintSeg.Lit("List["), PrintSeg.Field(0), PrintSeg.Lit("]"))),
        PrintRule("tyInt", List(PrintSeg.Lit("Int"))),
        PrintRule("tyBool", List(PrintSeg.Lit("Boolean"))),
        PrintRule("tyUnit", List(PrintSeg.Lit("Unit"))),
        PrintRule("tyVar", List(PrintSeg.Field(0)))),
      top = "file")

    /** Host-neutral expr -> single-line Scala text. */
    def body(e: Cst, effectCtx: Boolean): String = ExprUtil.foldE[String](e)(
      varF = identity,
      numF = identity,
      nilF = "List()",
      callF = (f, as) =>
        if effectCtx && f == "tick" && as.isEmpty then "c.tick()"
        else s"$f(${as.mkString(", ")})",
      ifF = (c, a, b) => s"(if $c then $a else $b)",
      matchF = (s, nilB, h, t, consB) => s"($s match { case Nil => $nilB; case $h :: $t => $consB })",
      seqF = (a, b) => s"{ $a; $b }",
      trueF = "true", falseF = "false",
      eqF = (a, b) => s"($a == $b)", leF = (a, b) => s"($a <= $b)", andF = (a, b) => s"($a && $b)")

    def emit(m: RosettaModule2): Either[String, PortOutput] =
      try
        val cap = m.name.capitalize
        val prelude: List[Cst] = List(
          defd("hd", List("a"), Nil, List("xs" -> RTy.RList(RTy.RVar("a"))), RTy.RVar("a"), "xs.head"),
          defd("tl", List("a"), Nil, List("xs" -> RTy.RList(RTy.RVar("a"))), RTy.RList(RTy.RVar("a")), "xs.tail"),
          defd("append", List("a"), Nil, List("x" -> RTy.RList(RTy.RVar("a")), "y" -> RTy.RList(RTy.RVar("a"))), RTy.RList(RTy.RVar("a")), "x ++ y"),
          defd("cons", List("a"), Nil, List("h" -> RTy.RVar("a"), "t" -> RTy.RList(RTy.RVar("a"))), RTy.RList(RTy.RVar("a")), "h :: t"),
          defd("isEmpty", List("a"), Nil, List("xs" -> RTy.RList(RTy.RVar("a"))), RTy.RBool, "xs.isEmpty"),
          defd("filterLt", List("a"), List("a" -> "Ord"), List("p" -> RTy.RVar("a"), "xs" -> RTy.RList(RTy.RVar("a"))), RTy.RList(RTy.RVar("a")), "xs.filter(x => summon[Ordering[a]].lt(x, p))"),
          defd("filterGe", List("a"), List("a" -> "Ord"), List("p" -> RTy.RVar("a"), "xs" -> RTy.RList(RTy.RVar("a"))), RTy.RList(RTy.RVar("a")), "xs.filter(x => !summon[Ordering[a]].lt(x, p))"),
          defd("isSorted", Nil, Nil, List("xs" -> RTy.RList(RTy.RInt)), RTy.RBool, body(isSortedBody, false)),
          defd("isPerm", Nil, Nil, List("x" -> RTy.RList(RTy.RInt), "y" -> RTy.RList(RTy.RInt)), RTy.RBool, "x.sorted == y.sorted"))
        val effectDecls: List[Cst] = m.effects.flatMap { e =>
          List(Cst.node("classD", leaf(e.name.capitalize),
            leaf(s"var n: Int = 0; ${e.ops.map(op => s"def $op(): Unit = n = n + 1").mkString("; ")} }")))
        }
        val dataDecls: List[Cst] = m.datas.map { d =>
          Cst.node("enumD", leaf(d.name),
            leaf(s"case ${d.ctors.map(_._1).mkString(", ")} }"))
        }
        val userDefs: List[Cst] = m.defs.map { d =>
          val eff = d.effect.isDefined
          val extraParams = if eff then List(("c", d.effect.get.capitalize)) else Nil
          val tpsCst =
            if d.typeParams.isEmpty then Cst.node("none")
            else Cst.node("some", Cst.node("tparams", lst(d.typeParams.map(leaf))))
          val allParams =
            extraParams.map((n, t) => Cst.node("param", leaf(n), Cst.node("tyVar", leaf(t)))) ++
            d.params.map((n, t) => Cst.node("param", leaf(n), ty(t)))
          val paramsCst = if allParams.isEmpty then Cst.node("none") else Cst.node("some", lst(allParams))
          val usingCst =
            if d.constraints.isEmpty then Cst.node("none")
            else Cst.node("some", Cst.node("usingC", leaf(d.constraints.head._1)))
          Cst.node("defD", leaf(d.name), tpsCst, paramsCst, usingCst, ty(d.ret), leaf(body(d.body, eff)))
        }
        val samples = """List(List(), List(1), List(3, 1, 2), List(5, 4, 3, 2, 1), List(2, 2, 1, 1))"""
        val checks = m.theorems.map { t =>
          def prop(s: Cst): String = s match
            case Cst.Node("rforall", List(Cst.Leaf(_), b)) => prop(b)
            case Cst.Node("rsorted", List(e)) => s"isSorted(${expr(e)})"
            case Cst.Node("rperm", List(a, b)) => s"isPerm(${expr(a)}, ${expr(b)})"
            case other => throw CodecError(s"bad statement: ${other.render}")
          def expr(e: Cst): String = ExprUtil.foldE[String](e)(identity, identity, "List()",
            (f, as) => s"$f(${as.mkString(", ")})", (c, a, b) => s"(if $c then $a else $b)",
            (_, _, _, _, _) => throw CodecError("match in statement"), (_, _) => throw CodecError("seq in statement"),
            "true", "false", (a, b) => s"($a == $b)", (a, b) => s"($a <= $b)", (a, b) => s"($a && $b)")
          s"""assert(${prop(t.statement)}, "${t.name}")"""
        }
        val effectCheck = m.defs.find(_.effect.isDefined).map { d =>
          val cls = d.effect.get.capitalize
          s"""val c = $cap.$cls(); assert($cap.${d.name}(c, List(3, 1, 2)) == List(1, 2, 3) && c.n > 0, "effect ${d.effect.get}")"""
        }
        val checkAll = Cst.node("defD", leaf("checkAll"), Cst.node("none"),
          Cst.node("none"), Cst.node("none"), Cst.node("tyUnit"),
          leaf(s"$samples.foreach(xs => { ${checks.mkString("; ")} })"))
        val mainD = Cst.node("mainD", leaf("runTests"),
          leaf(s"""{ $cap.checkAll(); ${effectCheck.fold("")(_ + "; ")}println("ALL TESTS PASS") }"""))
        val fileCst = Cst.node("file",
          leaf(s" generated by cairn rosetta scala port — artifact ${m.artifact.digest.short}"),
          leaf(cap),
          lst(effectDecls ++ dataDecls ++ prelude ++ userDefs ++ List(checkAll)),
          mainD)
        Printer.print(fileGrammar, fileCst).map(text =>
          PortOutput(hostName, s"${m.name}.scala", text, userDefs.map(_ => "")))
      catch case e: CodecError => Left(e.msg)

    private def defd(name: String, tps: List[String], constraints: List[(String, String)],
                     params: List[(String, RTy)], ret: RTy, bodyText: String): Cst =
      Cst.node("defD", leaf(name),
        if tps.isEmpty then Cst.node("none") else Cst.node("some", Cst.node("tparams", lst(tps.map(leaf)))),
        if params.isEmpty then Cst.node("none") else Cst.node("some", lst(params.map((n, t) => Cst.node("param", leaf(n), ty(t))))),
        if constraints.isEmpty then Cst.node("none") else Cst.node("some", Cst.node("usingC", leaf(constraints.head._1))),
        ty(ret), leaf(bodyText))

  // =========================== LEAN ===========================

  object LeanPort2 extends PortV2:
    val hostName = "lean"

    private def ty(t: RTy): Cst = t match
      case RTy.RInt      => Cst.node("tyInt")
      case RTy.RBool     => Cst.node("tyBool")
      case RTy.RUnit     => Cst.node("tyUnit")
      case RTy.RVar(n)   => Cst.node("tyVar", leaf(n))
      case RTy.RList(of) => Cst.node("tyList", ty(of))

    val fileGrammar: GrammarSpec = GrammarSpec(
      name = "port-lean-file",
      tokens = TokenSpec(
        keywords = List("namespace", "end", "partial", "def", "theorem", "inductive",
          "Type", "Ord", "List", "Int", "Bool", "Unit", "Prop", "Nat", "StateM",
          "listSorted", "listPerm"),
        puncts = List("(", ")", "{", "}", "[", "]", ":", ",", "∀"),
        lineComment = None,
        restOfLineMarkers = List("--!", ":=", "where")),
      categories = List(
        CategorySpec("file", List(
          ConstructorSpec("file", List(Elem.RestOfLine, Elem.Tok("namespace"), Elem.NameLeaf,
            Elem.Star(Elem.Cat("decl")), Elem.Tok("end"), Elem.NameLeaf)))),
        CategorySpec("decl", List(
          ConstructorSpec("indD", List(Elem.Tok("inductive"), Elem.NameLeaf, Elem.RestOfLine)),
          ConstructorSpec("defD", List(
            Elem.Tok("partial"), Elem.Tok("def"), Elem.AnyIdentLeaf, Elem.Star(Elem.Cat("param")),
            Elem.Tok(":"), Elem.Cat("type"), Elem.RestOfLine)),
          ConstructorSpec("pureDefD", List(
            Elem.Tok("def"), Elem.AnyIdentLeaf, Elem.Star(Elem.Cat("param")),
            Elem.Tok(":"), Elem.Cat("type"), Elem.RestOfLine)),
          ConstructorSpec("thmD", List(
            Elem.Tok("theorem"), Elem.AnyIdentLeaf, Elem.Tok(":"), Elem.Cat("prop"), Elem.RestOfLine)))),
        CategorySpec("param", List(
          ConstructorSpec("pImplicit", List(
            Elem.Tok("{"), Elem.NameLeaf, Elem.Tok(":"), Elem.Tok("Type"), Elem.Tok("}"))),
          ConstructorSpec("pInstance", List(
            Elem.Tok("["), Elem.Tok("Ord"), Elem.NameLeaf, Elem.Tok("]"))),
          ConstructorSpec("pExplicit", List(
            Elem.Tok("("), Elem.NameLeaf, Elem.Tok(":"), Elem.Cat("type"), Elem.Tok(")"))))),
        CategorySpec("tyAtom", List(
          ConstructorSpec("tyList", List(Elem.Tok("List"), Elem.Cat("tyAtom"))),
          ConstructorSpec("tyState", List(
            Elem.Tok("("), Elem.Tok("StateM"), Elem.Cat("tyAtom"), Elem.Tok("("),
            Elem.Cat("type"), Elem.Tok(")"), Elem.Tok(")"))),
          ConstructorSpec("$group", List(Elem.Tok("("), Elem.Cat("type"), Elem.Tok(")"))),
          ConstructorSpec("tyInt", List(Elem.Tok("Int"))),
          ConstructorSpec("tyBool", List(Elem.Tok("Bool"))),
          ConstructorSpec("tyUnit", List(Elem.Tok("Unit"))),
          ConstructorSpec("tyProp", List(Elem.Tok("Prop"))),
          ConstructorSpec("tyNat", List(Elem.Tok("Nat"))),
          ConstructorSpec("tyVar", List(Elem.NameLeaf)))),
        CategorySpec("expr", List(
          ConstructorSpec("call", List(
            Elem.Tok("("), Elem.NameLeaf, Elem.Star(Elem.Cat("expr")), Elem.Tok(")"))),
          ConstructorSpec("evar", List(Elem.NameLeaf)),
          ConstructorSpec("num", List(Elem.NumLeaf)))),
        CategorySpec("prop", List(
          ConstructorSpec("forallP", List(
            Elem.Tok("∀"), Elem.NameLeaf, Elem.Tok(":"), Elem.Tok("List"), Elem.Tok("Int"),
            Elem.Tok(","), Elem.Cat("prop"))),
          ConstructorSpec("sortedP", List(Elem.Tok("listSorted"), Elem.Cat("expr"))),
          ConstructorSpec("permP", List(Elem.Tok("listPerm"), Elem.Cat("expr"), Elem.Cat("expr")))))),
      precCategories = List(PrecCategory("type", "tyAtom", List(InfixOp("->", "tyArrow", 1, true)))),
      printRules = List(
        PrintRule("file", List(
          PrintSeg.Lit("--!"), PrintSeg.Field(0), PrintSeg.Newline,
          PrintSeg.Lit("namespace"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Newline,
          PrintSeg.SepFields(2, "\n"), PrintSeg.Newline,
          PrintSeg.Lit("end"), PrintSeg.Space, PrintSeg.Field(3))),
        PrintRule("indD", List(
          PrintSeg.Lit("inductive"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
          PrintSeg.Lit("where"), PrintSeg.Field(1))),
        PrintRule("defD", List(
          PrintSeg.Lit("partial def"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
          PrintSeg.SepFields(1, " "), PrintSeg.Space, PrintSeg.Lit(":"), PrintSeg.Space,
          PrintSeg.Field(2), PrintSeg.Space, PrintSeg.Lit(":="), PrintSeg.Field(3))),
        PrintRule("pureDefD", List(
          PrintSeg.Lit("def"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
          PrintSeg.SepFields(1, " "), PrintSeg.Space, PrintSeg.Lit(":"), PrintSeg.Space,
          PrintSeg.Field(2), PrintSeg.Space, PrintSeg.Lit(":="), PrintSeg.Field(3))),
        PrintRule("thmD", List(
          PrintSeg.Lit("theorem"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
          PrintSeg.Lit(":"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space, PrintSeg.Lit(":="), PrintSeg.Field(2))),
        PrintRule("pImplicit", List(PrintSeg.Lit("{"), PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(": Type}"))),
        PrintRule("pInstance", List(PrintSeg.Lit("[Ord"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Lit("]"))),
        PrintRule("pExplicit", List(
          PrintSeg.Lit("("), PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit(":"), PrintSeg.Space,
          PrintSeg.Field(1), PrintSeg.Lit(")"))),
        PrintRule("tyList", List(PrintSeg.Lit("List"), PrintSeg.Space, PrintSeg.Field(0))),
        PrintRule("tyState", List(
          PrintSeg.Lit("(StateM"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
          PrintSeg.Lit("("), PrintSeg.Field(1), PrintSeg.Lit("))"))),
        PrintRule("tyInt", List(PrintSeg.Lit("Int"))),
        PrintRule("tyBool", List(PrintSeg.Lit("Bool"))),
        PrintRule("tyUnit", List(PrintSeg.Lit("Unit"))),
        PrintRule("tyProp", List(PrintSeg.Lit("Prop"))),
        PrintRule("tyNat", List(PrintSeg.Lit("Nat"))),
        PrintRule("tyVar", List(PrintSeg.Field(0))),
        PrintRule("call", List(
          PrintSeg.Lit("("), PrintSeg.Field(0), PrintSeg.Space, PrintSeg.SepFields(1, " "), PrintSeg.Lit(")"))),
        PrintRule("evar", List(PrintSeg.Field(0))),
        PrintRule("num", List(PrintSeg.Field(0))),
        PrintRule("forallP", List(
          PrintSeg.Lit("∀"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space,
          PrintSeg.Lit(": List Int,"), PrintSeg.Space, PrintSeg.Field(1))),
        PrintRule("sortedP", List(PrintSeg.Lit("listSorted"), PrintSeg.Space, PrintSeg.Field(0))),
        PrintRule("permP", List(
          PrintSeg.Lit("listPerm"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Field(1)))),
      top = "file")

    def body(e: Cst, effectCtx: Boolean): String = ExprUtil.foldE[String](e)(
      varF = identity,
      numF = identity,
      nilF = "[]",
      callF = (f, as) =>
        if effectCtx && f == "tick" && as.isEmpty then "tick"
        else if as.isEmpty then f else s"($f ${as.mkString(" ")})",
      ifF = (c, a, b) => s"(bif $c then $a else $b)",
      matchF = (s, nilB, h, t, consB) => s"(match $s with | [] => $nilB | $h :: $t => $consB)",
      seqF = (a, b) => s"(do $a; pure $b)",
      trueF = "true", falseF = "false",
      eqF = (a, b) => s"($a == $b)", leF = (a, b) => s"($a <= $b)", andF = (a, b) => s"($a && $b)")

    private def statementExpr(e: Cst): Cst = e match
      case Cst.Node("rvar", List(Cst.Leaf(x)))  => Cst.node("evar", leaf(x))
      case Cst.Node("rcall", Cst.Leaf(f) :: as) => Cst.node("call", leaf(f), lst(as.map(statementExpr)))
      case other => throw CodecError(s"bad statement expr: ${other.render}")

    private def prop(s: Cst): Cst = s match
      case Cst.Node("rforall", List(x, b)) => Cst.node("forallP", x, prop(b))
      case Cst.Node("rsorted", List(e))    => Cst.node("sortedP", statementExpr(e))
      case Cst.Node("rperm", List(a, b))   => Cst.node("permP", statementExpr(a), statementExpr(b))
      case other => throw CodecError(s"bad statement: ${other.render}")

    def emit(m: RosettaModule2): Either[String, PortOutput] =
      try
        val cap = m.name.capitalize
        def pE(n: String, t: Cst): Cst = Cst.node("pExplicit", leaf(n), t)
        def listOf(t: Cst): Cst = Cst.node("tyList", t)
        val a = Cst.node("tyVar", leaf("a"))
        def gen(ps: (String, Cst)*): List[Cst] =
          Cst.node("pImplicit", leaf("a")) :: Cst.node("pInstance", leaf("a")) :: ps.toList.map(pE.tupled)
        val prelude: List[Cst] = List(
          Cst.node("defD", leaf("hd"), lst(gen("xs" -> listOf(a))),
            a, leaf(" match xs with | x :: _ => x | [] => hd xs")),
          Cst.node("defD", leaf("tl"), lst(gen("xs" -> listOf(a))), listOf(a), leaf(" xs.tail")),
          Cst.node("defD", leaf("append"), lst(gen("x" -> listOf(a), "y" -> listOf(a))), listOf(a), leaf(" x ++ y")),
          Cst.node("defD", leaf("cons"), lst(gen("h" -> a, "t" -> listOf(a))), listOf(a), leaf(" h :: t")),
          Cst.node("defD", leaf("isEmpty"), lst(gen("xs" -> listOf(a))), Cst.node("tyBool"), leaf(" xs.isEmpty")),
          Cst.node("defD", leaf("filterLt"), lst(gen("p" -> a, "xs" -> listOf(a))), listOf(a),
            leaf(" xs.filter (fun x => Ord.compare x p == Ordering.lt)")),
          Cst.node("defD", leaf("filterGe"), lst(gen("p" -> a, "xs" -> listOf(a))), listOf(a),
            leaf(" xs.filter (fun x => Ord.compare x p != Ordering.lt)")),
          Cst.node("pureDefD", leaf("listSorted"), lst(List(pE("xs", listOf(Cst.node("tyInt"))))),
            Cst.node("tyProp"), leaf(" List.Pairwise (fun x y => x <= y) xs")),
          Cst.node("pureDefD", leaf("listPerm"), lst(List(pE("x", listOf(Cst.node("tyInt"))), pE("y", listOf(Cst.node("tyInt"))))),
            Cst.node("tyProp"), leaf(" List.Perm x y")),
          Cst.node("pureDefD", leaf("tick"), lst(Nil),
            Cst.node("tyState", Cst.node("tyNat"), Cst.node("tyUnit")), leaf(" modify (fun n => n + 1)")))
        val dataDecls = m.datas.map(d => Cst.node("indD", leaf(d.name),
          leaf(d.ctors.map(c => s" | ${c._1} : ${d.name}").mkString)))
        val userDefs = m.defs.map { d =>
          val ps: List[Cst] =
            (if d.typeParams.isEmpty then Nil
             else d.typeParams.flatMap(tp => List(Cst.node("pImplicit", leaf(tp)))
               ++ d.constraints.filter(_._1 == tp).map(_ => Cst.node("pInstance", leaf(tp))))) ++
            d.params.map((n, t) => pE(n, ty(t)))
          val retTy = d.effect match
            case Some(_) => Cst.node("tyState", Cst.node("tyNat"), ty(d.ret))
            case None    => ty(d.ret)
          Cst.node("defD", leaf(d.name), lst(ps), retTy, leaf(" " + body(d.body, d.effect.isDefined)))
        }
        val theorems = m.theorems.map(t =>
          Cst.node("thmD", leaf(t.name), prop(t.statement), leaf(" by sorry")))
        val fileCst = Cst.node("file",
          leaf(s" generated by cairn rosetta lean port — artifact ${m.artifact.digest.short}"),
          leaf(cap), lst(dataDecls ++ prelude ++ userDefs ++ theorems), leaf(cap))
        Printer.print(fileGrammar, fileCst).map(text =>
          PortOutput(hostName, s"${m.name}.lean", text, Nil))
      catch case e: CodecError => Left(e.msg)

  // =========================== HASKELL ===========================

  object HaskellPort2 extends PortV2:
    val hostName = "haskell"

    private def ty(t: RTy): Cst = t match
      case RTy.RInt      => Cst.node("hInt")
      case RTy.RBool     => Cst.node("hBool")
      case RTy.RUnit     => Cst.node("hUnit")
      case RTy.RVar(n)   => Cst.node("hVar", leaf(n))
      case RTy.RList(of) => Cst.node("hList", ty(of))

    val fileGrammar: GrammarSpec = GrammarSpec(
      name = "port-haskell-file",
      tokens = TokenSpec(
        keywords = List("module", "where", "data", "Ord", "Int", "Bool", "IO", "State", "main"),
        puncts = List("[", "]", "(", ")", "::", "=>", ",", "->"),
        lineComment = None,
        restOfLineMarkers = List("-- !", "= ")),
      categories = List(
        CategorySpec("file", List(
          ConstructorSpec("file", List(Elem.RestOfLine, Elem.Tok("module"), Elem.NameLeaf,
            Elem.Tok("where"), Elem.Star(Elem.Cat("decl")))))),
        CategorySpec("decl", List(
          ConstructorSpec("dataD", List(Elem.Tok("data"), Elem.NameLeaf, Elem.RestOfLine)),
          ConstructorSpec("sigD", List(
            Elem.AnyIdentLeaf, Elem.Tok("::"), Elem.Opt(Elem.Cat("constraint")), Elem.Cat("htype"))),
          ConstructorSpec("bindD", List(Elem.AnyIdentLeaf, Elem.Star(Elem.NameLeaf), Elem.RestOfLine)))),
        CategorySpec("constraint", List(
          ConstructorSpec("cOrd", List(Elem.Tok("Ord"), Elem.NameLeaf, Elem.Tok("=>"))))),
        CategorySpec("hAtom", List(
          ConstructorSpec("hList", List(Elem.Tok("["), Elem.Cat("htype"), Elem.Tok("]"))),
          ConstructorSpec("hState", List(
            Elem.Tok("("), Elem.Tok("State"), Elem.Cat("hAtom"), Elem.Cat("hAtom"), Elem.Tok(")"))),
          ConstructorSpec("hIO", List(Elem.Tok("IO"), Elem.Tok("("), Elem.Tok(")"))),
          ConstructorSpec("hUnit", List(Elem.Tok("("), Elem.Tok(")"))),
          ConstructorSpec("$group", List(Elem.Tok("("), Elem.Cat("htype"), Elem.Tok(")"))),
          ConstructorSpec("hInt", List(Elem.Tok("Int"))),
          ConstructorSpec("hBool", List(Elem.Tok("Bool"))),
          ConstructorSpec("hVar", List(Elem.NameLeaf)))),
      ),
      precCategories = List(PrecCategory("htype", "hAtom", List(InfixOp("->", "hArrow", 1, true)))),
      printRules = List(
        PrintRule("file", List(
          PrintSeg.Lit("-- !"), PrintSeg.Field(0), PrintSeg.Newline,
          PrintSeg.Lit("module"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Space,
          PrintSeg.Lit("where"), PrintSeg.Newline,
          PrintSeg.SepFields(2, "\n"))),
        PrintRule("dataD", List(
          PrintSeg.Lit("data"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("= "), PrintSeg.Field(1))),
        PrintRule("sigD", List(
          PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("::"), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Field(2))),
        PrintRule("bindD", List(
          PrintSeg.Field(0), PrintSeg.Space, PrintSeg.SepFields(1, " "), PrintSeg.Space, PrintSeg.Lit("= "), PrintSeg.Field(2))),
        PrintRule("cOrd", List(PrintSeg.Lit("Ord"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("=>"), PrintSeg.Space)),
        PrintRule("hList", List(PrintSeg.Lit("["), PrintSeg.Field(0), PrintSeg.Lit("]"))),
        PrintRule("hState", List(
          PrintSeg.Lit("(State"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Field(1), PrintSeg.Lit(")"))),
        PrintRule("hIO", List(PrintSeg.Lit("IO ()"))),
        PrintRule("hUnit", List(PrintSeg.Lit("()"))),
        PrintRule("hInt", List(PrintSeg.Lit("Int"))),
        PrintRule("hBool", List(PrintSeg.Lit("Bool"))),
        PrintRule("hVar", List(PrintSeg.Field(0)))),
      top = "file")

    def body(e: Cst, effectCtx: Boolean): String = ExprUtil.foldE[String](e)(
      varF = identity,
      numF = identity,
      nilF = "[]",
      callF = (f, as) =>
        if effectCtx && f == "tick" && as.isEmpty then "tick"
        else if as.isEmpty then f else s"($f ${as.mkString(" ")})",
      ifF = (c, a, b) => s"(if $c then $a else $b)",
      matchF = (s, nilB, h, t, consB) => s"(case $s of { [] -> $nilB ; ($h : $t) -> $consB })",
      seqF = (a, b) => s"($a >> return $b)",
      trueF = "True", falseF = "False",
      eqF = (a, b) => s"($a == $b)", leF = (a, b) => s"($a <= $b)", andF = (a, b) => s"($a && $b)")

    private def arrowTy(params: List[RTy], ret: Cst): Cst =
      params.foldRight(ret)((p, acc) => Cst.node("hArrow", ty(p), acc))

    def emit(m: RosettaModule2): Either[String, PortOutput] =
      try
        val cap = m.name.capitalize
        def sig(n: String, constrained: Option[String], t: Cst): Cst =
          Cst.node("sigD", leaf(n),
            constrained.fold(Cst.node("none"))(v => Cst.node("some", Cst.node("cOrd", leaf(v)))), t)
        def bind(n: String, args: List[String], b: String): Cst =
          Cst.node("bindD", leaf(n), lst(args.map(leaf)), leaf(b))
        val la = Cst.node("hList", Cst.node("hVar", leaf("a")))
        val li = Cst.node("hList", Cst.node("hInt"))
        def arr(a: Cst, b: Cst) = Cst.node("hArrow", a, b)
        val prelude: List[Cst] = List(
          sig("hd", None, arr(la, Cst.node("hVar", leaf("a")))), bind("hd", List("xs"), "head xs"),
          sig("tl", None, arr(la, la)), bind("tl", List("xs"), "tail xs"),
          sig("append", None, arr(la, arr(la, la))), bind("append", List("x", "y"), "x ++ y"),
          sig("cons", None, arr(Cst.node("hVar", leaf("a")), arr(la, la))), bind("cons", List("h", "t"), "h : t"),
          sig("isEmpty", None, arr(la, Cst.node("hBool"))), bind("isEmpty", List("xs"), "null xs"),
          sig("filterLt", Some("a"), arr(Cst.node("hVar", leaf("a")), arr(la, la))),
          bind("filterLt", List("p", "xs"), "filter (< p) xs"),
          sig("filterGe", Some("a"), arr(Cst.node("hVar", leaf("a")), arr(la, la))),
          bind("filterGe", List("p", "xs"), "filter (>= p) xs"),
          sig("isSorted", None, arr(li, Cst.node("hBool"))),
          bind("isSorted", List("xs"), body(isSortedBody, false)),
          sig("isPerm", None, arr(li, arr(li, Cst.node("hBool")))),
          bind("isPerm", List("x", "y"), "foldr insertSorted [] x == foldr insertSorted [] y"),
          sig("insertSorted", None, arr(Cst.node("hInt"), arr(li, li))),
          bind("insertSorted", List("v", "xs"), "case xs of { [] -> [v] ; (h : t) -> if v <= h then v : xs else h : insertSorted v t }"))
        val dataDecls = m.datas.map(d => Cst.node("dataD", leaf(d.name),
          leaf(d.ctors.map(_._1).mkString(" | "))))
        val userDecls = m.defs.flatMap { d =>
          if d.effect.isDefined then Nil // Haskell effect projection: deferred honestly (no State import battle)
          else
            val constrained = d.constraints.headOption.map(_._1)
            List(
              sig(d.name, constrained, arrowTy(d.params.map(_._2), ty(d.ret))),
              bind(d.name, d.params.map(_._1), body(d.body, false)))
        }
        val checks = m.theorems.map { t =>
          def prop(s: Cst): String = s match
            case Cst.Node("rforall", List(Cst.Leaf(_), b)) => prop(b)
            case Cst.Node("rsorted", List(e))  => s"isSorted ${expr(e)}"
            case Cst.Node("rperm", List(a, b)) => s"isPerm ${expr(a)} ${expr(b)}"
            case other => throw CodecError(s"bad statement: ${other.render}")
          def expr(e: Cst): String = ExprUtil.foldE[String](e)(identity, identity, "[]",
            (f, as) => s"($f ${as.mkString(" ")})", (c, a, b) => s"(if $c then $a else $b)",
            (_, _, _, _, _) => throw CodecError("match"), (_, _) => throw CodecError("seq"),
            "True", "False", (a, b) => s"($a == $b)", (a, b) => s"($a <= $b)", (a, b) => s"($a && $b)")
          prop(t.statement)
        }
        val mainDecls = List(
          sig("main", None, Cst.node("hIO")),
          bind("main", Nil,
            s"""if all (\\xs -> ${checks.map(c => s"($c)").mkString(" && ")}) [[], [1], [3, 1, 2], [5, 4, 3, 2, 1], [2, 2, 1, 1]] then putStrLn "ALL TESTS PASS" else error "FAILED""""))
        val fileCst = Cst.node("file",
          leaf(s" generated by cairn rosetta haskell port — artifact ${m.artifact.digest.short}"),
          leaf(cap), lst(dataDecls ++ prelude ++ userDecls ++ mainDecls))
        Printer.print(fileGrammar, fileCst).map(text =>
          PortOutput(hostName, s"${m.name}.hs", text, Nil))
      catch case e: CodecError => Left(e.msg)

  // =========================== RUST ===========================

  object RustPort2 extends PortV2:
    val hostName = "rust"

    private def ty(t: RTy): Cst = t match
      case RTy.RInt      => Cst.node("rInt")
      case RTy.RBool     => Cst.node("rBool")
      case RTy.RUnit     => Cst.node("rUnit")
      case RTy.RVar(n)   => Cst.node("rVar", leaf(n))
      case RTy.RList(of) => Cst.node("rVec", ty(of))

    val fileGrammar: GrammarSpec = GrammarSpec(
      name = "port-rust-file",
      tokens = TokenSpec(
        keywords = List("fn", "enum", "struct", "impl", "test", "main", "Ord", "Clone", "Vec", "i64", "bool", "mut"),
        puncts = List("<", ">", "(", ")", "[", "]", ":", ",", "->", "&", "+", "#"),
        lineComment = None,
        restOfLineMarkers = List("//!", "{ ")),
      categories = List(
        CategorySpec("file", List(
          ConstructorSpec("file", List(Elem.RestOfLine, Elem.Star(Elem.Cat("decl")))))),
        CategorySpec("decl", List(
          ConstructorSpec("enumD", List(Elem.Tok("enum"), Elem.NameLeaf, Elem.RestOfLine)),
          ConstructorSpec("structD", List(Elem.Tok("struct"), Elem.NameLeaf, Elem.RestOfLine)),
          ConstructorSpec("implD", List(Elem.Tok("impl"), Elem.NameLeaf, Elem.RestOfLine)),
          ConstructorSpec("testD", List(
            Elem.Tok("#"), Elem.Tok("["), Elem.Tok("test"), Elem.Tok("]"),
            Elem.Tok("fn"), Elem.NameLeaf, Elem.Tok("("), Elem.Tok(")"), Elem.RestOfLine)),
          ConstructorSpec("fnD", List(
            Elem.Tok("fn"), Elem.AnyIdentLeaf, Elem.Opt(Elem.Cat("generics")),
            Elem.Tok("("), Elem.Opt(Elem.SepBy1(Elem.Cat("param"), ",")), Elem.Tok(")"),
            Elem.Opt(Elem.Cat("retClause")), Elem.RestOfLine)))),
        CategorySpec("generics", List(
          ConstructorSpec("generics", List(
            Elem.Tok("<"), Elem.NameLeaf, Elem.Tok(":"), Elem.Tok("Ord"),
            Elem.Tok("+"), Elem.Tok("Clone"), Elem.Tok(">"))))),
        CategorySpec("param", List(
          ConstructorSpec("param", List(Elem.NameLeaf, Elem.Tok(":"), Elem.Cat("rtype"))),
          ConstructorSpec("paramMut", List(
            Elem.NameLeaf, Elem.Tok(":"), Elem.Tok("&"), Elem.Tok("mut"), Elem.NameLeaf)))),
        CategorySpec("retClause", List(
          ConstructorSpec("ret", List(Elem.Tok("->"), Elem.Cat("rtype"))))),
        CategorySpec("rtype", List(
          ConstructorSpec("rSlice", List(Elem.Tok("&"), Elem.Tok("["), Elem.Cat("rtype"), Elem.Tok("]"))),
          ConstructorSpec("rRef", List(Elem.Tok("&"), Elem.NameLeaf)),
          ConstructorSpec("rVec", List(Elem.Tok("Vec"), Elem.Tok("<"), Elem.Cat("rtype"), Elem.Tok(">"))),
          ConstructorSpec("rInt", List(Elem.Tok("i64"))),
          ConstructorSpec("rBool", List(Elem.Tok("bool"))),
          ConstructorSpec("rUnit", List(Elem.Tok("("), Elem.Tok(")"))),
          ConstructorSpec("rVar", List(Elem.NameLeaf))))),
      precCategories = Nil,
      printRules = List(
        PrintRule("file", List(
          PrintSeg.Lit("//!"), PrintSeg.Field(0), PrintSeg.Newline, PrintSeg.SepFields(1, "\n"))),
        PrintRule("enumD", List(
          PrintSeg.Lit("enum"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("{ "), PrintSeg.Field(1))),
        PrintRule("structD", List(
          PrintSeg.Lit("struct"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("{ "), PrintSeg.Field(1))),
        PrintRule("implD", List(
          PrintSeg.Lit("impl"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Space, PrintSeg.Lit("{ "), PrintSeg.Field(1))),
        PrintRule("testD", List(
          PrintSeg.Lit("#[test] fn"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Lit("()"), PrintSeg.Space,
          PrintSeg.Lit("{ "), PrintSeg.Field(1))),
        PrintRule("fnD", List(
          PrintSeg.Lit("fn"), PrintSeg.Space, PrintSeg.Field(0), PrintSeg.Field(1),
          PrintSeg.Lit("("), PrintSeg.Field(2), PrintSeg.Lit(")"), PrintSeg.Field(3),
          PrintSeg.Space, PrintSeg.Lit("{ "), PrintSeg.Field(4))),
        PrintRule("generics", List(PrintSeg.Lit("<"), PrintSeg.Field(0), PrintSeg.Lit(": Ord + Clone>"))),
        PrintRule("param", List(PrintSeg.Field(0), PrintSeg.Lit(":"), PrintSeg.Space, PrintSeg.Field(1))),
        PrintRule("paramMut", List(
          PrintSeg.Field(0), PrintSeg.Lit(":"), PrintSeg.Space, PrintSeg.Lit("&mut"), PrintSeg.Space, PrintSeg.Field(1))),
        PrintRule("ret", List(PrintSeg.Space, PrintSeg.Lit("->"), PrintSeg.Space, PrintSeg.Field(0))),
        PrintRule("rSlice", List(PrintSeg.Lit("&["), PrintSeg.Field(0), PrintSeg.Lit("]"))),
        PrintRule("rRef", List(PrintSeg.Lit("&"), PrintSeg.Field(0))),
        PrintRule("rVec", List(PrintSeg.Lit("Vec<"), PrintSeg.Field(0), PrintSeg.Lit(">"))),
        PrintRule("rInt", List(PrintSeg.Lit("i64"))),
        PrintRule("rBool", List(PrintSeg.Lit("bool"))),
        PrintRule("rUnit", List(PrintSeg.Lit("()"))),
        PrintRule("rVar", List(PrintSeg.Field(0)))),
      top = "file")

    def snake(s: String): String =
      s.flatMap(c => if c.isUpper then s"_${c.toLower}" else c.toString)

    /** One renderer: slice params stay bare; every computed argument is
      * borrowed (`&Vec` / `&T` coerce to `&[T]` / `&T` at call sites).
      */
    private def render(e: Cst, effectCtx: Boolean, varMap: String => String): String =
      ExprUtil.foldE[String](e)(
        varF = varMap,
        numF = identity,
        nilF = "vec![]",
        callF = (f, as) =>
          if effectCtx && f == "tick" && as.isEmpty then "c.tick()"
          else s"${snake(f)}(${as.map(a => if a == "xs" then a else s"&($a)").mkString(", ")})",
        ifF = (c, a, b) => s"if $c { $a } else { $b }",
        matchF = (s, nilB, h, t, consB) => s"match $s.split_first() { None => $nilB, Some(($h, $t)) => $consB }",
        seqF = (a, b) => s"{ $a; $b }",
        trueF = "true", falseF = "false",
        eqF = (a, b) => s"($a == $b)", leF = (a, b) => s"($a <= $b)", andF = (a, b) => s"($a && $b)")

    def emit(m: RosettaModule2): Either[String, PortOutput] =
      try
        def fn(name: String, generic: Boolean, params: List[Cst], ret: Option[Cst], b: String): Cst =
          Cst.node("fnD", leaf(name),
            if generic then Cst.node("some", Cst.node("generics", leaf("T"))) else Cst.node("none"),
            if params.isEmpty then Cst.node("none") else Cst.node("some", lst(params)),
            ret.fold(Cst.node("none"))(t => Cst.node("some", Cst.node("ret", t))),
            leaf(b))
        def p(n: String, t: Cst): Cst = Cst.node("param", leaf(n), t)
        val sliceT = Cst.node("rSlice", Cst.node("rVar", leaf("T")))
        val vecT = Cst.node("rVec", Cst.node("rVar", leaf("T")))
        val refT = Cst.node("rRef", leaf("T"))
        val sliceI = Cst.node("rSlice", Cst.node("rInt"))
        val vecI = Cst.node("rVec", Cst.node("rInt"))
        val prelude: List[Cst] = List(
          fn("hd", true, List(p("xs", sliceT)), Some(Cst.node("rVar", leaf("T"))), "xs[0].clone() }"),
          fn("tl", true, List(p("xs", sliceT)), Some(vecT), "xs[1..].to_vec() }"),
          fn("append", true, List(p("x", sliceT), p("y", sliceT)), Some(vecT),
            "let mut v = x.to_vec(); v.extend_from_slice(y); v }"),
          fn("cons", true, List(p("h", refT), p("t", sliceT)), Some(vecT),
            "let mut v = vec![h.clone()]; v.extend_from_slice(t); v }"),
          fn("is_empty", true, List(p("xs", sliceT)), Some(Cst.node("rBool")), "xs.is_empty() }"),
          fn("filter_lt", true, List(p("p", refT), p("xs", sliceT)), Some(vecT),
            "xs.iter().filter(|x| *x < p).cloned().collect() }"),
          fn("filter_ge", true, List(p("p", refT), p("xs", sliceT)), Some(vecT),
            "xs.iter().filter(|x| *x >= p).cloned().collect() }"),
          fn("is_sorted", false, List(p("xs", sliceI)), Some(Cst.node("rBool")),
            render(isSortedBody, false, identity) + " }"),
          fn("is_perm", false, List(p("x", sliceI), p("y", sliceI)), Some(Cst.node("rBool")),
            "let mut a = x.to_vec(); let mut b = y.to_vec(); a.sort(); b.sort(); a == b }"))
        val effectDecls = m.effects.flatMap { e =>
          List(
            Cst.node("structD", leaf(e.name.capitalize), leaf("n: i64 }")),
            Cst.node("implD", leaf(e.name.capitalize),
              leaf(s"fn new() -> Self { ${e.name.capitalize} { n: 0 } } ${e.ops.map(op => s"fn $op(&mut self) { self.n += 1; }").mkString(" ")} }")))
        }
        val dataDecls = m.datas.map(d => Cst.node("enumD", leaf(d.name),
          leaf(d.ctors.map(_._1).mkString(", ") + " }")))
        // Rust body: the neutral body references xs (a slice); recursive calls
        // must pass slices — the renderer wraps recursive-call args with & only
        // when they are not already the slice param
        val userFns = m.defs.map { d =>
          val generic = d.typeParams.nonEmpty
          val eff = d.effect.isDefined
          val params =
            (if eff then List(Cst.node("paramMut", leaf("c"), leaf(d.effect.get.capitalize))) else Nil) ++
            d.params.map((n, t) => p(n, t match
              case RTy.RList(RTy.RVar(_)) => sliceT
              case RTy.RList(RTy.RInt)    => sliceI
              case other                  => ty(other)))
          fn(snake(d.name), generic, params,
            Some(d.ret match
              case RTy.RList(RTy.RVar(_)) => vecT
              case RTy.RList(RTy.RInt)    => vecI
              case other                  => ty(other)),
            render(d.body, eff, identity) + " }")
        }
        val tests = m.theorems.map { t =>
          def prop(s: Cst): String = s match
            case Cst.Node("rforall", List(Cst.Leaf(_), b)) => prop(b)
            case Cst.Node("rsorted", List(e))  => s"is_sorted(&(${render(e, false, v => s"&$v")}))"
            case Cst.Node("rperm", List(a, b)) => s"is_perm(&(${render(a, false, v => s"&$v")}), &(${render(b, false, v => s"&$v")}))"
            case other => throw CodecError(s"bad statement: ${other.render}")
          Cst.node("testD", leaf(snake(t.name)),
            leaf(s"for xs in [vec![], vec![1], vec![3, 1, 2], vec![5, 4, 3, 2, 1]] { assert!(${prop(t.statement)}); } }"))
        }
        val mainFn = fn("main", false, Nil, None,
          """println!("ALL TESTS PASS"); }""")
        val fileCst = Cst.node("file",
          leaf(s" generated by cairn rosetta rust port — artifact ${m.artifact.digest.short}"),
          lst(effectDecls ++ dataDecls ++ prelude ++ userFns ++ tests ++ List(mainFn)))
        Printer.print(fileGrammar, fileCst).map(text =>
          PortOutput(hostName, s"${m.name}.rs", text, Nil))
      catch case e: CodecError => Left(e.msg)
