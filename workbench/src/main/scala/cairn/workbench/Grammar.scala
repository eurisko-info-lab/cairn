package cairn.workbench

import cairn.kernel.*

/** One generic lexer (S9) driven entirely by TokenSpec — knows character
  * classes only, nothing about grammar shapes.
  */
enum TokKind:
  case Name, Keyword, Punct, Num, Str, Eof

final case class Token(kind: TokKind, text: String, line: Int, col: Int):
  def at: String = s"$line:$col"

final case class LexError(msg: String, line: Int, col: Int):
  def render: String = s"lex error at $line:$col: $msg"

object Lexer:
  def lex(spec: TokenSpec, input: String): Either[LexError, Vector[Token]] =
    val out = Vector.newBuilder[Token]
    var i = 0; var line = 1; var col = 1
    val punctsByLen = spec.puncts.sortBy(p => -p.length)
    def isIdentStart(c: Char) = c.isLetter || spec.identStartExtra.contains(c)
    def isIdentCont(c: Char) = c.isLetterOrDigit || spec.identContExtra.contains(c)
    def advance(n: Int): Unit =
      var k = 0
      while k < n do
        if input(i) == '\n' then { line += 1; col = 1 } else col += 1
        i += 1; k += 1
    while i < input.length do
      val c = input(i)
      if c.isWhitespace then advance(1)
      else if spec.lineComment.exists(lc => input.startsWith(lc, i)) then
        while i < input.length && input(i) != '\n' do advance(1)
      else if isIdentStart(c) then
        val (l0, c0) = (line, col)
        val start = i
        while i < input.length && isIdentCont(input(i)) do advance(1)
        val text = input.substring(start, i)
        out += Token(if spec.keywords.contains(text) then TokKind.Keyword else TokKind.Name, text, l0, c0)
      else if c.isDigit then
        val (l0, c0) = (line, col)
        val start = i
        while i < input.length && input(i).isDigit do advance(1)
        out += Token(TokKind.Num, input.substring(start, i), l0, c0)
      else if c == '"' then
        val (l0, c0) = (line, col)
        advance(1)
        val sb = StringBuilder()
        var closed = false
        while i < input.length && !closed do
          input(i) match
            case '"'  => closed = true; advance(1)
            case '\\' if i + 1 < input.length =>
              advance(1)
              sb += (input(i) match { case 'n' => '\n'; case 't' => '\t'; case other => other })
              advance(1)
            case other => sb += other; advance(1)
        if !closed then return Left(LexError("unterminated string", l0, c0))
        out += Token(TokKind.Str, sb.result(), l0, c0)
      else
        punctsByLen.find(p => input.startsWith(p, i)) match
          case Some(p) =>
            out += Token(TokKind.Punct, p, line, col)
            advance(p.length)
          case None => return Left(LexError(s"unexpected character '$c'", line, col))
    out += Token(TokKind.Eof, "", line, col)
    Right(out.result())

final case class ParseError(msg: String, token: Token):
  def render: String = s"parse error at ${token.at} (near '${token.text}'): $msg"

/** One generic recursive-descent parser (S10) interpreting GrammarSpec:
  * PEG-style ordered choice with full backtracking + precedence climbing.
  * Contains no per-construct code, ever.
  */
object Parser:
  /** Static left-recursion check: run against every grammar before use.
    * Detects a category whose alternative can recurse into itself (directly or
    * transitively through first elements) before consuming a token.
    */
  def leftRecursionCheck(g: GrammarSpec): Either[String, Unit] =
    def firstCats(name: String, seen: Set[String]): Either[String, Unit] =
      if seen.contains(name) then Left(s"left recursion through category path: ${(seen + name).mkString(" -> ")}")
      else
        val nexts: List[String] =
          g.category(name).map(_.ctors.flatMap(c => c.elems.headOption match
            case Some(Elem.Cat(n)) => List(n)
            case Some(Elem.Opt(Elem.Cat(n))) => List(n)
            case Some(Elem.Star(Elem.Cat(n))) => List(n)
            case _ => Nil)).getOrElse(Nil) ++
          g.precCategory(name).map(p => List(p.base)).getOrElse(Nil)
        nexts.distinct.foldLeft[Either[String, Unit]](Right(())) { (acc, n) =>
          acc.flatMap(_ => firstCats(n, seen + name)) }
    (g.categories.map(_.name) ++ g.precCategories.map(_.name))
      .foldLeft[Either[String, Unit]](Right(())) { (acc, n) => acc.flatMap(_ => firstCats(n, Set.empty)) }

  def parse(g: GrammarSpec, input: String): Either[String, Cst] =
    for
      _ <- leftRecursionCheck(g)
      toks <- Lexer.lex(g.tokens, input).left.map(_.render)
      cst <- run(g, toks)
    yield cst

  private def run(g: GrammarSpec, toks: Vector[Token]): Either[String, Cst] =
    var pos = 0
    def peek: Token = toks(pos)

    def parseCategory(name: String): Either[ParseError, Cst] =
      g.precCategory(name) match
        case Some(p) => parsePrec(p, 0)
        case None =>
          g.category(name) match
            case Some(cat) => tryCtors(cat)
            case None      => Left(ParseError(s"unknown grammar category '$name'", peek))

    def parsePrec(p: PrecCategory, minPrec: Int): Either[ParseError, Cst] =
      parseCategory(p.base).flatMap { first =>
        var lhs = first
        var done = false
        var err: Option[ParseError] = None
        while !done && err.isEmpty do
          val t = peek
          p.ops.find(o => o.text == t.text && (t.kind == TokKind.Punct || t.kind == TokKind.Keyword) && o.prec >= minPrec) match
            case Some(op) =>
              pos += 1
              val sub = if op.rightAssoc then op.prec else op.prec + 1
              parsePrec(p, sub) match
                case Right(rhs) => lhs = Cst.Node(op.tag, List(lhs, rhs))
                case Left(e)    => err = Some(e)
            case None => done = true
        err.toLeft(lhs)
      }

    def tryCtors(cat: CategorySpec): Either[ParseError, Cst] =
      val start = pos
      var best: ParseError = ParseError(s"no alternative of '${cat.name}' matched", peek)
      for ctor <- cat.ctors do
        pos = start
        parseFields(ctor.elems) match
          case Right(fields) =>
            // "$group" is transparent grouping: yields its single field directly.
            if ctor.tag == "$group" && fields.sizeIs == 1 then return Right(fields.head)
            else return Right(Cst.Node(ctor.tag, fields))
          case Left(e)       => best = e
      pos = start
      Left(best)

    def parseFields(elems: List[Elem]): Either[ParseError, List[Cst]] =
      val fields = List.newBuilder[Cst]
      for e <- elems do
        parseElem(e) match
          case Right(fs) => fields ++= fs
          case Left(err) => return Left(err)
      Right(fields.result())

    def parseElem(e: Elem): Either[ParseError, List[Cst]] = e match
      case Elem.Tok(text) =>
        if peek.text == text && peek.kind != TokKind.Str then { pos += 1; Right(Nil) }
        else Left(ParseError(s"expected '$text'", peek))
      case Elem.TokField(text) =>
        if peek.text == text && peek.kind != TokKind.Str then { pos += 1; Right(List(Cst.Leaf(text))) }
        else Left(ParseError(s"expected '$text'", peek))
      case Elem.Cat(n) => parseCategory(n).map(List(_))
      case Elem.Opt(inner) =>
        val start = pos
        parseElem(inner) match
          case Right(fs) => Right(List(Cst.Node("some", fs)))
          case Left(_)   => pos = start; Right(List(Cst.Node("none", Nil)))
      case Elem.Star(inner) =>
        val items = List.newBuilder[Cst]
        var going = true
        while going do
          val start = pos
          parseElem(inner) match
            case Right(fs) => items += (fs match { case List(one) => one; case fs => Cst.Node("group", fs) })
            case Left(_)   => pos = start; going = false
        Right(List(Cst.Node("list", items.result())))
      case Elem.SepBy1(inner, sep) =>
        parseElem(inner).flatMap { first =>
          val items = List.newBuilder[Cst]
          items += (first match { case List(one) => one; case fs => Cst.Node("group", fs) })
          var going = true
          var err: Option[ParseError] = None
          while going && err.isEmpty do
            if peek.text == sep && peek.kind == TokKind.Punct then
              pos += 1
              parseElem(inner) match
                case Right(fs) => items += (fs match { case List(one) => one; case fs => Cst.Node("group", fs) })
                case Left(e)   => err = Some(e)
            else going = false
          err.toLeft(List(Cst.Node("list", items.result())))
        }
      case Elem.NameLeaf =>
        if peek.kind == TokKind.Name then { val t = peek.text; pos += 1; Right(List(Cst.Leaf(t))) }
        else Left(ParseError(s"expected identifier", peek))
      case Elem.NumLeaf =>
        if peek.kind == TokKind.Num then { val t = peek.text; pos += 1; Right(List(Cst.Leaf(t))) }
        else Left(ParseError(s"expected number", peek))
      case Elem.StrLeaf =>
        if peek.kind == TokKind.Str then { val t = peek.text; pos += 1; Right(List(Cst.Leaf(t))) }
        else Left(ParseError(s"expected string literal", peek))

    parseCategory(g.top) match
      case Left(e) => Left(e.render)
      case Right(cst) =>
        if peek.kind == TokKind.Eof then Right(cst)
        else Left(ParseError("trailing input after top-level parse", peek).render)

/** One generic printer (S11) interpreting the print table by indexing a Cst's
  * children by position. Infix tags (from PrecCategory tables) print with
  * minimal parenthesization via precedence comparison.
  */
object Printer:
  def print(g: GrammarSpec, cst: Cst): Either[String, String] =
    val infixOps: Map[String, InfixOp] =
      g.precCategories.flatMap(_.ops).map(o => o.tag -> o).toMap
    val sb = StringBuilder()
    var indent = 0
    var atLineStart = true
    def emit(s: String): Unit =
      if s.nonEmpty then
        if atLineStart then { sb ++= "  " * indent; atLineStart = false }
        sb ++= s
    def newline(): Unit = { sb += '\n'; atLineStart = true }

    def go(c: Cst, parentPrec: Int): Either[String, Unit] = c match
      case Cst.Leaf(t) => emit(t); Right(())
      case Cst.Node("some", fs)  => fs.foldLeft[Either[String, Unit]](Right(())) { (acc, f) => acc.flatMap(_ => go(f, 0)) }
      case Cst.Node("none", _)   => Right(())
      case Cst.Node("list", fs)  =>
        fs.zipWithIndex.foldLeft[Either[String, Unit]](Right(())) { case (acc, (f, i)) =>
          acc.flatMap(_ => { if i > 0 then emit(", "); go(f, 0) }) }
      case Cst.Node(tag, children) =>
        infixOps.get(tag) match
          case Some(op) =>
            val needParens = op.prec < parentPrec
            if needParens then emit("(")
            val (lp, rp) = if op.rightAssoc then (op.prec + 1, op.prec) else (op.prec, op.prec + 1)
            for
              _ <- go(children(0), lp)
              _ = emit(s" ${op.text} ")
              _ <- go(children(1), rp)
            yield if needParens then emit(")")
          case None =>
            g.printRule(tag) match
              case None => Left(s"no print rule for constructor '$tag'")
              case Some(rule) =>
                rule.segs.foldLeft[Either[String, Unit]](Right(())) { (acc, seg) =>
                  acc.flatMap { _ =>
                    seg match
                      case PrintSeg.Lit(t)   => emit(t); Right(())
                      case PrintSeg.Space    => emit(" "); Right(())
                      case PrintSeg.Newline  => newline(); Right(())
                      case PrintSeg.IndentIn => indent += 1; Right(())
                      case PrintSeg.IndentOut => indent -= 1; Right(())
                      case PrintSeg.Field(i) =>
                        if i >= children.length then Left(s"print rule '$tag' field $i out of range (${children.length} children)")
                        else go(children(i), maxPrec(g))
                      case PrintSeg.SepFields(from, sep) =>
                        val items = children.drop(from) match
                          case List(Cst.Node("list", xs)) => xs
                          case other                      => other
                        items.zipWithIndex.foldLeft[Either[String, Unit]](Right(())) { case (a, (item, i)) =>
                          a.flatMap { _ =>
                            if i > 0 then { if sep.contains('\n') then newline() else emit(sep) }
                            go(item, maxPrec(g)) } }
                  }
                }

    def maxPrec(g: GrammarSpec): Int =
      (g.precCategories.flatMap(_.ops).map(_.prec) :+ 0).max + 1

    go(cst, 0).map(_ => sb.result())

/** Round-trip law suite hook (S11): every shipped grammar must satisfy
  * parse(print(t)) == t on its golden terms.
  */
object RoundTrip:
  def check(g: GrammarSpec, term: Cst): Either[String, Unit] =
    for
      printed <- Printer.print(g, term)
      reparsed <- Parser.parse(g, printed).left.map(e => s"$e\n--- printed text was:\n$printed")
      _ <- if reparsed == term then Right(())
           else Left(s"round-trip mismatch:\n  original: ${term.render}\n  printed:  $printed\n  reparsed: ${reparsed.render}")
    yield ()
