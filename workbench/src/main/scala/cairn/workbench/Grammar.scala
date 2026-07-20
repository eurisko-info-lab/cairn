package cairn.workbench

import cairn.kernel.*

/** One generic lexer (S9, M6, M7) driven entirely by TokenSpec — knows
  * character classes only. Tokens carry their char offset, raw length, and
  * LEADING TRIVIA (whitespace + comments preceding them) so concrete printing
  * can reproduce input byte-for-byte (M7).
  */
enum TokKind:
  case Name, Keyword, Punct, Num, Str, Rest, Eof

final case class Token(kind: TokKind, text: String, line: Int, col: Int,
                       offset: Int = 0, rawLen: Int = 0, leading: String = ""):
  def at: String = s"$line:$col"

final case class LexError(msg: String, line: Int, col: Int):
  def render: String = s"lex error at $line:$col: $msg"

object Lexer:
  def lex(spec: TokenSpec, input: String): Either[LexError, Vector[Token]] =
    val out = Vector.newBuilder[Token]
    var i = 0; var line = 1; var col = 1
    val punctsByLen = spec.puncts.sortBy(p => -p.length)
    val rolMarkers = spec.restOfLineMarkers.sortBy(m => -m.length)
    def isIdentStart(c: Char) = c.isLetter || spec.identStartExtra.contains(c)
    def isIdentCont(c: Char) = c.isLetterOrDigit || spec.identContExtra.contains(c)
    def advance(n: Int): Unit =
      var k = 0
      while k < n do
        if input(i) == '\n' then { line += 1; col = 1 } else col += 1
        i += 1; k += 1
    var triviaStart = 0
    while i < input.length do
      val c = input(i)
      val isRol = rolMarkers.exists(m => input.startsWith(m, i))
      if c.isWhitespace then advance(1)
      else if !isRol && spec.lineComment.exists(lc => input.startsWith(lc, i)) then
        while i < input.length && input(i) != '\n' do advance(1)
      else
        val leading = input.substring(triviaStart, i)
        val (l0, c0, o0) = (line, col, i)
        rolMarkers.find(m => input.startsWith(m, i)) match
          case Some(m) =>
            advance(m.length)
            val start = i
            while i < input.length && input(i) != '\n' do advance(1)
            out += Token(TokKind.Rest, input.substring(start, i), l0, c0, o0, i - o0, leading)
          case None =>
            if isIdentStart(c) then
              val start = i
              while i < input.length && isIdentCont(input(i)) do advance(1)
              val text = input.substring(start, i)
              out += Token(if spec.keywords.contains(text) then TokKind.Keyword else TokKind.Name,
                text, l0, c0, o0, i - o0, leading)
            else if c.isDigit then
              val start = i
              while i < input.length && input(i).isDigit do advance(1)
              out += Token(TokKind.Num, input.substring(start, i), l0, c0, o0, i - o0, leading)
            else if c == '"' then
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
              out += Token(TokKind.Str, sb.result(), l0, c0, o0, i - o0, leading)
            else
              punctsByLen.find(p => input.startsWith(p, i)) match
                case Some(p) =>
                  out += Token(TokKind.Punct, p, l0, c0, o0, p.length, leading)
                  advance(p.length)
                case None => return Left(LexError(s"unexpected character '$c'", line, col))
        triviaStart = i
    out += Token(TokKind.Eof, "", line, col, i, 0, input.substring(triviaStart, i))
    Right(out.result())

final case class ParseError(msg: String, token: Token):
  def render: String = s"parse error at ${token.at} (near '${token.text}'): $msg"
  /** M8: source excerpt with caret. */
  def renderRich(source: String): String =
    val lines = source.split("\n", -1)
    val ln = math.min(token.line, lines.length) - 1
    val srcLine = if ln >= 0 && ln < lines.length then lines(ln) else ""
    val caret = " " * math.max(0, token.col - 1) + "^"
    s"$render\n  ${token.line} | $srcLine\n  ${" " * token.line.toString.length} | $caret"

/** Node source spans (M8): token index range [startTok, endTok). Keyed by node
  * INSTANCE identity (the parser constructs each node fresh).
  */
final class SpanMap:
  private val m = java.util.IdentityHashMap[Cst, (Int, Int)]()
  def put(c: Cst, span: (Int, Int)): Unit = m.put(c, span)
  def get(c: Cst): Option[(Int, Int)] = Option(m.get(c))

final case class ParseOut(cst: Cst, tokens: Vector[Token], spans: SpanMap,
                          diagnostics: List[ParseError], steps: Int, memoHits: Int)

/** Packrat memo table (M10): (category, pos) -> outcome, reusable across
  * incremental reparses after span-precise invalidation.
  */
final class Memo:
  val table = scala.collection.mutable.Map[(String, Int), Either[ParseError, (Cst, Int)]]()
  var hits = 0
  def invalidateFrom(tokenIndex: Int): Unit =
    val stale = table.keys.filter { case (_, pos) => pos >= tokenIndex }.toList
    // NB: iterate as a list — collecting (String, Int) pairs straight off the
    // Map would rebuild a Map keyed by category name and silently deduplicate.
    val crossing = table.toList.collect {
      case (k @ (_, pos), Right((_, end))) if pos < tokenIndex && end >= tokenIndex => k
      case (k @ (_, pos), Left(_)) if pos < tokenIndex => k
    }
    (stale ++ crossing).foreach(table.remove)

/** One generic parser (S10, M6, M8, M10, M11): PEG ordered choice with full
  * backtracking, precedence climbing, layout combinators, packrat memoization,
  * span recording, furthest-failure diagnostics, and optional panic-mode
  * recovery at sync tokens inside Star. No per-construct code, ever.
  */
object Parser:
  def leftRecursionCheck(g: GrammarSpec): Either[String, Unit] =
    def firstCats(name: String, seen: Set[String]): Either[String, Unit] =
      if seen.contains(name) then Left(s"left recursion through category path: ${(seen + name).mkString(" -> ")}")
      else
        val nexts: List[String] =
          g.category(name).map(_.ctors.flatMap(c => c.elems.headOption match
            case Some(Elem.Cat(n)) => List(n)
            case Some(Elem.Opt(Elem.Cat(n))) => List(n)
            case Some(Elem.Star(Elem.Cat(n))) => List(n)
            case Some(Elem.Run(n)) => List(n)
            case Some(Elem.Block(n)) => List(n)
            case Some(Elem.Adjacent1(Elem.Cat(n))) => List(n)
            case _ => Nil)).getOrElse(Nil) ++
          g.precCategory(name).map(p => List(p.base)).getOrElse(Nil)
        nexts.distinct.foldLeft[Either[String, Unit]](Right(())) { (acc, n) =>
          acc.flatMap(_ => firstCats(n, seen + name)) }
    (g.categories.map(_.name) ++ g.precCategories.map(_.name))
      .foldLeft[Either[String, Unit]](Right(())) { (acc, n) => acc.flatMap(_ => firstCats(n, Set.empty)) }

  def parse(g: GrammarSpec, input: String): Either[String, Cst] =
    parseFull(g, input).map(_.cst)

  def parseRecovering(g: GrammarSpec, input: String): Either[String, ParseOut] =
    parseFull(g, input, recover = true)

  def parseFull(g: GrammarSpec, input: String, recover: Boolean = false,
                memo: Memo = Memo()): Either[String, ParseOut] =
    for
      _ <- leftRecursionCheck(g)
      toks <- Lexer.lex(g.tokens, input).left.map(_.render)
      out <- run(g, toks, input, recover, memo)
    yield out

  private def run(g: GrammarSpec, toks: Vector[Token], source: String,
                  recover: Boolean, memo: Memo): Either[String, ParseOut] =
    var pos = 0
    var steps = 0
    val memoHits0 = memo.hits
    val spans = SpanMap()
    val diags = List.newBuilder[ParseError]
    var furthestPos = -1
    var furthestExpected = Set.empty[String]
    def peek: Token = toks(pos)
    def fail(expected: String): ParseError =
      if pos > furthestPos then { furthestPos = pos; furthestExpected = Set(expected) }
      else if pos == furthestPos then furthestExpected += expected
      ParseError(s"expected $expected", peek)
    def spanned(c: Cst, start: Int): Cst = { spans.put(c, (start, pos)); c }

    def parseCategory(name: String): Either[ParseError, Cst] =
      steps += 1
      val key = (name, pos)
      memo.table.get(key) match
        case Some(Right((cst, end))) => memo.hits += 1; pos = end; Right(cst)
        case Some(Left(e))           => memo.hits += 1; Left(e)
        case None =>
          val result = g.precCategory(name) match
            case Some(p) => parsePrec(p, 0)
            case None =>
              g.category(name) match
                case Some(cat) => tryCtors(cat)
                case None      => Left(ParseError(s"unknown grammar category '$name'", peek))
          memo.table(key) = result.map(c => (c, pos))
          result

    def parsePrec(p: PrecCategory, minPrec: Int): Either[ParseError, Cst] =
      val start = pos
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
                case Right(rhs) => lhs = spanned(Cst.Node(op.tag, List(lhs, rhs)), start)
                case Left(e)    => err = Some(e)
            case None => done = true
        err.toLeft(lhs)
      }

    def tryCtors(cat: CategorySpec): Either[ParseError, Cst] =
      val start = pos
      var best: ParseError = fail(s"one of '${cat.name}'")
      for ctor <- cat.ctors do
        pos = start
        parseFields(ctor.elems) match
          case Right(fields) =>
            if ctor.tag == "$group" && fields.sizeIs == 1 then return Right(fields.head)
            else return Right(spanned(Cst.Node(ctor.tag, fields), start))
          case Left(e) =>
            // keep the FURTHEST failure — the alternative that made the most
            // progress explains the input best (M8/M11)
            if e.token.offset >= best.token.offset then best = e
      pos = start
      Left(best)

    def parseFields(elems: List[Elem]): Either[ParseError, List[Cst]] =
      val fields = List.newBuilder[Cst]
      for e <- elems do
        parseElem(e) match
          case Right(fs) => fields ++= fs
          case Left(err) => return Left(err)
      Right(fields.result())

    def lastConsumedLine: Int = if pos == 0 then 1 else toks(pos - 1).line

    def parseElem(e: Elem): Either[ParseError, List[Cst]] = e match
      case Elem.Tok(text) =>
        if peek.text == text && peek.kind != TokKind.Str && peek.kind != TokKind.Rest then { pos += 1; Right(Nil) }
        else Left(fail(s"'$text'"))
      case Elem.TokField(text) =>
        if peek.text == text && peek.kind != TokKind.Str then { pos += 1; Right(List(Cst.Leaf(text))) }
        else Left(fail(s"'$text'"))
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
            case Left(err) =>
              pos = start
              // recover only when the item made progress before failing;
              // a first-token failure means "end of this repetition" (e.g. the
              // closing brace), not a broken item.
              val madeProgress = err.token.offset > peek.offset
              if recover && g.syncTokens.nonEmpty && peek.kind != TokKind.Eof && madeProgress then
                // panic mode (M11): skip to a sync token, record an $error node
                diags += err
                val skipped = List.newBuilder[Cst]
                while peek.kind != TokKind.Eof && !g.syncTokens.contains(peek.text) do
                  skipped += Cst.Leaf(peek.text); pos += 1
                if peek.kind != TokKind.Eof then { skipped += Cst.Leaf(peek.text); pos += 1 }
                items += Cst.Node("$error", skipped.result())
              else going = false
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
        else Left(fail("identifier"))
      case Elem.AnyIdentLeaf =>
        if peek.kind == TokKind.Name || peek.kind == TokKind.Keyword then
          { val t = peek.text; pos += 1; Right(List(Cst.Leaf(t))) }
        else Left(fail("identifier or keyword"))
      case Elem.NumLeaf =>
        if peek.kind == TokKind.Num then { val t = peek.text; pos += 1; Right(List(Cst.Leaf(t))) }
        else Left(fail("number"))
      case Elem.StrLeaf =>
        if peek.kind == TokKind.Str then { val t = peek.text; pos += 1; Right(List(Cst.Leaf(t))) }
        else Left(fail("string literal"))
      case Elem.RestOfLine =>
        if peek.kind == TokKind.Rest then { val t = peek.text; pos += 1; Right(List(Cst.Leaf(t))) }
        else Left(fail("rest-of-line"))
      case Elem.Block(itemCat) =>
        // offside rule (M6): the first item fixes the column; items repeat at
        // exactly that column; a failing item AT the block column is a hard error.
        if peek.kind == TokKind.Eof then Right(List(Cst.Node("list", Nil)))
        else
          val blockCol = peek.col
          val items = List.newBuilder[Cst]
          var going = true
          var err: Option[ParseError] = None
          while going && err.isEmpty do
            if peek.kind != TokKind.Eof && peek.col == blockCol then
              parseCategory(itemCat) match
                case Right(c) => items += c
                case Left(e)  => err = Some(e)
            else going = false
          err.toLeft(List(Cst.Node("list", items.result())))
      case Elem.Run(atomCat) =>
        // juxtaposition (M6): atoms continue while on the same line as the last
        // consumed token, or indented past the run's floor column.
        val floorCol = peek.col
        parseCategory(atomCat).flatMap { first =>
          val items = List.newBuilder[Cst]
          items += first
          var going = true
          while going do
            val start = pos
            val onSameLine = peek.line == lastConsumedLine
            val indentedPast = peek.col > floorCol
            if peek.kind != TokKind.Eof && (onSameLine || indentedPast) then
              parseCategory(atomCat) match
                case Right(c) => items += c
                case Left(_)  => pos = start; going = false
            else going = false
          Right(List(Cst.Node("list", items.result())))
        }
      case Elem.Adjacent1(inner) =>
        parseElem(inner).flatMap { first =>
          val items = List.newBuilder[Cst]
          items += (first match { case List(one) => one; case fs => Cst.Node("group", fs) })
          var going = true
          while going do
            val start = pos
            val gap = peek.line > lastConsumedLine + 1
            if peek.kind != TokKind.Eof && !gap then
              parseElem(inner) match
                case Right(fs) => items += (fs match { case List(one) => one; case fs => Cst.Node("group", fs) })
                case Left(_)   => pos = start; going = false
            else going = false
          Right(List(Cst.Node("list", items.result())))
        }

    parseCategory(g.top) match
      case Left(e) =>
        val f = if furthestPos >= 0 then
          ParseError(s"expected ${furthestExpected.toList.sorted.mkString(" | ")}", toks(furthestPos))
        else e
        Left(f.renderRich(source))
      case Right(cst) =>
        if peek.kind == TokKind.Eof then
          Right(ParseOut(cst, toks, spans, diags.result(), steps, memo.hits - memoHits0))
        else
          // trailing input: the furthest failure explains WHY the parse stopped
          val err =
            if furthestPos > pos then
              ParseError(s"expected ${furthestExpected.toList.sorted.mkString(" | ")}", toks(furthestPos))
            else ParseError("trailing input after top-level parse", peek)
          Left(err.renderRich(source))

/** M10: incremental reparse — retain the memo table across edits, invalidating
  * only entries at or crossing the first changed token.
  */
final class IncrementalParser(g: GrammarSpec):
  private var memo = Memo()
  private var lastTokens: Vector[Token] = Vector.empty
  final case class Result(out: ParseOut, freshSteps: Int)

  def parse(source: String): Either[String, Result] =
    val toksE = Lexer.lex(g.tokens, source).left.map(_.render)
    toksE.flatMap { toks =>
      val firstChanged = lastTokens.zip(toks)
        .indexWhere((a, b) => a.text != b.text || a.kind != b.kind)
      if lastTokens.isEmpty then memo = Memo()
      else if firstChanged >= 0 then memo.invalidateFrom(firstChanged)
      else if toks.length != lastTokens.length then memo.invalidateFrom(math.min(toks.length, lastTokens.length) - 1)
      lastTokens = toks
      Parser.parseFull(g, source, recover = false, memo = memo).map(out => Result(out, out.steps - out.memoHits))
    }

/** M7: concrete (trivia-preserving) printing and span splicing. */
object Concrete:
  /** Byte-identical reconstruction of the parsed source from its tokens. */
  def printExact(source: String, out: ParseOut): String =
    val sb = StringBuilder()
    for t <- out.tokens do
      sb ++= t.leading
      if t.kind != TokKind.Eof then sb ++= source.substring(t.offset, t.offset + t.rawLen)
    sb.result()

  /** Replace the source region of `target` (a node instance from this parse)
    * with the canonical print of `replacement`; all other bytes preserved.
    * This is the grammar-as-lens `put` primitive — see [[RoundTrip.put]].
    */
  def splice(g: GrammarSpec, source: String, out: ParseOut, target: Cst, replacement: Cst): Either[String, String] =
    out.spans.get(target) match
      case None => Left("target node has no recorded span (not from this parse)")
      case Some((startTok, endTok)) =>
        Printer.print(g, replacement).map { printed =>
          val startOff = out.tokens(startTok).offset
          val endOff =
            if endTok == 0 then startOff
            else { val last = out.tokens(endTok - 1); last.offset + last.rawLen }
          source.substring(0, startOff) + printed + source.substring(endOff) }

  /** Alias for [[splice]] — the asymmetric lens `put: A × S → S` for a single
    * spanned subtree. Leading/trailing trivia and sibling text outside the
    * span are preserved byte-for-byte. General dirty-subtree re-association
    * (re-wrapping a parent whose children were each edited) is still absent.
    */
  def put(g: GrammarSpec, source: String, out: ParseOut, target: Cst, replacement: Cst): Either[String, String] =
    splice(g, source, out, target, replacement)

/** One generic printer (S11) interpreting the print table. */
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
                      case PrintSeg.StrField(i) =>
                        children.lift(i) match
                          case Some(Cst.Leaf(t)) =>
                            emit("\"" + t.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + "\"")
                            Right(())
                          case other => Left(s"print rule '$tag' strField $i is not a leaf: $other")
                      case PrintSeg.SepFields(from, sep) =>
                        // the field AT `from` if it is a list node, else the tail
                        val items = children.lift(from) match
                          case Some(Cst.Node("list", xs)) => xs
                          case _                          => children.drop(from)
                        items.zipWithIndex.foldLeft[Either[String, Unit]](Right(())) { case (a, (item, i)) =>
                          a.flatMap { _ =>
                            if i > 0 then { if sep.contains('\n') then newline() else emit(sep) }
                            go(item, maxPrec(g)) } }
                  }
                }

    def maxPrec(g: GrammarSpec): Int =
      (g.precCategories.flatMap(_.ops).map(_.prec) :+ 0).max + 1

    go(cst, 0).map(_ => sb.result())

/** Round-trip law suite hooks (S11, M31) and the grammar-as-lens `put` slice.
  *
  * `(parse, print)` alone is a **retraction/section pair over `Term`**:
  * [[Printer.print]] takes no source string, so it always regenerates
  * canonical text from scratch. Two laws are load-bearing for that pair; a
  * third asking for byte-exact reproduction of arbitrary hand-written input
  * (`print(parse(s)) == s`) does not apply — see `§2` "Bidirectional
  * grammar... modulo holes/whitespace policy".
  *
  *   1. **Retraction** ([[check]]): `parse(print(t)) == t` for every term —
  *      printing then reparsing never loses or alters structure.
  *   2. **Canonicalization idempotence** ([[fixpoint]]):
  *      `print(parse(print(parse(s)))) == print(parse(s))` for every parseable
  *      string `s` — `print∘parse` is a retraction onto a canonical-text
  *      fixpoint, so re-running it never moves the text again.
  *
  * **Lens `put` (present, smallest useful slice):** [[put]] / [[Concrete.put]]
  * / [[Concrete.splice]] edit one spanned subtree and preserve every byte
  * outside that span (leading/trailing trivia, siblings). Module-level ΔL
  * uses the same primitive via [[Delta.applyPreservingFormat]]. Still absent:
  * general dirty-subtree re-association, and format-preserving `remove` /
  * `rename` (rename needs leaf-name spans the parser does not yet record).
  */
object RoundTrip:
  /** Law 1: retraction (see object doc) — `parse(print(t)) == t`. */
  def check(g: GrammarSpec, term: Cst): Either[String, Unit] =
    for
      printed <- Printer.print(g, term)
      reparsed <- Parser.parse(g, printed).left.map(e => s"$e\n--- printed text was:\n$printed")
      _ <- if reparsed == term then Right(())
           else Left(s"round-trip mismatch:\n  original: ${term.render}\n  printed:  $printed\n  reparsed: ${reparsed.render}")
    yield ()

  /** Law 2: canonicalization idempotence (see object doc) — print∘parse∘print
    * byte fixpoint on emitted text (whole-file discipline).
    */
  def fixpoint(g: GrammarSpec, text: String): Either[String, Unit] =
    for
      t <- Parser.parse(g, text)
      p1 <- Printer.print(g, t)
      t2 <- Parser.parse(g, p1)
      p2 <- Printer.print(g, t2)
      _ <- if p1 == p2 then Right(()) else Left(s"print/parse fixpoint failed:\n--- p1:\n$p1\n--- p2:\n$p2")
    yield ()

  /** Format-preserving put: replace the spanned `target` (a node identity from
    * `out`, itself from parsing `source`) with the canonical print of
    * `replacement`. Bytes outside the span are unchanged.
    */
  def put(g: GrammarSpec, source: String, out: ParseOut, target: Cst, replacement: Cst): Either[String, String] =
    Concrete.put(g, source, out, target, replacement)
