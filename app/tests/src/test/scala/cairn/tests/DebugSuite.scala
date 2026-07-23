package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc

class DebugSuite extends munit.FunSuite:
  test("debug offside"):
    val g = new WaveBSuite().blockGrammar
    val src = "let\n  a = x\n  b = y\nin a"
    Parser.parse(g, src) match
      case Right(cst) =>
        println(s"PARSED: ${cst.render}")
        Printer.print(g, cst) match
          case Right(p) => println(s"PRINTED:\n$p")
            Parser.parse(g, p) match
              case Right(c2) => println(s"REPARSED: ${c2.render}")
              case Left(e) => println(s"REPARSE FAIL: $e")
          case Left(e) => println(s"PRINT FAIL: $e")
      case Left(e) => println(s"PARSE FAIL: $e")

  test("debug incremental"):
    val g = Stlc.language.grammar
    val src = (1 to 5).map(i => s"fun v$i : Bool . ").mkString + "true"
    val src2 = src.replace("true", "false")
    val memo = Memo()
    val r1 = Parser.parseFull(g, src, memo = memo).toOption.get
    println(s"r1 tail=${r1.cst.render.takeRight(30)} steps=${r1.steps}")
    println(s"table keys: ${memo.table.keys.toList.sortBy(_._2)}")
    println(s"values: ${memo.table.toList.sortBy(_._1._2).map((k, v) => k -> v.map((c, e) => e))}")
    val toks1 = Lexer.lex(g.tokens, src).toOption.get
    val toks2 = Lexer.lex(g.tokens, src2).toOption.get
    val firstChanged = toks1.zip(toks2).indexWhere((a, b) => a.text != b.text || a.kind != b.kind)
    println(s"firstChanged=$firstChanged of ${toks1.length}")
    memo.invalidateFrom(firstChanged)
    println(s"after invalidate: ${memo.table.keys.toList.sortBy(_._2)}")
    val r2 = Parser.parseFull(g, src2, memo = memo).toOption.get
    println(s"r2 tail=${r2.cst.render.takeRight(30)} steps=${r2.steps} hits=${r2.memoHits}")
