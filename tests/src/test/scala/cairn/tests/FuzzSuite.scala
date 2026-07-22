package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc

/** M50: grammar-driven fuzzing. ONE generic generator walks any GrammarSpec
  * producing random valid trees; every tree must satisfy the round-trip law.
  * 10^5 terms across the shipped grammars, zero failures.
  */
object GrammarFuzz:
  def gen(g: GrammarSpec, rnd: scala.util.Random, cat: String, depth: Int): Option[Cst] =
    g.precCategory(cat) match
      case Some(p) =>
        gen(g, rnd, p.base, depth).map { base =>
          if depth > 0 && rnd.nextInt(3) == 0 && p.ops.nonEmpty then
            val op = p.ops(rnd.nextInt(p.ops.length))
            gen(g, rnd, p.base, depth - 1) match
              case Some(rhs) => Cst.Node(op.tag, List(base, rhs))
              case None      => base
          else base
        }
      case None =>
        g.category(cat).flatMap { c =>
          val ctors = c.ctors.filterNot(_.tag == "$group")
          val usable =
            if depth <= 0 then
              val terminal = ctors.filter(k => !k.elems.exists(hasRecursion))
              if terminal.nonEmpty then terminal else ctors
            else ctors
          if usable.isEmpty then None
          else
            val ctor = usable(rnd.nextInt(usable.length))
            genFields(g, rnd, ctor.elems, depth).map(fs => Cst.Node(ctor.tag, fs))
        }

  private def hasRecursion(e: Elem): Boolean = e match
    case Elem.Cat(_) | Elem.Block(_) | Elem.Run(_) => true
    case Elem.Opt(i)       => hasRecursion(i)
    case Elem.Star(i)      => hasRecursion(i)
    case Elem.SepBy1(i, _) => hasRecursion(i)
    case Elem.Adjacent1(i) => hasRecursion(i)
    case _                 => false

  private def ident(rnd: scala.util.Random): String = s"zz${rnd.nextInt(1000)}"

  private def genFields(g: GrammarSpec, rnd: scala.util.Random, elems: List[Elem], depth: Int): Option[List[Cst]] =
    elems.foldLeft[Option[List[Cst]]](Some(Nil)) { (acc, e) =>
      acc.flatMap(fs => genElem(g, rnd, e, depth).map(fs ++ _))
    }

  private def genElem(g: GrammarSpec, rnd: scala.util.Random, e: Elem, depth: Int): Option[List[Cst]] = e match
    case Elem.Tok(_)      => Some(Nil)
    case Elem.TokField(t) => Some(List(Cst.Leaf(t)))
    case Elem.NameLeaf    => Some(List(Cst.Leaf(ident(rnd))))
    case Elem.AnyIdentLeaf => Some(List(Cst.Leaf(
      if rnd.nextBoolean() && g.tokens.keywords.nonEmpty then g.tokens.keywords(rnd.nextInt(g.tokens.keywords.length))
      else ident(rnd))))
    case Elem.NumLeaf     => Some(List(Cst.Leaf(rnd.nextInt(1000).toString)))
    case Elem.StrLeaf     => Some(List(Cst.Leaf(
      List.fill(rnd.nextInt(6))(rnd.alphanumeric.head).mkString + (if rnd.nextInt(4) == 0 then "\"\\ " else ""))))
    case Elem.RestOfLine  => Some(List(Cst.Leaf(" fuzz " + ident(rnd))))
    case Elem.Cat(n)      => gen(g, rnd, n, depth - 1).map(List(_))
    case Elem.Opt(inner)  =>
      if rnd.nextBoolean() then Some(List(Cst.node("none")))
      else genElem(g, rnd, inner, depth).map(fs => List(Cst.node("some", fs*)))
    case Elem.Star(inner) =>
      val n = rnd.nextInt(3)
      (0 until n).foldLeft[Option[List[Cst]]](Some(Nil)) { (acc, _) =>
        acc.flatMap(items => genElem(g, rnd, inner, depth).map(fs =>
          items ++ List(fs match { case List(one) => one; case many => Cst.Node("group", many) })))
      }.map(items => List(Cst.Node("list", items)))
    case Elem.SepBy1(inner, _) =>
      val n = 1 + rnd.nextInt(2)
      (0 until n).foldLeft[Option[List[Cst]]](Some(Nil)) { (acc, _) =>
        acc.flatMap(items => genElem(g, rnd, inner, depth).map(fs =>
          items ++ List(fs match { case List(one) => one; case many => Cst.Node("group", many) })))
      }.map(items => List(Cst.Node("list", items)))
    case Elem.Block(_) | Elem.Run(_) | Elem.Adjacent1(_) => None // layout: not fuzzed

class FuzzSuite extends munit.FunSuite:
  private val packs = cairn.runtime.PackLoader(cairn.systemhandler.EffectContext.forPackLoader())
  private val Pki = cairn.examples.pki.Pki(packs)
  private val Law = cairn.examples.law.Law(packs)
  private val Sds = cairn.examples.sds.Sds(packs)
  private val Riemann = cairn.examples.riemann.Riemann(packs)
  private val Search = cairn.examples.search.Search(packs)
  override def munitTimeout = scala.concurrent.duration.Duration(600, "s")

  private def fuzz(g0: GrammarSpec, top: String, count: Int, seed: Long, depth: Int): Int =
    val g = if g0.top == top then g0 else g0.copy(top = top)
    val rnd = new scala.util.Random(seed)
    var generated = 0
    var attempts = 0
    while generated < count && attempts < count * 4 do
      attempts += 1
      GrammarFuzz.gen(g, rnd, top, depth) match
        case Some(t) =>
          generated += 1
          RoundTrip.check(g, t) match
            case Right(()) => ()
            case Left(err) => fail(s"[${g.name}] seed=$seed n=$generated\n$err")
        case None => ()
    generated

  /** Law 2 (RoundTrip.fixpoint, see its object doc): canonicalization
    * idempotence, string-first. `fuzz` above only checks Law 1 (retraction,
    * term-first: parse(print(t)) == t) — this checks the complementary law
    * starting from text, the shape an arbitrary hand-written or foreign
    * input actually takes: print(parse(print(parse(s)))) == print(parse(s)).
    */
  private def fuzzFixpoint(g0: GrammarSpec, top: String, count: Int, seed: Long, depth: Int): Int =
    val g = if g0.top == top then g0 else g0.copy(top = top)
    val rnd = new scala.util.Random(seed)
    var generated = 0
    var attempts = 0
    while generated < count && attempts < count * 4 do
      attempts += 1
      GrammarFuzz.gen(g, rnd, top, depth) match
        case Some(t) =>
          generated += 1
          val text = Printer.print(g, t).fold(e => fail(s"[${g.name}] seed=$seed n=$generated: $e"), identity)
          RoundTrip.fixpoint(g, text) match
            case Right(()) => ()
            case Left(err) => fail(s"[${g.name}] seed=$seed n=$generated\n$err")
        case None => ()
    generated

  test("M50: 10^5 fuzzed terms round-trip with zero failures"):
    var total = 0
    total += fuzz(Stlc.language.grammar, "term", 60_000, seed = 1L, depth = 4)
    total += fuzz(Stlc.language.grammar, "type", 10_000, seed = 2L, depth = 5)
    total += fuzz(Delta.deltaOf(Stlc.language).toOption.get.grammar, "Δstlc.changeset", 10_000, seed = 3L, depth = 4)
    total += fuzz(Query.language.grammar, "query", 5_000, seed = 4L, depth = 4)
    total += fuzz(cairn.user.policy.PolicyLang.language.grammar, "policyTerm", 5_000, seed = 5L, depth = 3)
    total += fuzz(JsonSurface.grammar, "json", 10_000, seed = 6L, depth = 4)
    assert(total >= 100_000, s"only $total terms generated")

  test("M50: exemplar packs (PKI/Law/SDS/Riemann/Search) fuzz round-trip clean"):
    // These packs only ever had hand-written golden-example round-trips before
    // this test; nothing had exercised them under random generation.
    var total = 0
    total += fuzz(Pki.language.grammar, "registryItem", 5_000, seed = 10L, depth = 3)
    total += fuzz(Law.language.grammar, "lawObj", 5_000, seed = 11L, depth = 3)
    total += fuzz(Sds.language.grammar, "sdsObj", 5_000, seed = 12L, depth = 3)
    total += fuzz(Riemann.language.grammar, "prop", 5_000, seed = 13L, depth = 4)
    total += fuzz(Riemann.LeanPort.grammar, "prop", 5_000, seed = 14L, depth = 4)
    total += fuzz(Search.language.grammar, "searchObj", 5_000, seed = 15L, depth = 3)
    assert(total >= 25_000, s"only $total terms generated")

  test("Law 2: canonicalization idempotence holds string-first across every shipped grammar"):
    // Complements the term-first `fuzz` tests above with the other law that
    // actually applies to this (parse, print) retraction pair (see
    // RoundTrip's object doc) — starting from text, not from a term.
    var total = 0
    total += fuzzFixpoint(Stlc.language.grammar, "term", 5_000, seed = 20L, depth = 4)
    total += fuzzFixpoint(Pki.language.grammar, "registryItem", 2_000, seed = 21L, depth = 3)
    total += fuzzFixpoint(Law.language.grammar, "lawObj", 2_000, seed = 22L, depth = 3)
    total += fuzzFixpoint(Sds.language.grammar, "sdsObj", 2_000, seed = 23L, depth = 3)
    total += fuzzFixpoint(Riemann.language.grammar, "prop", 2_000, seed = 24L, depth = 4)
    total += fuzzFixpoint(Riemann.LeanPort.grammar, "prop", 2_000, seed = 25L, depth = 4)
    total += fuzzFixpoint(Search.language.grammar, "searchObj", 2_000, seed = 26L, depth = 3)
    total += fuzzFixpoint(Query.language.grammar, "query", 2_000, seed = 27L, depth = 4)
    total += fuzzFixpoint(cairn.user.policy.PolicyLang.language.grammar, "policyTerm", 2_000, seed = 28L, depth = 3)
    assert(total >= 15_000, s"only $total terms generated")

  test("M50: fuzzed JSON also survives the value-level encode/decode"):
    val rnd = new scala.util.Random(7L)
    for _ <- 1 to 2000 do
      GrammarFuzz.gen(Stlc.language.grammar, rnd, "term", 4).foreach { t =>
        assertEquals(JsonSurface.decode(JsonSurface.encode(t).toOption.get), Right(t))
      }
