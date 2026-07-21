package cairn.core

import cairn.kernel.*

/** Export Cairn STLC λ-terms / AffineNet·IcNet fixtures to an HVM2-lineage
  * surface for agreement certificates (`docs/agreement.md`).
  *
  * Surface dialect (HVM2 tree IR, not Bend / HVM5 / full ABI):
  * {{{
  *   (a b)   CON / γ (fan)     — λ and application spines
  *   {a b}   DUP / δ           — sharing
  *   *       ERA / ε
  *   @Name   REF
  *   & t ~ u active pair
  * }}}
  *
  * Boolean constants are Church-affine refs (`@True` / `@False`), not Cairn's
  * opaque labelled `konst` agents — the envelope maps them back for digests.
  * This is a **projection**, not a claim of HVM memory layout or CLI wire
  * compatibility beyond the tiny agreement corpus.
  */
object HvmSurface:

  /** Classical IC λ-surface (lineage text): `(λx. body)`, `(f x)`, `true`/`false`. */
  def icLambda(term: Cst): Either[String, String] =
    def go(t: Cst): Either[String, String] = t match
      case Cst.Node("var", List(Cst.Leaf(x))) => Right(x)
      case Cst.Node("lam", List(Cst.Leaf(x), _, body)) =>
        go(body).map(b => s"(λ$x. $b)")
      case Cst.Node("app", List(f, a)) =>
        for
          fs <- go(f)
          as <- go(a)
        yield s"($fs $as)"
      case Cst.Node("true", _)  => Right("true")
      case Cst.Node("false", _) => Right("false")
      case other => Left(s"icLambda: unsupported ${other.render}")
    go(term)

  /** HVM2 book that deposits the NF of a closed STLC λ-term on `@main`. */
  def bookFromLambda(term: Cst): Either[String, String] =
    val fresh = Iterator.from(1).map(i => s"w$i")
    val r = "r"
    deposit(term, r, fresh).map { redexes =>
      val body = redexes.map(rx => s"  & $rx").mkString("\n")
      s"""@True = (t (* t))
         |@False = (* (f f))
         |@main = $r
         |$body
         |""".stripMargin
    }

  /** HVM2 book for the AffineNet era-fan agreement fixture (empty NF). */
  def bookEraFan: String =
    """@main = *
      |  & * ~ ((1 (2 *)) *)
      |""".stripMargin

  /** Digest of an exported surface (for certificate evidence / goldens). */
  def exportDigest(kind: String, text: String): Digest =
    Digest.of(Canon.cmap("kind" -> Canon.CStr(kind), "text" -> Canon.CStr(text)))

  /** Parse `hvm run` stdout → normalized result token (`*`, `@True`, …). */
  def readResult(stdout: String): Either[String, String] =
    stdout.linesIterator.map(_.trim).find(_.startsWith("Result:")) match
      case None => Left("hvm: no Result: line")
      case Some(line) =>
        val tok = line.stripPrefix("Result:").trim
        if tok.isEmpty then Left("hvm: empty Result") else Right(tok)

  /** Coarse acceptance of an HVM2 result against a named corpus expectation. */
  def accepts(expectation: String, result: String): Boolean = expectation match
    case "true"  => result == "@True" || result == "true"
    case "false" => result == "@False" || result == "false"
    case "era"   => result == "*"
    case "id"    =>
      result == "@id" || result.matches("""\(v\d+ v\d+\)""") ||
        result.matches("""\(([a-zA-Z_]\w*) \1\)""")
    case other   => result == other

  // ---- internals ------------------------------------------------------------

  private def countUses(t: Cst, x: String): Int = t match
    case Cst.Node("var", List(Cst.Leaf(y))) => if x == y then 1 else 0
    case Cst.Node("lam", List(Cst.Leaf(y), _, body)) =>
      if x == y then 0 else countUses(body, x)
    case Cst.Node(_, cs) => cs.map(countUses(_, x)).sum
    case _ => 0

  /** Emit redexes so evaluating `term` deposits its NF on wire `out`. */
  private def deposit(
      term: Cst, out: String, fresh: Iterator[String]
  ): Either[String, List[String]] = term match
    case Cst.Node("true", _)  => Right(List(s"@True ~ $out"))
    case Cst.Node("false", _) => Right(List(s"@False ~ $out"))
    case Cst.Node("var", List(Cst.Leaf(x))) => Right(List(s"$x ~ $out"))
    case Cst.Node("lam", List(Cst.Leaf(x), _, body)) =>
      lamTree(x, body, fresh).map { (tree, rx) => rx :+ s"$tree ~ $out" }
    case Cst.Node("app", List(f, a)) =>
      val fW = fresh.next(); val aW = fresh.next()
      for
        rf <- deposit(f, fW, fresh)
        ra <- deposit(a, aW, fresh)
      yield rf ++ ra :+ s"$fW ~ ($aW $out)"
    case other => Left(s"bookFromLambda: unsupported ${other.render}")

  /** Encode `λx. body` as an HVM2 value tree plus any body redexes. */
  private def lamTree(
      x: String, body: Cst, fresh: Iterator[String]
  ): Either[String, (String, List[String])] =
    val uses = countUses(body, x)
    if uses == 0 then
      val bW = fresh.next()
      deposit(body, bW, fresh).map(rx => (s"(* $bW)", rx))
    else if uses == 1 then
      val bW = fresh.next()
      deposit(body, bW, fresh).map(rx => (s"($x $bW)", rx))
    else
      // δ-tree: uses-1 duplicators off the binder, then deposit body with
      // fresh names for each occurrence (rewritten env).
      val ports = scala.collection.mutable.Queue[String]()
      var feed = x
      val dupRx = List.newBuilder[String]
      for _ <- 1 until uses do
        val p = fresh.next(); val q = fresh.next()
        dupRx += s"{$p $q} ~ $feed"
        ports += p
        feed = q
      ports += feed
      val renamed = renameUses(body, x, () => ports.dequeue())
      val bW = fresh.next()
      deposit(renamed, bW, fresh).map(rx => (s"($x $bW)", dupRx.result() ++ rx))

  /** Replace successive uses of `x` with successive ports from `next`. */
  private def renameUses(t: Cst, x: String, next: () => String): Cst = t match
    case Cst.Node("var", List(Cst.Leaf(y))) if y == x =>
      Cst.node("var", Cst.Leaf(next()))
    case Cst.Node("lam", List(Cst.Leaf(y), ty, body)) =>
      if y == x then t
      else Cst.node("lam", Cst.Leaf(y), ty, renameUses(body, x, next))
    case Cst.Node(k, cs) => Cst.Node(k, cs.map(renameUses(_, x, next)))
    case other => other
