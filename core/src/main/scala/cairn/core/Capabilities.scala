package cairn.core

import cairn.kernel.*

/** M43: the §2b capability bundle as a first-class, lintable manifest.
  *
  * Every row is one of:
  *   - [[Row.Present]] — digest of a real CAS/artifact key (grammar, language, …)
  *   - [[Row.PlatformProvided]] — host platform capability; version digest is
  *     NOT a resolvable CAS object
  *   - [[Row.Deferred]] — explicit absence with a reason
  * Never silently absent; never mark a symbolic marker as [[Row.Present]].
  */
object Capabilities:
  val requiredRows: List[String] = List(
    "grammar", "surfaces", "interpreters", "changes", "projections", "judgments",
    "obligations", "traces", "migrations", "queries", "laws", "provenance",
    "trust", "effects", "workflows", "import-export")

  enum Row:
    /** Digest addresses a real artifact / language / grammar body. */
    case Present(digest: Digest)
    /** Platform mechanism available for this language; `version` fingerprints
      * the capability id + language, but is not a CAS get-key. */
    case PlatformProvided(capabilityId: String, version: Digest)
    case Deferred(note: String)

  final case class Manifest(language: Digest, rows: Map[String, Row]):
    def canon: Canon = Canon.cmap(
      "language" -> Canon.CStr(language.hex),
      "rows" -> Canon.CMap(rows.toList.sortBy(_._1).map((k, v) => k -> (v match
        case Row.Present(d)                    => Canon.CTag("present", Canon.CStr(d.hex))
        case Row.PlatformProvided(id, version) =>
          Canon.CTag("platform", Canon.cmap(
            "id" -> Canon.CStr(id), "version" -> Canon.CStr(version.hex)))
        case Row.Deferred(n)                   => Canon.CTag("deferred", Canon.CStr(n))))))
    def artifact: Artifact = Artifact(ArtifactKind.Capability, canon)
    def render: String =
      (s"capabilities of ${language.short}:" ::
        rows.toList.sortBy(_._1).map((k, v) => v match
          case Row.Present(d)                    => f"  $k%-14s ${d.short}"
          case Row.PlatformProvided(id, version) => f"  $k%-14s platform:$id (${version.short})"
          case Row.Deferred(n)                   => f"  $k%-14s deferred: $n")).mkString("\n")

  /** Rows every language gets automatically from the platform. */
  def auto(l: ComposedLanguage, surfaceDigests: Map[String, Digest] = Map.empty): Map[String, Row] =
    val deltaDigest = Delta.deltaOf(l).toOption.map(_.digest)
    def platform(id: String): Row.PlatformProvided =
      Row.PlatformProvided(id, Digest.of(Canon.cmap(
        "capability" -> Canon.CStr(id), "language" -> Canon.CStr(l.digest.hex))))
    val surfaceRow =
      if surfaceDigests.nonEmpty then
        Row.Present(Digest.of(Canon.CMap(surfaceDigests.toList.sortBy(_._1).map((n, d) =>
          n -> Canon.CStr(d.hex)))))
      else
        // encodings only (text/json/canon) when no named syntax surfaces are registered
        Row.Present(Digest.of(Canon.cstrs(Surfaces.forLanguage(l).keys.toList.sorted)))
    Map(
      "grammar" -> Row.Present(GrammarSpec.artifact(l.grammar).digest),
      "surfaces" -> surfaceRow,
      "interpreters" -> (if l.rewriteRules.nonEmpty then Row.Present(l.digest) else Row.Deferred("no rewrite rules")),
      "changes" -> deltaDigest.fold(Row.Deferred("ΔL underivable"))(Row.Present.apply),
      "judgments" -> (if l.judgments.nonEmpty then
        Row.Present(Digest.of(Canon.cstrs(l.judgments.keys.toList.sorted))) else Row.Deferred("none declared")),
      // generic platform mechanisms — not CAS-resolvable artifact keys:
      "traces" -> (if l.rewriteRules.nonEmpty then platform("eval-trace") // core.TreeEngine.normalizeTraced + kernel.TraceChecker
                   else Row.Deferred("no rewrite rules to trace")),
      "migrations" -> platform("lang-migration"),   // Migrate.module/changeset
      "queries" -> Row.Present(Query.language.digest),          // Query.run over any module
      "laws" -> platform("parse-print-roundtrip"),   // RoundTrip.check law suite
      "provenance" -> platform("provenance-record"), // ledger Provenance records
      "obligations" -> platform("claims-certificates"), // proof-layer claims/certs (override via extra with a real Claim digest)
      // genuinely per-language, not yet uniform:
      "projections" -> Row.Deferred("rosetta ports project rosetta artifacts; net lowering is per-pack"),
      "trust" -> Row.Deferred("policies gate ledger branches, not languages"),
      "effects" -> Row.Deferred("effect vocabulary exists only in rosetta modules"),
      "workflows" -> Row.Deferred("transcripts are global, not language-scoped"),
      "import-export" -> Row.Present(Digest.of(Canon.cstrs(List("text", "json", "canon")))))

  def build(l: ComposedLanguage, extra: Map[String, Row] = Map.empty,
            surfaceDigests: Map[String, Digest] = Map.empty): Either[String, Manifest] =
    val rows = requiredRows.map(r =>
      r -> (extra.get(r).orElse(auto(l, surfaceDigests).get(r)).getOrElse(Row.Deferred("not yet modeled")))).toMap
    lint(Manifest(l.digest, rows)).map(_ => Manifest(l.digest, rows))

  /** Lint (M43 AC): every required row is Present, PlatformProvided, or Deferred. */
  def lint(m: Manifest): Either[String, Unit] =
    val missing = requiredRows.filterNot(m.rows.contains)
    val unknown = m.rows.keys.filterNot(requiredRows.contains).toList
    if missing.nonEmpty then Left(s"capability manifest missing rows: ${missing.mkString(", ")}")
    else if unknown.nonEmpty then Left(s"capability manifest has undeclared rows: ${unknown.mkString(", ")}")
    else Right(())

/** M45: the query language — patterns + kind predicates over modules and the
  * CAS, itself a grammar-engine language (so ΔQuery exists via the closure).
  *
  * Surface:
  *   defs matching <pat>
  *   defs typed <pat>           (via derivation search when a checker cfg is given)
  *   artifacts kind <name>
  */
object Query:
  val fragment: Fragment = Fragment(
    name = "query",
    provides = List("query"),
    requires = Nil,
    sorts = List(SortDef("Query", SortMode.Tree)),
    constructors = List(
      CtorDef("qMatch", "Query", List("Pat")),
      CtorDef("qTyped", "Query", List("Pat")),
      CtorDef("qKind", "Query", List("Name"))),
    grammar = GrammarPart(
      keywords = List("defs", "matching", "typed", "artifacts", "kind"),
      puncts = List("(", ")", ",", "$"),
      identContExtra = "-",
      categories = List(
        CategorySpec("query", List(
          ConstructorSpec("qMatch", List(Elem.Tok("defs"), Elem.Tok("matching"), Elem.Cat("qpat"))),
          ConstructorSpec("qTyped", List(Elem.Tok("defs"), Elem.Tok("typed"), Elem.Cat("qpat"))),
          ConstructorSpec("qKind", List(Elem.Tok("artifacts"), Elem.Tok("kind"), Elem.AnyIdentLeaf)))),
        CategorySpec("qpat", List(
          ConstructorSpec("qpMetaNode", List(
            Elem.Tok("$"), Elem.NameLeaf, Elem.Tok("("), Elem.Opt(Elem.SepBy1(Elem.Cat("qpat"), ",")), Elem.Tok(")"))),
          ConstructorSpec("qpMeta", List(Elem.Tok("$"), Elem.NameLeaf)),
          ConstructorSpec("qpNode", List(
            Elem.NameLeaf, Elem.Tok("("), Elem.Opt(Elem.SepBy1(Elem.Cat("qpat"), ",")), Elem.Tok(")"))),
          ConstructorSpec("qpLeaf", List(Elem.NameLeaf))))),
      printRules = List(
        PrintRule("qMatch", List(PrintSeg.Lit("defs matching"), PrintSeg.Space, PrintSeg.Field(0))),
        PrintRule("qTyped", List(PrintSeg.Lit("defs typed"), PrintSeg.Space, PrintSeg.Field(0))),
        PrintRule("qKind", List(PrintSeg.Lit("artifacts kind"), PrintSeg.Space, PrintSeg.Field(0))),
        PrintRule("qpMetaNode", List(PrintSeg.Lit("$"), PrintSeg.Field(0), PrintSeg.Lit("("), PrintSeg.Field(1), PrintSeg.Lit(")"))),
        PrintRule("qpMeta", List(PrintSeg.Lit("$"), PrintSeg.Field(0))),
        PrintRule("qpNode", List(PrintSeg.Field(0), PrintSeg.Lit("("), PrintSeg.Field(1), PrintSeg.Lit(")"))),
        PrintRule("qpLeaf", List(PrintSeg.Field(0)))),
      top = Some("query")))

  lazy val language: ComposedLanguage =
    Compose.compose("query", List(fragment)).fold(
      e => throw RuntimeException(e.map(_.render).mkString("\n")), identity)

  def parse(src: String): Either[String, Cst] = Parser.parse(language.grammar, src)

  private def qpatToPat(p: Cst): Either[String, Cst] = p match
    case Cst.Node("qpMeta", List(Cst.Leaf(x))) => Right(Cst.Leaf(s"$$$x"))
    case Cst.Node("qpLeaf", List(Cst.Leaf(x))) => Right(Cst.Leaf(x))
    case Cst.Node("qpNode", List(Cst.Leaf(t), args)) =>
      seqE(items(args).map(qpatToPat)).map(Cst.Node(t, _))
    case Cst.Node("qpMetaNode", List(Cst.Leaf(t), args)) =>
      seqE(items(args).map(qpatToPat)).map(Cst.Node(s"$$$t", _))
    case other => Left(s"not a query pattern: ${other.render}")

  private def items(c: Cst): List[Cst] = c match
    case Cst.Node("some", List(Cst.Node("list", xs))) => xs
    case Cst.Node("some", List(x))                    => List(x)
    case _                                            => Nil
  private def seqE[A](xs: List[Either[String, A]]): Either[String, List[A]] =
    xs.foldLeft[Either[String, List[A]]](Right(Nil)) { (acc, x) => for as <- acc; a <- x yield as :+ a }

  private def matches(pat: Cst, t: Cst): Boolean = (pat, t) match
    case (Cst.Leaf(p), _) if p.startsWith("$")                   => true
    case (Cst.Leaf(p), Cst.Leaf(a))                              => p == a
    case (Cst.Node(pc, ps), Cst.Node(ac, as)) if pc == ac && ps.length == as.length =>
      ps.zip(as).forall(matches)
    case _ => false

  /** Match anywhere in the term (subterm search). */
  private def matchesAnywhere(pat: Cst, t: Cst): Boolean =
    matches(pat, t) || (t match
      case Cst.Node(_, cs) => cs.exists(matchesAnywhere(pat, _))
      case _               => false)

  final case class Result(hits: List[(String, Digest)]):
    def canon: Canon = Canon.CList(hits.map((n, d) => Canon.cmap(
      "name" -> Canon.CStr(n), "digest" -> Canon.CStr(d.hex))))
    def artifact: Artifact = Artifact(ArtifactKind.QueryResult, canon)

  /** Execute a query against a module (+ optional preloaded artifacts for kind
    * queries, + optional type-inference callback for `defs typed`). Kind queries
    * take already-decoded [[Artifact]]s — CAS/filesystem walking is a System
    * Handler concern; callers that used to pass a CAS root load artifacts first.
    */
  def run(query: Cst, module: Module,
          artifacts: Option[List[Artifact]] = None,
          typeOf: Option[Cst => Either[String, Cst]] = None): Either[String, Result] =
    query match
      case Cst.Node("qMatch", List(qp)) =>
        qpatToPat(qp).map { pat =>
          Result(module.defs.filter((_, t) => matchesAnywhere(pat, t))
            .map((n, t) => (n, Artifact(ArtifactKind.Term, Cst.toCanon(t)).digest))) }
      case Cst.Node("qTyped", List(qp)) =>
        for
          pat <- qpatToPat(qp)
          infer <- typeOf.toRight("`defs typed` needs a type-inference callback")
        yield Result(module.defs.flatMap { (n, t) =>
          infer(t).toOption.filter(ty => matches(pat, ty))
            .map(_ => (n, Artifact(ArtifactKind.Term, Cst.toCanon(t)).digest)) })
      case Cst.Node("qKind", List(Cst.Leaf(kind))) =>
        artifacts.toRight("`artifacts kind` needs a preloaded artifact list").map { arts =>
          val hits = arts.filter(_.kind.name == kind).map(a => (a.kind.name, a.digest))
          Result(hits.sortBy(_._2.hex)) }
      case other => Left(s"not a query: ${other.render}")
