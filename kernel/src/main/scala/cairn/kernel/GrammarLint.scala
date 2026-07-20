package cairn.kernel

/** M9: static grammar analysis beyond left recursion — run automatically on
  * every composed grammar (see [[Compose.compose]]). Errors fail composition;
  * warnings surface to tooling.
  */
object GrammarLint:
  enum Severity { case Error, Warn }
  final case class Issue(severity: Severity, where: String, msg: String):
    def render: String = s"[$severity] $where: $msg"

  /** Number of Cst fields an elem sequence yields (Tok is discarded). */
  def fieldCount(elems: List[Elem]): Int = elems.count {
    case Elem.Tok(_) => false
    case _           => true
  }

  def check(g: GrammarSpec): List[Issue] =
    val issues = List.newBuilder[Issue]
    val infixTags = g.precCategories.flatMap(_.ops).map(_.tag).toSet
    val knownCats = (g.categories.map(_.name) ++ g.precCategories.map(_.name)).toSet

    for cat <- g.categories do
      // duplicate alternatives (identical elems) => later unreachable
      val seen = scala.collection.mutable.Map[List[Elem], String]()
      for c <- cat.ctors do
        seen.get(c.elems) match
          case Some(prev) => issues += Issue(Severity.Error, s"${cat.name}/${c.tag}",
            s"identical to earlier alternative '$prev' — unreachable")
          case None => seen(c.elems) = c.tag
      // prefix shadowing (skill checklist #7): an EARLIER alternative that is a
      // proper prefix of a later one makes the later one unreachable
      for
        (a, i) <- cat.ctors.zipWithIndex
        (b, j) <- cat.ctors.zipWithIndex if i < j
        if a.elems.nonEmpty && b.elems.startsWith(a.elems) && b.elems.length > a.elems.length
      do issues += Issue(Severity.Error, s"${cat.name}/${b.tag}",
        s"shadowed by earlier prefix alternative '${a.tag}' — order the longer/specific alternative first")
      // unknown category references
      def cats(e: Elem): List[String] = e match
        case Elem.Cat(n) => List(n)
        case Elem.Opt(x) => cats(x)
        case Elem.Star(x) => cats(x)
        case Elem.SepBy1(x, _) => cats(x)
        case Elem.Adjacent1(x) => cats(x)
        case Elem.Block(n) => List(n)
        case Elem.Run(n) => List(n)
        case _ => Nil
      for c <- cat.ctors; n <- c.elems.flatMap(cats) if !knownCats.contains(n) do
        issues += Issue(Severity.Error, s"${cat.name}/${c.tag}", s"references unknown category '$n'")

    // print/parse agreement: every parse alternative needs a print rule with
    // in-range field references; duplicate Field refs are warnings (skill #4)
    val allCtors = g.categories.flatMap(_.ctors)
    for c <- allCtors if c.tag != "$group" && !infixTags.contains(c.tag) do
      val n = fieldCount(c.elems)
      g.printRule(c.tag) match
        case None => issues += Issue(Severity.Error, c.tag, "no print rule for parse alternative")
        case Some(rule) =>
          val refs = rule.segs.collect {
            case PrintSeg.Field(i) => i
            case PrintSeg.StrField(i) => i }
          for i <- refs if i >= n do
            issues += Issue(Severity.Error, c.tag, s"print rule references field $i but parse yields only $n fields")
          for (i, count) <- refs.groupBy(identity).view.mapValues(_.size) if count > 1 do
            issues += Issue(Severity.Warn, c.tag, s"field $i referenced $count times in print rule (skill checklist #4)")
    issues.result()

  def errors(g: GrammarSpec): List[Issue] = check(g).filter(_.severity == Severity.Error)
