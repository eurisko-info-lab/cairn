package cairn.examples.sds

/** Regulatory SDS section numbering (REACH Annex II / EU-CLP / GHS).
  *
  * Titles and legality come from the versioned [[EuClp]] profile
  * (`languages/eu-clp.cairn`, `languages/sds/profiles/eu-clp-annex-ii.cairn`)
  * — not a host title table. Ascending/unique remain structural checks.
  */
object SectionNumbering:
  final case class SectionDef(number: Int, title: String)

  /** Profile-backed EU-CLP / REACH Annex II 16-section shape. */
  lazy val euClp: List[SectionDef] =
    EuClp.annexIiSections.map((n, t) => SectionDef(n, t))

  lazy val numbers: List[Int] = euClp.map(_.number)
  def byNumber: Map[Int, String] = euClp.map(s => s.number -> s.title).toMap

  def isValidNumber(n: Int): Boolean = byNumber.contains(n)

  def parseNumber(raw: String): Either[String, Int] =
    raw.trim.toIntOption match
      case None => Left(s"section number '$raw' is not an integer")
      case Some(n) if !byNumber.contains(n) =>
        Left(s"section number $n out of range (expected 1..16)")
      case Some(n) => Right(n)

  def titleOf(n: Int): Either[String, String] =
    byNumber.get(n).toRight(s"section number $n out of range (expected 1..16)")

  /** A sparse outline entry: regulatory number + claimed title. */
  final case class OutlineEntry(number: Int, title: String)

  enum OutlineError:
    case BadNumber(raw: String, detail: String)
    case TitleMismatch(number: Int, claimed: String, expected: String)
    case Duplicate(number: Int)
    case OutOfOrder(numbers: List[Int])

  /** Validate a (possibly sparse) outline against EU-CLP numbering rules. */
  def validateOutline(entries: List[OutlineEntry]): Either[List[OutlineError], List[SectionDef]] =
    val errs = List.newBuilder[OutlineError]
    val seen = scala.collection.mutable.LinkedHashSet.empty[Int]
    for e <- entries do
      if !EuClp.checkSectionNumber(e.number.toString) then
        errs += OutlineError.BadNumber(e.number.toString, "fails sectionNumberOk / out of range")
      else
        val expectedTitle = byNumber.get(e.number)
        val titleOk = expectedTitle.exists { expected =>
          EuClp.checkSectionTitle(e.number.toString, expected) && e.title == expected
        }
        if !titleOk then
          errs += OutlineError.TitleMismatch(
            e.number, e.title, expectedTitle.getOrElse("?"))
        if !seen.add(e.number) then errs += OutlineError.Duplicate(e.number)
    val nums = entries.map(_.number)
    if nums != nums.sorted then errs += OutlineError.OutOfOrder(nums)
    val es = errs.result()
    if es.nonEmpty then Left(es)
    else Right(entries.map(e => SectionDef(e.number, byNumber(e.number))))

  /** Order an unordered bag of valid section numbers ascending (1..16). */
  def order(numbers: Iterable[Int]): Either[String, List[SectionDef]] =
    val uniq = numbers.toList.distinct
    val bad = uniq.filterNot(byNumber.contains)
    if bad.nonEmpty then Left(s"invalid section number(s): ${bad.mkString(", ")}")
    else Right(uniq.sorted.map(n => SectionDef(n, byNumber(n))))

  /** Acetone tutorial spine: sparse outline of sections the host language
    * objects actually speak to (hazards + composition).
    */
  def acetoneSparseOutline: List[OutlineEntry] = List(
    OutlineEntry(2, "Hazards identification"),
    OutlineEntry(3, "Composition/information on ingredients"))
