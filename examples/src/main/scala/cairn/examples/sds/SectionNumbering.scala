package cairn.examples.sds

/** Regulatory SDS section numbering (REACH Annex II / EU-CLP / GHS).
  *
  * Prefer the versioned [[EuClp]] profile language + annex-II instance module
  * (`languages/eu-clp.cairn`, `languages/sds/profiles/eu-clp-annex-ii.cairn`).
  * The host [[euClp]] list is the fallback mirror when the pack is unavailable.
  * Numbers must be in 1..16; titles must match the canonical heading; sparse
  * outlines are ascending, unique, and title-matched (gaps allowed).
  *
  * Section-number legality is also a Cairn judgment (`sectionNumberOk` on
  * eu-clp). Phrase + section-field staleness materialize via derived ΔSDS
  * ([[PhraseStaleness.deriveEnRewrite]] / [[SectionFieldStaleness.deriveEnRewrite]]
  * + `translationState` / `sectionFieldState`). Full module conformance is
  * [[EuClp.conform]] — not Studio.
  */
object SectionNumbering:
  final case class SectionDef(number: Int, title: String)

  /** Official EU-CLP / REACH Annex II 16-section shape. Prefer [[EuClp.annexIiSections]]. */
  val euClpFallback: List[SectionDef] = List(
    SectionDef(1, "Identification"),
    SectionDef(2, "Hazards identification"),
    SectionDef(3, "Composition/information on ingredients"),
    SectionDef(4, "First-aid measures"),
    SectionDef(5, "Firefighting measures"),
    SectionDef(6, "Accidental release measures"),
    SectionDef(7, "Handling and storage"),
    SectionDef(8, "Exposure controls/personal protection"),
    SectionDef(9, "Physical and chemical properties"),
    SectionDef(10, "Stability and reactivity"),
    SectionDef(11, "Toxicological information"),
    SectionDef(12, "Ecological information"),
    SectionDef(13, "Disposal considerations"),
    SectionDef(14, "Transport information"),
    SectionDef(15, "Regulatory information"),
    SectionDef(16, "Other information"))

  /** Profile-backed titles when the eu-clp pack loads; else host fallback. */
  lazy val euClp: List[SectionDef] =
    try EuClp.annexIiSections.map((n, t) => SectionDef(n, t))
    catch case _: Throwable => euClpFallback

  val numbers: List[Int] = euClpFallback.map(_.number)
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
      if !byNumber.contains(e.number) then
        errs += OutlineError.BadNumber(e.number.toString, "out of range (expected 1..16)")
      else
        byNumber.get(e.number).foreach { expected =>
          if e.title != expected then
            errs += OutlineError.TitleMismatch(e.number, e.title, expected)
        }
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

  /** Acetone tutorial spine: sparse outline of sections the host *language*
    * objects actually speak to (hazards + composition).
    */
  def acetoneSparseOutline: List[OutlineEntry] = List(
    OutlineEntry(2, "Hazards identification"),
    OutlineEntry(3, "Composition/information on ingredients"))
