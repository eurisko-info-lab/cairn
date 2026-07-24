package cairn.core

/** Generic multilingual restale: free-text locales go stale when the English
  * source hash drifts; official-corpus rows never stale.
  *
  * Domain packs supply state tags / projection; this algorithm is not SDS.
  */
object MultilingualRestale:

  final case class Row(
      text: String,
      translatedFromHash: String,
      official: Boolean,
      stale: Boolean,
  )

  /** Mark non-EN free-text rows stale when `translatedFromHash` ≠ `newEnglishHash`. */
  def restale[A](
      current: Map[String, A],
      newEnglishHash: String,
      isOfficial: A => Boolean,
      fromHash: A => String,
      markStale: A => A,
  ): Map[String, A] =
    current.map {
      case ("en", t) => "en" -> t
      case (lang, t) if isOfficial(t) => lang -> t
      case (lang, t) if fromHash(t) == newEnglishHash => lang -> t
      case (lang, t) => lang -> markStale(t)
    }
