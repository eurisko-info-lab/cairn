package cairn.core

/** Generic lang → en → any fallback over `(lang, text)` rows.
  *
  * Domain packs must not re-implement this pick order in match arms.
  */
object MultilingualResolve:
  def pick(rows: List[(String, String)], lang: String): Option[String] =
    rows.collectFirst { case (l, t) if l == lang => t }
      .orElse(rows.collectFirst { case (l, t) if l == "en" => t })
      .orElse(rows.headOption.map(_._2))

  def pickRef(rows: List[(String, String)], lang: String): Option[String] =
    pick(rows, lang)
