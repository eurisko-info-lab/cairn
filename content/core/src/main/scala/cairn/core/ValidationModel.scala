package cairn.core

import cairn.kernel.*

/** Names a judgment by the exact language artifact that provides it — not
  * just a bare string. Two [[ModuleStructural.Spec]]s referencing
  * `"sectionNumberOk"` are only the SAME check if they also agree on WHICH
  * language declared that judgment; `JudgmentRef` makes that agreement part
  * of the spec's own canon instead of an unstated assumption at the call site.
  */
final case class JudgmentRef(language: Digest, judgment: String):
  def canon: Canon = Canon.cmap("language" -> Canon.CStr(language.hex), "judgment" -> Canon.CStr(judgment))

object JudgmentRef:
  def fromCanon(c: Canon): JudgmentRef = JudgmentRef(Digest(c.field("language").asStr), c.field("judgment").asStr)
