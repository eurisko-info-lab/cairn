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

/** A whole-module validator with canonical, content-addressed identity —
  * `targetLanguage` the language it validates modules of, `specs` the
  * complete [[ModuleStructural.Spec]] list (order-preserved: unlike
  * [[ChangeModel]]'s operations, spec order can affect error-message
  * ordering, so it is NOT sorted before hashing), `providers` every
  * judgment-provider language digest any spec's [[JudgmentRef]]s name —
  * recorded explicitly (not just implied by decoding every spec) so an
  * auditor can see which provider languages are in play without decoding
  * the whole model.
  */
final case class ValidationModel(targetLanguage: Digest, specs: List[ModuleStructural.Spec], providers: List[Digest]):
  def canon: Canon = Canon.cmap(
    "targetLanguage" -> Canon.CStr(targetLanguage.hex),
    "specs" -> Canon.CList(specs.map(_.canon)),
    "providers" -> Canon.CList(providers.map(d => Canon.CStr(d.hex))))
  def artifact: Artifact = Artifact(ArtifactKind.ValidationModel, canon)
  def digest: Digest = artifact.digest

object ValidationModel:
  def fromCanon(c: Canon): ValidationModel = ValidationModel(
    Digest(c.field("targetLanguage").asStr),
    c.field("specs").asList.map(ModuleStructural.Spec.fromCanon),
    c.field("providers").asList.map(d => Digest(d.asStr)))
