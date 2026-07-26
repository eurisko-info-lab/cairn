package cairn.core

import cairn.kernel.*

/** Resolves a composed language's alias-level `provider`/`validate`
  * declarations (`target.providers`/`target.validations` — kernel-safe data,
  * populated by [[Meta.elaborateFragment]]/[[Compose.compose]]) into a real,
  * digest-bound [[ValidationModel]].
  *
  * This is a SEPARATE, later step from elaboration by necessity:
  * `Meta.elaborateFragment` sees one fragment's own text in isolation and
  * cannot know another language's actual digest — only here, with a
  * resolver that can load every other pack, do aliases become real
  * [[Digest]]s. Exactly analogous to how `Fragment.requires` names are
  * resolved into real merged fragments only at pack-close time, never
  * inside `elaborateFragment` itself.
  */
object ValidationModelLoader:
  /** `resolveProvider` must return the EXACT language a `provider` alias
    * names (by pack name, e.g. `"eu-clp"`) — typically `packs.requireClosed`.
    * Throws (matching `requireClosed`'s own convention) if an alias a
    * `validate ... satisfies alias.judgment;` declaration references was
    * never itself declared via a `provider alias = language name;`.
    */
  def resolve(target: ComposedLanguage, resolveProvider: String => ComposedLanguage): ValidationModel =
    val providerDigests: Map[String, Digest] =
      target.providers.map((alias, langName) => alias -> resolveProvider(langName).digest)
    def resolveAlias(alias: String): Digest =
      providerDigests.getOrElse(alias, throw RuntimeException(
        s"ValidationModelLoader: judgment provider alias '$alias' is not declared (missing 'provider $alias = language ...;')"))
    val specs = target.validations.map {
      case Canon.CTag("LeafOkUnresolved", m) =>
        ModuleStructural.Spec.LeafOk(m.field("ctor").asStr, m.field("idx").asInt.toInt,
          JudgmentRef(resolveAlias(m.field("alias").asStr), m.field("judgmentName").asStr))
      case Canon.CTag("OutlineNumsUnresolved", m) =>
        ModuleStructural.Spec.OutlineNums(m.field("ctor").asStr, m.field("refsField").asInt.toInt,
          m.field("numberSources").asList.map(ModuleStructural.NumberSource.fromCanon),
          JudgmentRef(resolveAlias(m.field("alias").asStr), m.field("judgmentName").asStr), m.field("label").asStr)
      case other => ModuleStructural.Spec.fromCanon(other)
    }
    ValidationModel(target.digest, specs, providerDigests.values.toList.distinct.sortBy(_.hex))
