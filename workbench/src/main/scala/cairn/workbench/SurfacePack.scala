package cairn.workbench

import cairn.kernel.*

/** Concrete syntax pack bound to a semantic language (Phase 2).
  *
  * File layout (pragmatic Meta interim — no `surface` top yet):
  *   languages/<lang>/surfaces/<style>.cairn
  * with body `language <style> { fragment … { keyword/syntax/… } }`.
  * Fragment names match the language's semantic fragments; grammar is merged
  * by [[PackLoader.bindSurface]].
  */
final case class SurfacePack(
    name: String,
    language: String,
    fragments: List[Fragment],
):
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name),
    "language" -> Canon.CStr(language),
    "fragments" -> Canon.CList(fragments.sortBy(_.name).map(f =>
      Canon.cmap(
        "name" -> Canon.CStr(f.name),
        "grammar" -> SurfacePack.grammarCanon(f.grammar)))))
  def digest: Digest = Digest.of(canon)

object SurfacePack:
  def grammarCanon(g: GrammarPart): Canon = Canon.cmap(
    "keywords" -> Canon.cstrs(g.keywords),
    "puncts" -> Canon.cstrs(g.puncts),
    "identContExtra" -> Canon.CStr(g.identContExtra),
    "spec" -> GrammarSpec.toCanon(GrammarSpec("", TokenSpec(Nil, Nil, None),
      g.categories, g.precCategories, g.printRules, g.top.getOrElse(""))))
