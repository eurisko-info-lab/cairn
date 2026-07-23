package cairn.runtime

import cairn.kernel.*
import cairn.core.{ModuleSurface, Parser}
import cairn.systemhandler.{EffectContext, Filesystem, RuntimeEffectRegistry}
import cairn.systeminterface.Filesystem as Fs
import java.nio.file.Path

/** Stronger Meta effect bootstrap: load `effect-interface` language, then each
  * pinned `effect-*` vocabulary pack + `iface.cairn` declaration module, derive
  * [[EffectMeta.EffectFamily]] / digest-bound [[Effects.ActionKey]]s, and check
  * fixpoint against cold-start host seeds.
  *
  * Host seeds ([[EffectMeta.families]], [[EffectMeta.packDecls]]) remain for
  * handler cold start; disk under `languages/` is the runtime source of truth.
  * Action names are **not** a hand-maintained enum — they register from packs.
  * [[Effects.Family]] is the residual JVM routing tag (ids ↔ packDecls).
  */
object EffectBootstrap:

  final case class Loaded(
      interfaceLanguage: ComposedLanguage,
      families: Map[Effects.Family, EffectMeta.EffectFamily],
      pinned: Map[Effects.Family, EffectMeta.PinnedInterface]):
    def actionKey(family: Effects.Family, name: String): Either[String, Effects.ActionKey] =
      pinned.get(family) match
        case None => Left(s"family $family not loaded")
        case Some(p) => Effects.ActionKey.fromPinned(p, name)

    /** Live registry for [[EffectContext]] / handlers (disk-loaded vocabulary). */
    def registry: RuntimeEffectRegistry =
      RuntimeEffectRegistry(families, pinned)

  def ifacePath(pack: String): Path =
    Path.of("content/languages", pack, "iface.cairn")

  def loadDecl(
      interfaceLang: ComposedLanguage,
      pack: String,
      fsCtx: EffectContext
  ): Either[String, EffectMeta.InterfaceDecl] =
    val path = ifacePath(pack)
    val abs = Fs.Path(path.toAbsolutePath.normalize.toString)
    Filesystem.run(Fs.Request.Read(abs), fsCtx) match
      case Left(e) => Left(s"$path: $e")
      case Right(Fs.Response.Text(text)) =>
        for
          cst <- Parser.parse(ModuleSurface.grammar(interfaceLang), text)
          mod <- ModuleSurface.toModule(cst)
          decl <- EffectMeta.InterfaceDecl.fromModule(mod.defs)
        yield decl
      case Right(other) => Left(s"$path: unexpected fs response $other")

  /** Sort/ctor shape of a vocabulary Fragment (name/`provides` may differ
    * between host seed `effect.clock` and disk pack `effect-clock`). */
  def vocabShape(f: Fragment): (Set[(String, SortMode)], Set[(String, String, List[String])]) =
    (
      f.sorts.map(s => (s.name, s.mode)).toSet,
      f.constructors.map(c => (c.name, c.sort, c.argSorts)).toSet)

  private def loadOne(
      packs: PackLoader,
      interfaceLang: ComposedLanguage,
      fsCtx: EffectContext,
      name: String
  ): Either[String, (Effects.Family, EffectMeta.EffectFamily, EffectMeta.PinnedInterface)] =
    for
      lang <- Right(packs.requireClosed(name))
      frag <- lang.fragments.find(_.name == name).toRight(s"$name: missing fragment '$name'")
      diskDecl <- loadDecl(interfaceLang, name, fsCtx)
      hostDecl <- EffectMeta.packDecls.get(name).toRight(s"$name: no host packDecls seed")
      _ <- Either.cond(diskDecl == hostDecl, (), s"$name: iface.cairn diverges from host packDecls seed")
      ef <- EffectMeta.fromFragmentPack(name, frag, diskDecl).left.map(e => s"$name: $e")
      _ <- EffectMeta.completeness(ef) match
        case Nil => Right(())
        case errs => Left(s"$name: ${errs.mkString("; ")}")
      host <- EffectMeta.families.get(ef.family).toRight(s"$name: no host EffectMeta seed for ${ef.family}")
      _ <- Either.cond(ef.decl == host.decl, (), s"$name: declaration mismatch vs host seed")
      _ <- Either.cond(
        vocabShape(ef.fragment) == vocabShape(host.fragment), (),
        s"$name: vocabulary shape mismatch vs host seed")
      _ <- Either.cond(
        Effects.Family.forPack(name) == Some(ef.family), (),
        s"$name: Family.forPack bridge missing or wrong")
      pinned <- EffectMeta.PinnedInterface.fromArtifact(EffectMeta.interfaceArtifact(ef))
        .left.map(e => s"$name pin: $e")
      _ <- ef.actions.foldLeft[Either[String, Unit]](Right(())) { (acc, a) =>
        acc.flatMap(_ =>
          Effects.ActionKey.fromPinned(pinned, a) match
            case Left(e) => Left(s"$name ActionKey.fromPinned($a): $e")
            case Right(k) if k.interfaceDigest != Some(ef.fragment.digest) =>
              Left(s"$name: ActionKey $a not bound to disk fragment digest")
            case Right(_) => Right(()))
      }
    yield (ef.family, ef, pinned)

  /** Load all effect packs from disk; verify against host seeds. */
  def load(packs: PackLoader, fsCtx: EffectContext = EffectContext.forFilesystem()): Either[String, Loaded] =
    if !Effects.Family.idsMatchPackDecls then
      Left(s"Family enum ids ${Effects.Family.values.map(_.toString).toSet} != packDecls family ids")
    else
      val interfaceLang = packs.requireClosed("effect-interface")
      EffectMeta.fragmentPackNames.foldLeft[Either[String, List[(Effects.Family, EffectMeta.EffectFamily, EffectMeta.PinnedInterface)]]](Right(Nil)) {
        case (Left(e), _) => Left(e)
        case (Right(acc), name) =>
          loadOne(packs, interfaceLang, fsCtx, name).map(acc :+ _)
      }.flatMap { rows =>
        val familyMap = rows.map(r => r._1 -> r._2).toMap
        val pinMap = rows.map(r => r._1 -> r._3).toMap
        if familyMap.keySet != Effects.Family.values.toSet then
          Left(s"loaded families ${familyMap.keySet} != enum ${Effects.Family.values.toSet}")
        else
          val derivedNames = familyMap.values.flatMap(f => f.actions.map(a => s"${f.family}.$a")).toSet
          val packNames = EffectMeta.packDecls.values.flatMap(d => d.actions.map(a => s"${d.familyId}.$a")).toSet
          if derivedNames != packNames then
            Left(s"bootstrap ActionKey names from disk $derivedNames != packDecls $packNames")
          else
            Right(Loaded(interfaceLang, familyMap, pinMap))
      }

  /** Convenience: load or throw (composition-root / test helper). */
  def require(packs: PackLoader, fsCtx: EffectContext = EffectContext.forFilesystem()): Loaded =
    load(packs, fsCtx).fold(e => throw RuntimeException(e), identity)
