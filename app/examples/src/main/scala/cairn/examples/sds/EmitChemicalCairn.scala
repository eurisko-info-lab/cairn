package cairn.examples.sds
import cairn.runtime.EffectContexts

import cairn.core.*
import cairn.runtime.PackLoader
import java.nio.file.{Files, Path}

/** One-shot emitter for SDS chemical instance `.cairn` modules. */
@main def EmitChemicalCairn(args: String*): Unit =
  val packs = PackLoader(EffectContexts.forPackLoader())
  val sds = Sds(packs)
  val g = ModuleSurface.grammar(sds.language)
  val outDir = Path.of(args.headOption.getOrElse("content/languages/sds/chemicals"))
  Files.createDirectories(outDir)
  def emit(name: String, m: Module): Unit =
    val text = Printer.print(g, ModuleSurface.fromModule(m)).fold(e => sys.error(e), identity)
    val path = outDir.resolve(s"$name.cairn")
    Files.writeString(path, text + "\n")
    println(s"wrote $path (${text.length} bytes, digest=${m.digest.short})")
  emit("acetone-thin", Chemicals.Acetone.thin.toTypedModule("acetoneOutline"))
  emit("acetone", Chemicals.Acetone.pure.toModule("acetoneOutline"))
  emit("ethanol-thin", Chemicals.Ethanol.thin.toTypedModule("ethanolOutline"))
  emit("ethanol", Chemicals.Ethanol.pure.toModule("ethanolOutline"))
