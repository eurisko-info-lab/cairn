package cairn.examples.sds

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.PackLoader
import cairn.systemhandler.EffectContext
import java.nio.file.{Files, Path}

/** One-shot emitter for SDS chemical instance `.cairn` modules. */
@main def EmitChemicalCairn(args: String*): Unit =
  val packs = PackLoader(EffectContext.forPackLoader())
  val sds = Sds(packs)
  val g = ModuleSurface.grammar(sds.language)
  val outDir = Path.of(args.headOption.getOrElse("languages/sds/chemicals"))
  Files.createDirectories(outDir)
  def emit(name: String, m: Module): Unit =
    val text = Printer.print(g, ModuleSurface.fromModule(m)).fold(e => sys.error(e), identity)
    val path = outDir.resolve(s"$name.cairn")
    Files.writeString(path, text + "\n")
    println(s"wrote $path (${text.length} bytes, digest=${m.digest.short})")
  emit("acetone-thin", Chemicals.Acetone.thinModule)
  emit("acetone", Chemicals.Acetone.asModule)
  emit("ethanol-thin", Chemicals.Ethanol.thinModule)
  emit("ethanol", Chemicals.Ethanol.asModule)
