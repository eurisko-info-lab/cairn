package cairn.workbench

import cairn.kernel.*
import java.nio.file.{Files, Path}

/** Runtime pack loader for checked-in `.cairn` languages (M42 + exemplar DAG).
  *
  * Fragment `provides`/`requires` edges are real composition constraints: a pack
  * whose own fragments require an interface must pull in a providing pack before
  * [[Compose.compose]] succeeds. The shipped exemplar chain is
  * `PKI (cert) → Law (law) → SDS (sds)`.
  *
  * Dependency fragments are demoted (no `top`, no `varCtor`) so leaf packs keep
  * a single surface top while still amalgamating dependency sorts/ctors/grammar.
  */
object PackLoader:
  /** Strip leaf-only surface markers when a pack is used as a dependency. */
  def demote(f: Fragment): Fragment =
    f.copy(varCtor = None, grammar = f.grammar.copy(top = None))

  def languageDirs: List[Path] =
    List("languages", "../languages", "../../languages").map(Path.of(_))
      .filter(Files.isDirectory(_))

  /** Raw fragments per language file name (no composition / no dep resolution).
    * Parse failures are fatal — silent skips hide incomplete `.cairn` packs.
    */
  def loadRaw(dir: Path): Map[String, List[Fragment]] =
    if !Files.exists(dir) then Map.empty
    else
      import scala.jdk.CollectionConverters.*
      Files.list(dir).iterator.asScala
        .filter(_.toString.endsWith(".cairn"))
        .map { p =>
          Meta.parseLanguageAst(Files.readString(p)) match
            case Right((name, fs)) => name -> fs
            case Left(err) =>
              throw RuntimeException(s"failed to parse language pack $p: $err")
        }
        .toMap

  def loadRaw(): Map[String, List[Fragment]] =
    languageDirs.view.map(loadRaw).find(_.nonEmpty).getOrElse(Map.empty)

  /** Resolve transitive `requires` by pulling providing packs, then compose. */
  def close(name: String, packs: Map[String, List[Fragment]]): Either[List[ComposeError], ComposedLanguage] =
    packs.get(name) match
      case None =>
        Left(List(ComposeError("pack", name, "-", s"language pack '$name' not found")))
      case Some(own) =>
        var selected = Map(name -> own)
        var guard = 0
        var progress = true
        while progress && guard < 32 do
          guard += 1
          progress = false
          val provided = selected.values.flatten.flatMap(_.provides).toSet
          val needed = selected.values.flatten.flatMap(_.requires).filterNot(provided.contains).toSet
          for iface <- needed do
            packs.find { (n, fs) =>
              !selected.contains(n) && fs.exists(_.provides.contains(iface))
            } match
              case Some((dep, fs)) =>
                selected = selected + (dep -> fs)
                progress = true
              case None => ()
        val fragments = selected.toList.flatMap { (n, fs) =>
          if n == name then fs else fs.map(demote)
        }
        Compose.compose(name, fragments)

  def close(name: String): Either[List[ComposeError], ComposedLanguage] =
    close(name, loadRaw())

  def requireOwn(name: String): List[Fragment] =
    loadRaw().getOrElse(name, throw RuntimeException(s"language pack '$name' not found under languages/"))

  def requireClosed(name: String): ComposedLanguage =
    close(name).fold(
      e => throw RuntimeException(e.map(_.render).mkString("\n")),
      identity)

  /** Close every pack that can be closed from `dir` (skips unsatisfied graphs). */
  def loadClosed(dir: Path): Map[String, ComposedLanguage] =
    val raw = loadRaw(dir)
    raw.keys.flatMap(n => close(n, raw).toOption.map(n -> _)).toMap

  def loadClosed(): Map[String, ComposedLanguage] =
    languageDirs.view.map(loadClosed).find(_.nonEmpty).getOrElse(Map.empty)

  /** Interfaces this pack's own fragments still need after local provides. */
  def unmetRequires(name: String, packs: Map[String, List[Fragment]]): Set[String] =
    packs.get(name) match
      case None => Set.empty
      case Some(fs) =>
        val provided = fs.flatMap(_.provides).toSet
        fs.flatMap(_.requires).filterNot(provided.contains).toSet
