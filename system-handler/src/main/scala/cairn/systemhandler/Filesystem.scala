package cairn.systemhandler

import cairn.systeminterface.Filesystem as Fs
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Local filesystem handler (Phase 3). Convenience methods keep existing
  * call sites compiling; [[perform]] is the typed effect entry point.
  */
object Filesystem:
  def toNio(p: Fs.Path): Path = Path.of(p.value)
  def fromNio(p: Path): Fs.Path = Fs.Path(p.toString)

  def mkdirs(dir: Path): Unit = Files.createDirectories(dir)

  /** Write `content` to `path`, creating parent directories if needed. */
  def writeFile(path: Path, content: String): Unit =
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.writeString(path, content)

  def writeBytes(path: Path, bytes: Array[Byte]): Unit =
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.write(path, bytes)

  def readText(path: Path): String = Files.readString(path)
  def readBytes(path: Path): Array[Byte] = Files.readAllBytes(path)
  def exists(path: Path): Boolean = Files.exists(path)
  def isDirectory(path: Path): Boolean = Files.isDirectory(path)
  def isRegularFile(path: Path): Boolean = Files.isRegularFile(path)
  def isExecutable(path: Path): Boolean = Files.isExecutable(path)

  def list(path: Path): List[Path] =
    if !Files.exists(path) then Nil
    else Files.list(path).iterator.asScala.toList

  def delete(path: Path): Unit =
    if Files.exists(path) then Files.delete(path)

  def createTempDirectory(prefix: String): Path =
    Files.createTempDirectory(prefix)

  def perform(req: Fs.Request): Either[Fs.Error, Fs.Response] =
    try req match
      case Fs.Request.Read(p) =>
        val n = toNio(p)
        if !Files.exists(n) then Left(Fs.Error.NotFound(p))
        else Right(Fs.Response.Text(Files.readString(n)))
      case Fs.Request.Write(p, content) =>
        writeFile(toNio(p), content); Right(Fs.Response.Ok)
      case Fs.Request.WriteBytes(p, bytes) =>
        writeBytes(toNio(p), bytes); Right(Fs.Response.Ok)
      case Fs.Request.Mkdirs(p) =>
        mkdirs(toNio(p)); Right(Fs.Response.Ok)
      case Fs.Request.Exists(p) =>
        Right(Fs.Response.Bool(exists(toNio(p))))
      case Fs.Request.IsDirectory(p) =>
        Right(Fs.Response.Bool(isDirectory(toNio(p))))
      case Fs.Request.IsRegularFile(p) =>
        Right(Fs.Response.Bool(isRegularFile(toNio(p))))
      case Fs.Request.IsExecutable(p) =>
        Right(Fs.Response.Bool(isExecutable(toNio(p))))
      case Fs.Request.List(p) =>
        Right(Fs.Response.Entries(list(toNio(p)).map(_.getFileName.toString)))
      case Fs.Request.Delete(p) =>
        delete(toNio(p)); Right(Fs.Response.Ok)
      case Fs.Request.CreateTempDirectory(prefix) =>
        Right(Fs.Response.PathValue(fromNio(createTempDirectory(prefix))))
      case Fs.Request.Resolve(base, rel) =>
        Right(Fs.Response.PathValue(fromNio(toNio(base).resolve(rel.value))))
    catch case e: Exception => Left(Fs.Error.Io(e.getMessage))
