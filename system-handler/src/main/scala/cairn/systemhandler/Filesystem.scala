package cairn.systemhandler

import cairn.systeminterface.Filesystem as Fs
import cairn.kernel.{Authority, Effects}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Local filesystem handler (Phase 3). `perform` is the only entry point,
  * gated by [[AuthorityGate]] (Subject("local") is a placeholder — this
  * family has no real multi-tenant identity yet); every other method here
  * is private. The action derivation below mirrors
  * `kernel.EffectMeta.filesystem`'s `requestActions` grouping exactly
  * (read/write/mkdirs classes; `Resolve` needs no right, pure path
  * arithmetic with zero I/O).
  */
object Filesystem:
  private def toNio(p: Fs.Path): Path = Path.of(p.value)
  private def fromNio(p: Path): Fs.Path = Fs.Path(p.toString)

  private def mkdirs(dir: Path): Unit = Files.createDirectories(dir)

  /** Write `content` to `path`, creating parent directories if needed. */
  private def writeFile(path: Path, content: String): Unit =
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.writeString(path, content)

  private def writeBytes(path: Path, bytes: Array[Byte]): Unit =
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.write(path, bytes)

  private def readText(path: Path): String = Files.readString(path)
  private def readBytes(path: Path): Array[Byte] = Files.readAllBytes(path)
  private def exists(path: Path): Boolean = Files.exists(path)
  private def isDirectory(path: Path): Boolean = Files.isDirectory(path)
  private def isRegularFile(path: Path): Boolean = Files.isRegularFile(path)
  private def isExecutable(path: Path): Boolean = Files.isExecutable(path)

  private def list(path: Path): List[Path] =
    if !Files.exists(path) then Nil
    else Files.list(path).iterator.asScala.toList

  private def delete(path: Path): Unit =
    if Files.exists(path) then Files.delete(path)

  private def createTempDirectory(prefix: String): Path =
    Files.createTempDirectory(prefix)

  def perform(req: Fs.Request): Either[Fs.Error, Fs.Response] =
    // Resource path is the real target of the request (or, for
    // CreateTempDirectory, its prefix — the closest thing to a target it
    // has, since the actual path is OS-generated after the fact) rather
    // than a wildcard, so path-scoped policies (already supported by
    // Authority.Resource.matches) have real data to match against.
    val (action, resourcePath): (Option[Effects.Action], String) = req match
      case Fs.Request.Read(p)                     => (Some(Effects.Action.FsRead), p.value)
      case Fs.Request.Exists(p)                    => (Some(Effects.Action.FsRead), p.value)
      case Fs.Request.IsDirectory(p)               => (Some(Effects.Action.FsRead), p.value)
      case Fs.Request.IsRegularFile(p)              => (Some(Effects.Action.FsRead), p.value)
      case Fs.Request.IsExecutable(p)               => (Some(Effects.Action.FsRead), p.value)
      case Fs.Request.List(p)                       => (Some(Effects.Action.FsRead), p.value)
      case Fs.Request.Write(p, _)                   => (Some(Effects.Action.FsWrite), p.value)
      case Fs.Request.WriteBytes(p, _)               => (Some(Effects.Action.FsWrite), p.value)
      case Fs.Request.Delete(p)                     => (Some(Effects.Action.FsWrite), p.value)
      case Fs.Request.Mkdirs(p)                      => (Some(Effects.Action.FsMkdirs), p.value)
      case Fs.Request.CreateTempDirectory(prefix)    => (Some(Effects.Action.FsMkdirs), prefix)
      case Fs.Request.Resolve(_, _)                  => (None, "*")
    action match
      case None => performRaw(req)
      case Some(a) =>
        val authReq = Authority.EffectRequest(Authority.Subject("local"), a, Authority.Resource("filesystem", resourcePath))
        AuthorityGate.forFamily(Effects.Family.Filesystem).checked(authReq)(err => Fs.Error.Io(s"denied: $err"))(performRaw(req))

  private def performRaw(req: Fs.Request): Either[Fs.Error, Fs.Response] =
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
