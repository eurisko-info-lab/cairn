package cairn.systemhandler

import java.nio.channels.FileChannel
import java.nio.file.{
  AtomicMoveNotSupportedException, Files, Path, StandardCopyOption, StandardOpenOption
}
import java.nio.file.attribute.FileTime

/** Crash-safe durable writes: temp file + fsync + rename, then fsync parent.
  *
  * Consensus state must use [[writeConsensus]] (atomic move required — no
  * non-atomic fallback). Ordinary durable files may use [[writeAtomic]].
  */
object DurableIo:
  /** Best-effort durable write. Falls back to non-atomic replace only when the
    * filesystem rejects `ATOMIC_MOVE` (not for consensus paths).
    */
  def writeAtomic(path: Path, bytes: Array[Byte]): Either[String, Unit] =
    write(path, bytes, requireAtomicMove = false, replaceExisting = true)

  /** Consensus / voting-state write: atomic move required; no fallback. */
  def writeConsensus(path: Path, bytes: Array[Byte]): Either[String, Unit] =
    write(path, bytes, requireAtomicMove = true, replaceExisting = true)

  /** Create-only write (fails if `path` already exists). Used by keystore. */
  def writeCreateOnly(path: Path, bytes: Array[Byte]): Either[String, Unit] =
    if Files.exists(path) then Left(s"durable-io: refusing to overwrite existing $path")
    else write(path, bytes, requireAtomicMove = true, replaceExisting = false)

  private def write(
      path: Path,
      bytes: Array[Byte],
      requireAtomicMove: Boolean,
      replaceExisting: Boolean,
  ): Either[String, Unit] =
    var tmp: Path = null
    try
      val parent = Option(path.getParent).getOrElse(path.toAbsolutePath.getParent)
      Files.createDirectories(parent)
      tmp = parent.resolve(s".${path.getFileName}.tmp.${ProcessHandle.current().pid}.${System.nanoTime}")
      Files.write(tmp, bytes)
      fsyncPath(tmp)
      val moveOpts =
        if replaceExisting then
          Array[StandardCopyOption](StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
        else
          Array[StandardCopyOption](StandardCopyOption.ATOMIC_MOVE)
      try
        Files.move(tmp, path, moveOpts*)
        tmp = null
      catch
        case _: AtomicMoveNotSupportedException if !requireAtomicMove && replaceExisting =>
          Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING)
          tmp = null
        case _: AtomicMoveNotSupportedException =>
          return Left(s"durable-io: atomic move required but unsupported for $path")
      fsyncParent(parent)
      // Bump mtime so observers notice the replace even when content length matches.
      Files.setLastModifiedTime(path, FileTime.fromMillis(System.currentTimeMillis()))
      Right(())
    catch
      case e: Exception =>
        Left(Option(e.getMessage).getOrElse(e.getClass.getSimpleName))
    finally
      if tmp != null && Files.exists(tmp) then
        try Files.deleteIfExists(tmp) catch case _: Exception => ()

  private def fsyncPath(path: Path): Unit =
    val ch = FileChannel.open(path, StandardOpenOption.WRITE)
    try ch.force(true)
    finally ch.close()

  /** Directory fsync — best-effort; some OS/FS pairs disallow opening dirs. */
  private def fsyncParent(parent: Path): Unit =
    try
      val ch = FileChannel.open(parent, StandardOpenOption.READ)
      try ch.force(true)
      finally ch.close()
    catch case _: Exception => ()
