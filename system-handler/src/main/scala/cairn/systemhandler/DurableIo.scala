package cairn.systemhandler

import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}

/** Crash-safe durable writes: write to a sibling temp file, then rename into place. */
object DurableIo:
  def writeAtomic(path: Path, bytes: Array[Byte]): Either[String, Unit] =
    try
      val parent = Option(path.getParent).getOrElse(path.toAbsolutePath.getParent)
      Files.createDirectories(parent)
      val tmp = parent.resolve(s".${path.getFileName}.tmp.${ProcessHandle.current().pid}")
      Files.write(tmp, bytes)
      try
        Files.move(
          tmp, path,
          StandardCopyOption.REPLACE_EXISTING,
          StandardCopyOption.ATOMIC_MOVE)
      catch
        case _: AtomicMoveNotSupportedException =>
          Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING)
      Right(())
    catch
      case e: Exception =>
        Left(e.getMessage)
