package cairn.systemhandler

import cairn.kernel.Authority
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.jdk.CollectionConverters.*

/** Durable, issuer-scoped replay store for capability nonces and request ids.
  *
  * Shared across [[AuthorityGate]] instances (not gate-local). Tokens are
  * namespaced by issuer id so distinct subjects may reuse the same string
  * without colliding. Backed by in-memory sets or append-only filesystem
  * ledgers (CAS content-addressed blobs are a poor fit for growing sets).
  */
trait ReplayStore:
  /** Consume a one-shot grant nonce under `issuer`. */
  def consumeNonce(issuer: String, nonce: String): Either[String, Unit]

  /** Consume a one-shot request id under `issuer`. */
  def consumeRequestId(issuer: String, requestId: String): Either[String, Unit]

  /** Snapshot of consumed tokens (tests / diagnostics). */
  def snapshot: ReplayStore.Snapshot

object ReplayStore:
  final case class Snapshot(
      nonces: Map[String, Set[String]],
      requestIds: Map[String, Set[String]],
  ):
    def flatNonces: Set[String] = nonces.values.flatten.toSet
    def flatRequestIds: Set[String] = requestIds.values.flatten.toSet

  /** Process-local store — default for fresh gates. */
  def memory(): ReplayStore = new MemoryReplayStore

  /** Append-only files under `root/<issuer>/{nonces,requestIds}`. */
  def filesystem(root: Path): ReplayStore = new FilesystemReplayStore(root)

  private final class MemoryReplayStore extends ReplayStore:
    private var nonces: Map[String, Set[String]] = Map.empty
    private var requestIds: Map[String, Set[String]] = Map.empty

    def consumeNonce(issuer: String, nonce: String): Either[String, Unit] =
      synchronized {
        Authority.Replay.consumeNonce(nonce, nonces.getOrElse(issuer, Set.empty)).map { next =>
          nonces = nonces.updated(issuer, next)
        }
      }

    def consumeRequestId(issuer: String, requestId: String): Either[String, Unit] =
      synchronized {
        Authority.Replay.consumeRequestId(requestId, requestIds.getOrElse(issuer, Set.empty)).map { next =>
          requestIds = requestIds.updated(issuer, next)
        }
      }

    def snapshot: Snapshot = synchronized { Snapshot(nonces, requestIds) }

  private final class FilesystemReplayStore(root: Path) extends ReplayStore:
    private def issuerDir(issuer: String): Path =
      val safe = issuer.map(c => if c.isLetterOrDigit || c == '-' || c == '_' then c else '_').mkString
      root.resolve(if safe.nonEmpty then safe else "_")

    private def loadSet(file: Path): Set[String] =
      if !Files.isRegularFile(file) then Set.empty
      else
        Files.readString(file).linesIterator.map(_.trim).filter(_.nonEmpty).toSet

    private def appendToken(file: Path, token: String): Unit =
      Option(file.getParent).foreach(Files.createDirectories(_))
      Files.writeString(
        file,
        token + "\n",
        StandardOpenOption.CREATE,
        StandardOpenOption.WRITE,
        StandardOpenOption.APPEND)

    private def consume(kind: String, issuer: String, token: String, fileName: String): Either[String, Unit] =
      synchronized {
        val file = issuerDir(issuer).resolve(fileName)
        val used = loadSet(file)
        Authority.Replay.consume(token, used, kind).map { _ =>
          appendToken(file, token)
        }
      }

    def consumeNonce(issuer: String, nonce: String): Either[String, Unit] =
      consume("nonce", issuer, nonce, "nonces")

    def consumeRequestId(issuer: String, requestId: String): Either[String, Unit] =
      consume("requestId", issuer, requestId, "requestIds")

    def snapshot: Snapshot =
      synchronized {
        if !Files.isDirectory(root) then Snapshot(Map.empty, Map.empty)
        else
          val issuers =
            Files.list(root).iterator.asScala.filter(Files.isDirectory(_)).toList
          val nonces = issuers.map { d =>
            d.getFileName.toString -> loadSet(d.resolve("nonces"))
          }.filter(_._2.nonEmpty).toMap
          val ids = issuers.map { d =>
            d.getFileName.toString -> loadSet(d.resolve("requestIds"))
          }.filter(_._2.nonEmpty).toMap
          Snapshot(nonces, ids)
      }
