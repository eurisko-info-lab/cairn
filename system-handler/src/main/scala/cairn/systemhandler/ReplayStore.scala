package cairn.systemhandler

import cairn.kernel.{Artifact, ArtifactKind, Authority, Canon, Digest}
import cairn.systeminterface.Cas
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.jdk.CollectionConverters.*

/** Durable, issuer-scoped replay store for capability nonces and request ids.
  *
  * Shared across [[AuthorityGate]] instances (not gate-local). Tokens are
  * namespaced by issuer id so distinct subjects may reuse the same string
  * without colliding. Backed by in-memory sets or append-only filesystem
  * ledgers. Growing sets are not stored as a single CAS blob; instead a
  * [[ReplayStore.Snapshot]] may be published as a CAS `replay-snapshot`
  * artifact and merged on another node (syncable digests — not consensus).
  */
trait ReplayStore:
  /** Consume a one-shot grant nonce under `issuer`. */
  def consumeNonce(issuer: String, nonce: String): Either[String, Unit]

  /** Consume a one-shot request id under `issuer`. */
  def consumeRequestId(issuer: String, requestId: String): Either[String, Unit]

  /** Union `snap` into this store (already-present tokens stay present). */
  def absorb(snap: ReplayStore.Snapshot): Unit

  /** Snapshot of consumed tokens (tests / diagnostics / CAS publish). */
  def snapshot: ReplayStore.Snapshot

object ReplayStore:
  final case class Snapshot(
      nonces: Map[String, Set[String]],
      requestIds: Map[String, Set[String]],
  ):
    def flatNonces: Set[String] = nonces.values.flatten.toSet
    def flatRequestIds: Set[String] = requestIds.values.flatten.toSet

    def union(other: Snapshot): Snapshot =
      def mergeMaps(a: Map[String, Set[String]], b: Map[String, Set[String]]) =
        (a.keySet ++ b.keySet).map { k =>
          k -> (a.getOrElse(k, Set.empty) ++ b.getOrElse(k, Set.empty))
        }.toMap
      Snapshot(mergeMaps(nonces, other.nonces), mergeMaps(requestIds, other.requestIds))

    def toCanon: Canon =
      def issuerMap(m: Map[String, Set[String]]): Canon =
        Canon.cmap(m.toSeq.sortBy(_._1).map { (issuer, toks) =>
          issuer -> Canon.cstrs(toks.toList.sorted)
        }*)
      Canon.CTag(
        "replay-snapshot",
        Canon.cmap(
          "nonces" -> issuerMap(nonces),
          "requestIds" -> issuerMap(requestIds)))

    def artifact: Artifact = Artifact(ArtifactKind.ReplaySnapshot, toCanon)

  object Snapshot:
    def empty: Snapshot = Snapshot(Map.empty, Map.empty)

    def fromCanon(c: Canon): Either[String, Snapshot] =
      import Canon.*
      c match
        case CTag("replay-snapshot", body) =>
          def readMap(field: String): Either[String, Map[String, Set[String]]] =
            try
              body.field(field) match
                case CMap(es) =>
                  Right(es.map { (k, v) =>
                    k -> v.asList.map(_.asStr).toSet
                  }.toMap)
                case _ => Left(s"replay-snapshot: $field is not a map")
            catch case e: Exception => Left(Option(e.getMessage).getOrElse(e.toString))
          for
            nonces <- readMap("nonces")
            ids <- readMap("requestIds")
          yield Snapshot(nonces, ids)
        case _ => Left("replay-snapshot: missing tag")

    def fromArtifact(a: Artifact): Either[String, Snapshot] =
      if a.kind != ArtifactKind.ReplaySnapshot then
        Left(s"expected replay-snapshot artifact, got ${a.kind.name}")
      else fromCanon(a.body)

  /** Process-local store — default for fresh gates. */
  def memory(): ReplayStore = new MemoryReplayStore

  /** Append-only files under `root/<issuer>/{nonces,requestIds}`. */
  def filesystem(root: Path): ReplayStore = new FilesystemReplayStore(root)

  /** Publish current snapshot into CAS; returns the content digest. */
  def publish(store: ReplayStore, cas: Cas, ctx: EffectContext): Either[Cas.Error, Digest] =
    CasEffects.put(cas, store.snapshot.artifact, ctx).map(_.valueHash)

  /** Load a CAS `replay-snapshot` digest and [[ReplayStore.absorb]] it. */
  def mergeFromCas(
      store: ReplayStore, cas: Cas, digest: Digest, ctx: EffectContext
  ): Either[String, Unit] =
    CasEffects.get(cas, digest, ctx).left.map(_.toString).flatMap { a =>
      Snapshot.fromArtifact(a).map(store.absorb)
    }

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

    def absorb(snap: Snapshot): Unit =
      synchronized {
        def merge(into: Map[String, Set[String]], from: Map[String, Set[String]]) =
          from.foldLeft(into) { case (acc, (issuer, toks)) =>
            acc.updated(issuer, acc.getOrElse(issuer, Set.empty) ++ toks)
          }
        nonces = merge(nonces, snap.nonces)
        requestIds = merge(requestIds, snap.requestIds)
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

    def absorb(snap: Snapshot): Unit =
      synchronized {
        def absorbKind(m: Map[String, Set[String]], fileName: String): Unit =
          m.foreach { (issuer, toks) =>
            val file = issuerDir(issuer).resolve(fileName)
            val used = loadSet(file)
            toks.foreach { t =>
              if !used.contains(t) then appendToken(file, t)
            }
          }
        absorbKind(snap.nonces, "nonces")
        absorbKind(snap.requestIds, "requestIds")
      }

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
