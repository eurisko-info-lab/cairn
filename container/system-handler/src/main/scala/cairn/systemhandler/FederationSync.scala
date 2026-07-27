package cairn.systemhandler

import cairn.kernel.*

/** PR33.1 slice 4: autonomous, network-driven certificate adoption — the
  * production composition around [[FederationReplica.adoptCertificate]],
  * which is itself deliberately network-free. A replica that missed one or
  * more quorum rounds (offline, partitioned, or simply started late) calls
  * [[synchronizeFinality]] to discover finalized generations it never voted
  * on, pull the exact artifacts those certificates bind, and adopt them —
  * repeating until it has caught up with its peers' finalized history.
  *
  * Trust model: nothing a peer serves is believed on its say-so. A fetched
  * certificate only becomes a candidate once its exact proposal has been
  * fetched (integrity-checked against `certificate.proposal`) and the pair
  * passes [[FederationFinality.verifyCertificateForProposal]] — quorum
  * seals over exactly that proposal digest, every unsigned projection
  * matching the signed proposal's real fields. Adoption itself then re-runs
  * the replica's own deep verification callback (`adoptCertificate`), so a
  * peer can at worst waste this replica's time, never advance its cursor
  * to a generation the quorum didn't approve or the content doesn't prove.
  *
  * The analogous block-finality machinery is [[BftFinality.resumeFollowerAdoption]];
  * this is its federation-shaped counterpart, run from the same places (a
  * node's startup/reconnect path, [[GossipDaemon]]'s periodic tick, and
  * `HttpNode`'s `/federation/msg` handler when a message names an epoch
  * beyond the local cursor).
  */
object FederationSync:

  /** What one [[synchronizeFinality]] pass accomplished: every generation
    * newly adopted (oldest first), and the cursor this replica finished at.
    * `adopted` empty simply means the replica was already caught up (or no
    * peer had anything newer) — that is a success, not an error.
    */
  final case class SyncResult(
      adopted: List[FederationReplica.AdoptedGeneration],
      cursor: FederationReplica.FinalizedFederationCursor,
  )

  /** Recover ONE missing digest from peers — as a federation proposal this
    * replica never received (`/federation/proposal/<hex>`, keyed by the
    * proposal's own artifact digest), or as a raw CAS blob (`/blob/<hex>`).
    * Both paths verify the served bytes actually hash to `digest` before
    * accepting them (`fetchProposal` internally; explicitly for blobs) —
    * never trusts the peer. Shared by `HttpNode`'s `/federation/msg`
    * fetch-and-retry hook and [[synchronizeFinality]]'s closure recovery.
    */
  def fetchMissing(digest: Digest, peerUrls: List[String], replica: FederationReplica, node: Node): Boolean =
    val gotProposal = peerUrls.exists { url =>
      FederationFinality.fetchProposal(url, digest) match
        case Right(p) => replica.learnProposal(p); true
        case _ => false
    }
    val gotBlob = peerUrls.exists { url =>
      BftFinality.httpGet(s"$url/blob/${digest.hex}") match
        case Right(bytes) if Digest.ofBytes(bytes) == digest => node.cas.putBytes(bytes); true
        case _ => false
    }
    gotProposal || gotBlob

  /** The next generation to adopt after `cursor`, if any peer has one:
    * scans every peer's served certificates for candidates claiming to
    * extend the cursor (`previousState`/`epoch` here are only a FILTER —
    * unverified projections narrowing which certificates are worth the
    * round-trips), fetches each candidate's exact proposal by
    * `certificate.proposal`, and returns the lowest-epoch pair that passes
    * the full shared cert↔proposal verifier. Candidates that fail
    * verification are skipped, not fatal — a Byzantine peer must not be
    * able to wedge synchronization for everyone else.
    */
  private def nextVerifiedCertificate(
      replica: FederationReplica,
      peerUrls: List[String],
      cursor: FederationReplica.FinalizedFederationCursor,
  ): Option[FederationFinality.VerifiedProposalCertificate] =
    val candidates = peerUrls
      .flatMap(url => FederationFinality.fetchCerts(url).toOption.toList.flatten)
      .filter(c => c.epoch > cursor.epoch && c.previousState == cursor.state)
      .distinctBy(_.proposal)
      .sortBy(_.epoch)
    candidates.view.flatMap { cert =>
      peerUrls.view
        .flatMap(url => FederationFinality.fetchProposal(url, cert.proposal).toOption)
        .headOption
        .flatMap { proposal =>
          FederationFinality.verifyCertificateForProposal(
            cert, proposal, replica.authorities, replica.setDigest).toOption
        }
    }.headOption

  /** Adopt one verified certificate, recovering missing semantic closure
    * from peers between attempts — the same bounded fetch-and-retry loop
    * `HttpNode`'s `/federation/msg` handler already runs for `receive`,
    * since adoption surfaces `MissingClosure` through the identical
    * convention (proposal recovery and deep-CAS recovery arrive as
    * SEPARATE rounds, so one adoption can need several passes).
    */
  private def adoptWithClosure(
      replica: FederationReplica,
      node: Node,
      peerUrls: List[String],
      verified: FederationFinality.VerifiedProposalCertificate,
      retries: Int,
  ): Either[String, FederationReplica.AdoptedGeneration] =
    replica.adoptCertificate(verified.certificate, verified.proposal) match
      case Right(gen) => Right(gen)
      case Left(e) =>
        FederationReplica.missingClosureDigests(e) match
          case Some(digests) if digests.nonEmpty && retries > 0 &&
              digests.toList.map(fetchMissing(_, peerUrls, replica, node)).exists(identity) =>
            adoptWithClosure(replica, node, peerUrls, verified, retries - 1)
          case _ => Left(e)

  /** Catch this replica's finalized cursor up with its peers: repeatedly
    * find, verify, and adopt the next finalized generation until no peer
    * has anything newer (or `maxGenerations` bounds the pass). Local
    * decision throughout — peers only serve reads (`/federation/certs`,
    * `/federation/proposal/<hex>`, `/blob/<hex>`); no new write endpoint
    * exists for adoption, deliberately.
    *
    * `Left` only for a REAL failure (a fully-verified certificate whose
    * generation this replica cannot adopt — deep verification rejection or
    * durable-I/O fail-closed); running out of new generations is `Right`.
    */
  def synchronizeFinality(
      replica: FederationReplica,
      node: Node,
      peerUrls: List[String],
      maxGenerations: Int = 64,
      fetchRetries: Int = 4,
  ): Either[String, SyncResult] =
    def loop(adopted: List[FederationReplica.AdoptedGeneration], remaining: Int): Either[String, SyncResult] =
      if remaining <= 0 then Right(SyncResult(adopted.reverse, replica.finalizedCursor))
      else
        val cursor = replica.finalizedCursor
        nextVerifiedCertificate(replica, peerUrls, cursor) match
          case None => Right(SyncResult(adopted.reverse, cursor))
          case Some(verified) =>
            adoptWithClosure(replica, node, peerUrls, verified, fetchRetries) match
              case Left(e) => Left(e)
              case Right(gen) => loop(gen :: adopted, remaining - 1)
    loop(Nil, maxGenerations)
