package cairn.runtime

import cairn.kernel.*
import cairn.core.*
import cairn.systeminterface.Cas
import cairn.systemhandler.{Ed25519, Keypair, Node, Signer}

object EcosystemBundles:
  def sign(
      namespace: String, version: SemanticVersion, root: Digest, rootKind: EcosystemRootKind,
      migrations: List[Digest], previous: List[Digest], signer: Signer,
  ): SignedEcosystemBundle =
    val release = EcosystemRelease(namespace, version, root, rootKind, migrations, previous,
      signer.name, signer.publicBytes)
    SignedEcosystemBundle(release, signer.sign(release.signingBytes))

  def publish(
      bundle: SignedEcosystemBundle, node: Node, authority: Keypair,
      authorities: Map[String, Vector[Byte]],
  ): Either[String, Block] =
    if authority.name != bundle.release.publisher then Left("publisher must append its own ecosystem bundle")
    else
      node.cas.put(bundle.artifact)
      node.append(authority, authorities, List(
        authority.signTx(Tx.RegisterIdentity(authority.name, authority.publicBytes)),
        authority.signTx(Tx.PublishArtifact(bundle.artifact.key))))

final class EcosystemRegistry(cas: Cas, policy: EcosystemTrustPolicy, published: Set[String]):
  private var releases = Map.empty[Digest, SignedEcosystemBundle]

  def ingest(digest: Digest): Either[String, SignedEcosystemBundle] = for
    artifact <- cas.getByDigest(digest)
    bundle <- SignedEcosystemBundle.fromArtifact(artifact)
    _ <- policy.check(bundle, published, Ed25519.verify)
    _ <- bundle.release.previous.foldLeft[Either[String, Unit]](Right(())) { (acc, previous) =>
      acc.flatMap(_ => Either.cond(previous != digest, (), "ecosystem release cannot cite itself")) }
    _ <- Either.cond(!releases.exists { (knownDigest, known) =>
      knownDigest != digest && known.release.namespace == bundle.release.namespace &&
        known.release.version == bundle.release.version }, (),
      s"equivocating release for ${bundle.release.namespace} ${bundle.release.version.render}")
    _ = releases += digest -> bundle
  yield bundle

  def versions(namespace: String): List[(SemanticVersion, Digest)] = releases.toList.collect {
    case (digest, bundle) if bundle.release.namespace == namespace => bundle.release.version -> digest
  }.sortBy(_._1)

  def latest(namespace: String): Option[SignedEcosystemBundle] =
    versions(namespace).lastOption.flatMap((_, digest) => releases.get(digest))

  /** Find a shortest declared migration route between language revisions. */
  def migrationRoute(namespace: String, from: Digest, to: Digest): Either[String, List[LangMigration]] =
    val edges = releases.values.filter(_.release.namespace == namespace).toList
      .flatMap(_.release.migrations).distinct.flatMap(d =>
        cas.getByDigest(d).toOption.flatMap(LangMigration.fromArtifact(_).toOption))
    def bfs(frontier: List[(Digest, List[LangMigration])], seen: Set[Digest]): Option[List[LangMigration]] = frontier match
      case Nil => None
      case (current, path) :: rest if current == to => Some(path)
      case (current, _) :: rest if seen(current) => bfs(rest, seen)
      case (current, path) :: rest =>
        val next = edges.filter(_.fromLang == current).map(m => m.toLang -> (path :+ m))
        bfs(rest ++ next, seen + current)
    bfs(List(from -> Nil), Set.empty).toRight(s"no trusted migration route from ${from.short} to ${to.short}")

  def all: Map[Digest, SignedEcosystemBundle] = releases

object EcosystemReplication:
  def pull(bundle: Digest, from: Cas, to: Cas): Either[String, Set[Digest]] =
    ArtifactApplicationResolver(to).install(bundle, from)
