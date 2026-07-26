package cairn.core

import cairn.kernel.*

final case class SemanticVersion(major: Int, minor: Int, patch: Int) extends Ordered[SemanticVersion]:
  require(major >= 0 && minor >= 0 && patch >= 0, "semantic version components must be non-negative")
  def render: String = s"$major.$minor.$patch"
  def compare(other: SemanticVersion): Int = Ordering[(Int, Int, Int)].compare(
    (major, minor, patch), (other.major, other.minor, other.patch))

object SemanticVersion:
  def parse(value: String): Either[String, SemanticVersion] = value.split("\\.", -1).toList match
    case a :: b :: c :: Nil =>
      for
        major <- a.toIntOption.toRight(s"invalid semantic version '$value'")
        minor <- b.toIntOption.toRight(s"invalid semantic version '$value'")
        patch <- c.toIntOption.toRight(s"invalid semantic version '$value'")
        version <- scala.util.Try(SemanticVersion(major, minor, patch)).toEither.left.map(_.getMessage)
      yield version
    case _ => Left(s"invalid semantic version '$value'")

enum EcosystemRootKind:
  case Pack, Application

final case class EcosystemRelease(
    namespace: String,
    version: SemanticVersion,
    root: Digest,
    rootKind: EcosystemRootKind,
    migrations: List[Digest],
    previous: List[Digest],
    publisher: String,
    publicKey: Vector[Byte],
):
  def canon: Canon = Canon.cmap(
    "domain" -> Canon.CStr("cairn-ecosystem-release-v1"),
    "namespace" -> Canon.CStr(namespace), "version" -> Canon.CStr(version.render),
    "root" -> Canon.CStr(root.hex), "rootKind" -> Canon.CStr(rootKind.toString),
    "migrations" -> Canon.cstrs(migrations.distinct.sortBy(_.hex).map(_.hex)),
    "previous" -> Canon.cstrs(previous.distinct.sortBy(_.hex).map(_.hex)),
    "publisher" -> Canon.CStr(publisher), "publicKey" -> Canon.CBytes(publicKey))
  def signingBytes: Array[Byte] = Canon.encode(canon)

final case class SignedEcosystemBundle(release: EcosystemRelease, signature: Vector[Byte]):
  def canon: Canon = Canon.cmap("release" -> release.canon, "signature" -> Canon.CBytes(signature))
  def artifact: Artifact = Artifact(ArtifactKind.EcosystemBundle, canon)
  def digest: Digest = artifact.digest

object SignedEcosystemBundle:
  private def bytes(c: Canon): Vector[Byte] = c match
    case Canon.CBytes(value) => value
    case _ => throw CodecError("expected bytes")
  def fromArtifact(artifact: Artifact): Either[String, SignedEcosystemBundle] =
    if artifact.kind != ArtifactKind.EcosystemBundle then Left("artifact is not an ecosystem bundle")
    else try
      val r = artifact.body.field("release")
      if r.field("domain").asStr != "cairn-ecosystem-release-v1" then Left("wrong ecosystem release domain")
      else for
        version <- SemanticVersion.parse(r.field("version").asStr)
        rootKind <- EcosystemRootKind.values.find(_.toString == r.field("rootKind").asStr)
          .toRight("unknown ecosystem root kind")
      yield SignedEcosystemBundle(EcosystemRelease(
        r.field("namespace").asStr, version, Digest(r.field("root").asStr), rootKind,
        r.field("migrations").asList.map(x => Digest(x.asStr)),
        r.field("previous").asList.map(x => Digest(x.asStr)),
        r.field("publisher").asStr, bytes(r.field("publicKey"))), bytes(artifact.body.field("signature")))
    catch case e: Exception => Left(s"invalid ecosystem bundle: ${e.getMessage}")

final case class EcosystemTrustPolicy(
    publishers: Map[String, Vector[Byte]],
    namespaceOwners: Map[String, Set[String]],
    revokedBundles: Set[Digest] = Set.empty,
    requirePublished: Boolean = true,
):
  def check(bundle: SignedEcosystemBundle, published: Set[String], verify: (Vector[Byte], Array[Byte], Vector[Byte]) => Boolean): Either[String, Unit] =
    for
      trusted <- publishers.get(bundle.release.publisher).toRight(s"untrusted publisher '${bundle.release.publisher}'")
      _ <- Either.cond(trusted == bundle.release.publicKey, (), "publisher key does not match trust policy")
      owners <- namespaceOwners.get(bundle.release.namespace).toRight(s"untrusted namespace '${bundle.release.namespace}'")
      _ <- Either.cond(owners.contains(bundle.release.publisher), (), "publisher does not own this namespace")
      _ <- Either.cond(!revokedBundles.contains(bundle.digest), (), "ecosystem bundle is revoked")
      _ <- Either.cond(verify(trusted, bundle.release.signingBytes, bundle.signature), (), "bad ecosystem bundle signature")
      _ <- Either.cond(!requirePublished || published.contains(bundle.artifact.key.render), (), "ecosystem bundle is not ledger-published")
    yield ()
