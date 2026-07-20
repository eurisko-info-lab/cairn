package cairn.kernel

import java.security.MessageDigest

/** L0 dual identity (S3, §2 Key/digest, §4.5).
  *
  * - [[Digest]] — content hash of canonical bytes: storage/dedup identity.
  * - [[TypedKey]] — stable semantic key: valueHash + typeHash + kind. Long-lived
  *   references use TypedKey, never a bare untyped hash.
  */
final case class Digest(hex: String):
  require(hex.length == 64, s"digest must be 64 hex chars, got ${hex.length}")
  def short: String = hex.take(12)
  override def toString: String = hex

object Digest:
  def ofBytes(bs: Array[Byte]): Digest =
    val md = MessageDigest.getInstance("SHA-256")
    Digest(md.digest(bs).map(b => f"${b & 0xff}%02x").mkString)
  def of(c: Canon): Digest = ofBytes(Canon.encode(c))
  def parse(s: String): Either[String, Digest] =
    if s.length == 64 && s.forall(ch => ch.isDigit || ('a' to 'f').contains(ch)) then Right(Digest(s))
    else Left(s"not a sha-256 hex digest: '$s'")

/** Artifact kinds (S5, §2 Artifact). Domain examples never extend this in L0. */
enum ArtifactKind(val name: String):
  case Language       extends ArtifactKind("language")
  case Fragment       extends ArtifactKind("fragment")
  case Grammar        extends ArtifactKind("grammar")
  case Source         extends ArtifactKind("source")
  case Ir             extends ArtifactKind("ir")
  case Term           extends ArtifactKind("term")
  case VmImage        extends ArtifactKind("vm-image")
  case Claim          extends ArtifactKind("claim")
  case Axiom          extends ArtifactKind("axiom")
  case Lemma          extends ArtifactKind("lemma")
  case Theorem        extends ArtifactKind("theorem")
  case ProofTerm      extends ArtifactKind("proof-term")
  case TestSuite      extends ArtifactKind("test-suite")
  case Certificate    extends ArtifactKind("certificate")
  case BranchManifest extends ArtifactKind("branch-manifest")
  case ChangeSet      extends ArtifactKind("change-set")
  case Block          extends ArtifactKind("block")
  case Net            extends ArtifactKind("net")
  case RosettaDecl    extends ArtifactKind("rosetta-decl")
  case Transaction    extends ArtifactKind("transaction")
  case Identity       extends ArtifactKind("identity")

object ArtifactKind:
  def parse(s: String): Either[String, ArtifactKind] =
    values.find(_.name == s).toRight(s"unknown artifact kind '$s'")

/** Stable semantic key: content identity refined by type identity (§4.5). */
final case class TypedKey(kind: ArtifactKind, valueHash: Digest, typeHash: Digest):
  def render: String = s"${kind.name}:${valueHash.hex}:${typeHash.hex}"

object TypedKey:
  /** Structured error on kind/type mismatch instead of silent acceptance. */
  def check(expected: TypedKey, actual: TypedKey): Either[String, Unit] =
    if expected == actual then Right(())
    else if expected.valueHash == actual.valueHash then
      Left(s"typed-key mismatch on ${expected.valueHash.short}: expected kind=${expected.kind.name} type=${expected.typeHash.short}, got kind=${actual.kind.name} type=${actual.typeHash.short}")
    else Left(s"value hash mismatch: expected ${expected.valueHash.short}, got ${actual.valueHash.short}")

/** Typed, immutable, content-addressed envelope (S5). */
final case class Artifact(kind: ArtifactKind, body: Canon):
  def canon: Canon = Canon.cmap("kind" -> Canon.CStr(kind.name), "body" -> body)
  def digest: Digest = Digest.of(canon)
  /** typeHash: digest of the kind marker + structural tag of the body head. */
  def typeHash: Digest = Digest.of(Canon.cmap(
    "kind" -> Canon.CStr(kind.name),
    "shape" -> Canon.CStr(body match
      case Canon.CTag(t, _) => t
      case other            => other.getClass.getSimpleName)))
  def key: TypedKey = TypedKey(kind, digest, typeHash)

object Artifact:
  def fromCanon(c: Canon): Either[String, Artifact] =
    try
      import Canon.*
      for kind <- ArtifactKind.parse(c.field("kind").asStr)
      yield Artifact(kind, c.field("body"))
    catch case CodecError(m) => Left(m)
  def decode(bs: Array[Byte]): Either[String, Artifact] =
    Canon.decode(bs).flatMap(fromCanon)
