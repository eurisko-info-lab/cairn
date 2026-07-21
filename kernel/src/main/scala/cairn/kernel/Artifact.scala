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
  case AgreementCertificate extends ArtifactKind("agreement-certificate")
  case BranchManifest extends ArtifactKind("branch-manifest")
  case ChangeSet      extends ArtifactKind("change-set")
  case Block          extends ArtifactKind("block")
  case Net            extends ArtifactKind("net")
  case RosettaDecl    extends ArtifactKind("rosetta-decl")
  case Transaction    extends ArtifactKind("transaction")
  case Identity       extends ArtifactKind("identity")
  case TacticScript   extends ArtifactKind("tactic-script")
  case Trace          extends ArtifactKind("trace")
  case Provenance     extends ArtifactKind("provenance")
  case Migration      extends ArtifactKind("migration")
  case ChunkedBlob    extends ArtifactKind("chunked-blob")
  case Capability     extends ArtifactKind("capability-manifest")
  case Policy         extends ArtifactKind("policy")
  case QueryResult    extends ArtifactKind("query-result")
  case ReplaySnapshot extends ArtifactKind("replay-snapshot")

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
  /** typeHash (M1): digest of the kind marker + full recursive structural
    * fingerprint of the body — any two artifact schemas that differ anywhere
    * in shape get distinct type hashes.
    */
  def typeHash: Digest = Digest.of(Canon.cmap(
    "kind" -> Canon.CStr(kind.name),
    "shape" -> TypeFingerprint.shape(body)))
  def key: TypedKey = TypedKey(kind, digest, typeHash)

/** M1: structural shape signature of a canonical value. Values collapse to
  * their type skeleton: primitives to names, lists to the set of distinct
  * member shapes, maps to their sorted field-name/shape table, tags to a
  * tagged shape. Same schema ⇒ same fingerprint; any structural difference
  * (extra field, changed tag, different nesting) ⇒ different fingerprint.
  */
object TypeFingerprint:
  import Canon.*
  def shape(c: Canon): Canon = c match
    case CInt(_)    => CStr("int")
    case CStr(_)    => CStr("str")
    case CBytes(_)  => CStr("bytes")
    case CList(xs)  =>
      val shapes = xs.map(shape).distinct.sortBy(s => new String(Canon.encode(s).map(_.toChar)))
      CTag("list", CList(shapes))
    case CMap(es)   => CTag("map", Canon.cmap(es.map((k, v) => k -> shape(v))*))
    case CTag(t, v) => CTag("tag:" + t, shape(v))
  def of(c: Canon): Digest = Digest.of(shape(c))

object Artifact:
  def fromCanon(c: Canon): Either[String, Artifact] =
    try
      import Canon.*
      for kind <- ArtifactKind.parse(c.field("kind").asStr)
      yield Artifact(kind, c.field("body"))
    catch case CodecError(m) => Left(m)
  def decode(bs: Array[Byte]): Either[String, Artifact] =
    Canon.decode(bs).flatMap(fromCanon)
