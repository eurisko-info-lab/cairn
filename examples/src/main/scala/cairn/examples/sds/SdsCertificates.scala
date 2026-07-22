package cairn.examples.sds

import cairn.kernel.*
import cairn.systemhandler.{Branches, Ed25519, Keypair}

/** Approval / signing / publication certificates as CAS `certificate`
  * artifacts fully linked from [[BranchManifest.certificates]].
  *
  * Kind tags are Cairn judgments (`certificateKindOk` in
  * [[languages/sds-certificate.cairn]]); the workflow evidence chain module
  * ([[SdsCertificateKinds]]) is disk SoT. Host minting remains effectful
  * (Ed25519 / tip digests) — not Studio approval UI. Digests are
  * content-addressed and reachable from branch state alone.
  */
object SdsCertificates:
  enum Kind:
    case Approval, Signature, Publication

  def kindTag(k: Kind): String = k match
    case Kind.Approval     => "sds-approval"
    case Kind.Signature    => "sds-tip-signature"
    case Kind.Publication  => "sds-publication"

  private def requireKind(tag: String): Unit =
    if !SdsCertificateKinds.checkKind(tag) then
      throw RuntimeException(s"certificate kind '$tag' fails certificateKindOk")

  def mint(
      kind: Kind,
      issuer: String,
      tipDigest: Digest,
      extra: Map[String, String] = Map.empty
  ): Artifact =
    val tag = kindTag(kind)
    requireKind(tag)
    val fields = List(
      "kind" -> Canon.CStr(tag),
      "issuer" -> Canon.CStr(issuer),
      "tip" -> Canon.CStr(tipDigest.hex)) ++
      extra.toList.sortBy(_._1).map((k, v) => k -> Canon.CStr(v))
    Artifact(ArtifactKind.Certificate, Canon.cmap(fields*))

  def tipSignature(issuer: Keypair, tipDigest: Digest): Artifact =
    val tipSig = Ed25519.sign(issuer.privateKey, tipDigest.hex.getBytes("UTF-8"))
    val tipSigHex = tipSig.map(b => f"${b & 0xff}%02x").mkString
    mint(Kind.Signature, issuer.name, tipDigest, Map("sig" -> tipSigHex))

  def approval(issuer: String, tipDigest: Digest, note: String = "approved"): Artifact =
    mint(Kind.Approval, issuer, tipDigest, Map("note" -> note))

  def publication(issuer: String, tipDigest: Digest, branch: String): Artifact =
    mint(Kind.Publication, issuer, tipDigest, Map("branch" -> branch))

  /** Attach approval + tip-signature + publication certs; returns digests in order. */
  def attachWorkflow(
      branches: Branches,
      branch: String,
      issuer: Keypair,
      tipDigest: Digest
  ): Either[String, List[Digest]] =
    for
      r1 <- branches.attachCertificate(branch, approval(issuer.name, tipDigest))
      r2 <- branches.attachCertificate(branch, tipSignature(issuer, tipDigest))
      r3 <- branches.attachCertificate(branch, publication(issuer.name, tipDigest, branch))
    yield List(r1._2, r2._2, r3._2)
