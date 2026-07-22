package cairn.kernel

/** Kernel structural gate for branch-linked certificates.
  *
  * Opacity / kind tags alone are not enough: [[Branches.attachCertificate]]
  * must reject free-form or tip-mismatched certificate bodies before they
  * become privileged branch state. Cryptographic signature verification
  * (Ed25519) remains a system-handler concern on top of this check.
  */
object CertificateAttach:

  /** Accept only Certificate / AgreementCertificate kinds with required
    * fields. When [[expectedTip]] is set, the certificate's `tip` field must
    * equal it (forgery of an unrelated tip digest is rejected).
    */
  def check(cert: Artifact, expectedTip: Option[Digest] = None): Either[String, Unit] =
    cert.kind match
      case ArtifactKind.Certificate =>
        checkCertificateBody(cert.body, expectedTip)
      case ArtifactKind.AgreementCertificate =>
        checkAgreementBody(cert.body)
      case other =>
        Left(s"attachCertificate expects certificate or agreement-certificate, got ${other.name}")

  private def checkCertificateBody(body: Canon, expectedTip: Option[Digest]): Either[String, Unit] =
    try
      val kind = body.field("kind").asStr
      if kind.isEmpty then Left("certificate missing kind")
      else
        val tipHex = body.field("tip").asStr
        Digest.parse(tipHex) match
          case Left(e) => Left(s"certificate tip: $e")
          case Right(tip) =>
            val issuer = body.field("issuer").asStr
            if issuer.isEmpty then Left("certificate missing issuer")
            else expectedTip match
              case Some(exp) if tip != exp =>
                Left(s"certificate tip ${tip.short} does not match branch head ${exp.short}")
              case _ => Right(())
    catch case CodecError(m) => Left(s"certificate body: $m")

  private def checkAgreementBody(body: Canon): Either[String, Unit] =
    try
      val env = body.field("envelope").asStr
      val agreed = body.field("agreed").asStr
      if env.isEmpty then Left("agreement-certificate missing envelope")
      else if agreed != "true" then Left(s"agreement-certificate not agreed (agreed=$agreed)")
      else
        Digest.parse(body.field("cairn").asStr).flatMap { cairn =>
          Digest.parse(body.field("native").asStr).flatMap { native =>
            if cairn != native then
              Left(s"agreement-certificate digests differ: ${cairn.short} ≠ ${native.short}")
            else Right(())
          }
        }
    catch case CodecError(m) => Left(s"agreement-certificate body: $m")
