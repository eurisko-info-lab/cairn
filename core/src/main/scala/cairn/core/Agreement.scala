package cairn.core

import cairn.kernel.*

/** Differential agreement between a Cairn reference computation and a
  * native (or recorded-native) counterpart.
  *
  * This is an **agreement envelope**, not a claim of full Lean-kernel or
  * HVM-runtime compatibility. See `docs/agreement.md`.
  *
  * Pipeline:
  * {{{
  * Cairn reference term
  *   → native representation
  *   → native execution/check  (live | golden | stub)
  *   → imported result
  *   → AgreementCertificate
  * }}}
  */
object Agreement:

  /** Named boundary: what we claim to agree on, and what we refuse to claim. */
  final case class Envelope(id: String, claims: List[String], excludes: List[String]):
    def canon: Canon = Canon.cmap(
      "id" -> Canon.CStr(id),
      "claims" -> Canon.CList(claims.map(Canon.CStr(_))),
      "excludes" -> Canon.CList(excludes.map(Canon.CStr(_))))
    def digest: Digest = Digest.of(canon)

  /** LeanCore ↔ native Lean 4 — closed fragment only. */
  val leanCore: Envelope = Envelope(
    id = "lean-core",
    claims = List(
      "Closed LeanCore fragment: sort0/sort1, Nat+zero/succ/natRec, Π/λ/app, Eq/refl/subst",
      "hasType(ctx, term, ty) via Cairn Checker agrees with projected Lean #check on the envelope corpus",
      "ι-rule subst(P, refl(a), px) ⇝ px matches Lean Eq.ndrec (Type-level; Eq.subst is Prop-sorted)",
      "ι-rules natRec(_, z, s, zero) ⇝ z and natRec on succ agree with Lean N.rec definitional equalities (id-Nat corpus)"),
    excludes = List(
      "Lean 4 kernel / elaborator / tactic-engine compatibility",
      "Lean surface syntax, import/export, mathlib",
      "Full CIC: universe polymorphism, user inductives, path-induction J, delta-unfolding defs",
      "Rosetta LeanPort theorem bodies (remain sorry obligations — §4.10)"))

  /** AffineNet/IcNet ↔ classical IC / HVM lineage — rule table + corpus, not ABI. */
  val hvmIc: Envelope = Envelope(
    id = "hvm-ic",
    claims = List(
      "AffineNet: γ/ε annihilation and erasure of the classical IC affine fragment",
      "IcNet: γγ/δδ annihilate, γδ commute, ε erases, δ copies labelled konst",
      "Lowered λ-net normal forms agree with recorded classical-IC outcomes on the envelope corpus",
      "HvmSurface projects the corpus to HVM2 CON/DUP/ERA books; live hvm run agrees when present"),
    excludes = List(
      "HVM / HVM5 runtime ABI, memory layout, or full CLI wire compatibility",
      "HVM2 NUM/OPR/SWI ops; Bend / HVM5 / Kind surface syntax",
      "HVM strict/lazy modes; numbers/recursion primitives beyond the corpus",
      "Full Bend / Kind / QDIC surfaces",
      "Opaque labelled konst ↔ Church @True/@False isomorphism outside the corpus"))

  /** Where the native-side digest came from. */
  enum NativeSource:
    case Golden
    case Live(tool: String, detail: String)
    case Stub(reason: String)
    def label: String = this match
      case Golden              => "golden"
      case Live(tool, detail)  => s"live:$tool:$detail"
      case Stub(reason)        => s"stub:$reason"

  /** Kernel-shaped record that Cairn and native results agree (or honestly don't).
    *
    * [[envelopeDigest]] pins the envelope claims/excludes; [[nativeEvidence]]
    * records native-run detail (tool output digest / golden id) beyond the
    * source label so live vs golden vs stub are distinguishable in canon.
    */
  final case class AgreementCertificate(
      envelopeId: String,
      envelopeDigest: Digest,
      caseName: String,
      subject: Digest,
      cairnResult: Digest,
      nativeResult: Digest,
      source: String,
      nativeEvidence: Digest,
      agreed: Boolean
  ):
    def canon: Canon = Canon.cmap(
      "envelope" -> Canon.CStr(envelopeId),
      "envelopeDigest" -> Canon.CStr(envelopeDigest.hex),
      "case" -> Canon.CStr(caseName),
      "subject" -> Canon.CStr(subject.hex),
      "cairn" -> Canon.CStr(cairnResult.hex),
      "native" -> Canon.CStr(nativeResult.hex),
      "source" -> Canon.CStr(source),
      "nativeEvidence" -> Canon.CStr(nativeEvidence.hex),
      "agreed" -> Canon.CStr(if agreed then "true" else "false"))
    def artifact: Artifact = Artifact(ArtifactKind.AgreementCertificate, canon)

  /** Outcome digest shared by Cairn-side and native-side adapters. */
  def outcome(status: String, detail: Canon = Canon.CStr("")): Digest =
    Digest.of(Canon.cmap("status" -> Canon.CStr(status), "detail" -> detail))

  def evidenceFor(source: NativeSource): Digest = source match
    case NativeSource.Golden =>
      outcome("golden")
    case NativeSource.Live(tool, detail) =>
      outcome("live", Canon.cmap("tool" -> Canon.CStr(tool), "detail" -> Canon.CStr(detail)))
    case NativeSource.Stub(reason) =>
      outcome("stub", Canon.CStr(reason))

  /** Validate certificate internal consistency, then accept only if agreed. */
  def check(
      cert: AgreementCertificate,
      envelope: Option[Envelope] = None
  ): Either[String, AgreementCertificate] =
    val matchOk = cert.cairnResult == cert.nativeResult
    if matchOk != cert.agreed then
      Left(s"${cert.caseName}: agreed=${cert.agreed} but digests ${
          if matchOk then "match" else "differ"}")
    else if !cert.agreed then
      Left(s"${cert.caseName}: Cairn ${cert.cairnResult.short} ≠ native ${cert.nativeResult.short} (source=${cert.source})")
    else envelope match
      case Some(env) if env.id != cert.envelopeId =>
        Left(s"${cert.caseName}: envelope id mismatch")
      case Some(env) if env.digest != cert.envelopeDigest =>
        Left(s"${cert.caseName}: envelope digest mismatch (claims/excludes drift)")
      case _ => Right(cert)

  /** Issue + check in one step. */
  def certify(
      envelope: Envelope,
      caseName: String,
      subject: Digest,
      cairnResult: Digest,
      nativeResult: Digest,
      source: NativeSource
  ): Either[String, AgreementCertificate] =
    val agreed = cairnResult == nativeResult
    check(AgreementCertificate(
      envelope.id, envelope.digest, caseName, subject, cairnResult, nativeResult,
      source.label, evidenceFor(source), agreed), Some(envelope))
