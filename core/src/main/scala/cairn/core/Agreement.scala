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
      "ι-rule subst(P, refl(a), px) ⇝ px matches Lean Eq.ndrec (Type-level; Eq.subst is Prop-sorted)"),
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
      "Lowered λ-net normal forms agree with recorded classical-IC outcomes on the envelope corpus"),
    excludes = List(
      "HVM / HVM5 runtime ABI, memory layout, or CLI wire compatibility",
      "HVM strict/lazy modes; numbers/recursion primitives",
      "Full Bend / Kind / QDIC surfaces"))

  /** Where the native-side digest came from. */
  enum NativeSource:
    case Golden
    case Live(tool: String, detail: String)
    case Stub(reason: String)
    def label: String = this match
      case Golden              => "golden"
      case Live(tool, detail)  => s"live:$tool:$detail"
      case Stub(reason)        => s"stub:$reason"

  /** Kernel-shaped record that Cairn and native results agree (or honestly don't). */
  final case class AgreementCertificate(
      envelopeId: String,
      caseName: String,
      subject: Digest,
      cairnResult: Digest,
      nativeResult: Digest,
      source: String,
      agreed: Boolean
  ):
    def canon: Canon = Canon.cmap(
      "envelope" -> Canon.CStr(envelopeId),
      "case" -> Canon.CStr(caseName),
      "subject" -> Canon.CStr(subject.hex),
      "cairn" -> Canon.CStr(cairnResult.hex),
      "native" -> Canon.CStr(nativeResult.hex),
      "source" -> Canon.CStr(source),
      "agreed" -> Canon.CStr(if agreed then "true" else "false"))
    def artifact: Artifact = Artifact(ArtifactKind.AgreementCertificate, canon)

  /** Outcome digest shared by Cairn-side and native-side adapters. */
  def outcome(status: String, detail: Canon = Canon.CStr("")): Digest =
    Digest.of(Canon.cmap("status" -> Canon.CStr(status), "detail" -> detail))

  /** Validate certificate internal consistency, then accept only if agreed. */
  def check(cert: AgreementCertificate): Either[String, AgreementCertificate] =
    val matchOk = cert.cairnResult == cert.nativeResult
    if matchOk != cert.agreed then
      Left(s"${cert.caseName}: agreed=${cert.agreed} but digests ${
          if matchOk then "match" else "differ"}")
    else if !cert.agreed then
      Left(s"${cert.caseName}: Cairn ${cert.cairnResult.short} ≠ native ${cert.nativeResult.short} (source=${cert.source})")
    else Right(cert)

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
      envelope.id, caseName, subject, cairnResult, nativeResult, source.label, agreed))
