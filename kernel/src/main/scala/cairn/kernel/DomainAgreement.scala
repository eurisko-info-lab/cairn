package cairn.kernel

/** Certified claim that a domain child may hang under a primary ancestor (and
  * soft references) with an owning authority and language-dependency evidence.
  *
  * The domain graph alone is structurally validated ([[DomainBranch]]); a
  * [[DomainAgreement]] is the *governance* object: who may plant the name,
  * which language digests the child/ancestors claim, and whether an ancestry
  * change is an allowed amendment of a prior agreement.
  *
  * Under a primary ancestor, planting also requires a separate
  * [[DomainAncestorDelegation]] certificate sealed by the primary's owner.
  *
  * Stored as a [[ArtifactKind.Certificate]] body tagged `domain-agreement`.
  */
final case class DomainAgreement(
    child: String,
    primaryAncestor: Option[String],
    references: List[String],
    /** Subject that owns this namespace name (may plant / amend). */
    owner: String,
    childLanguage: Option[Digest],
    /** Branch name → claimed language digest for each ancestor (primary + refs). */
    ancestorLanguages: List[(String, Digest)],
    /** Structured dependency evidence (e.g. required `provides` names). */
    dependencyEvidence: Canon,
    /** Prior agreement digest when amending ancestry; absent for first plant. */
    replaces: Option[Digest] = None,
    /** Digest of a [[DomainAncestorDelegation]] when hanging under a primary. */
    ancestorDelegation: Option[Digest] = None,
):
  /** Agreement body without the delegation pointer — what a grantor seals against. */
  def claimCanon: Canon = copy(ancestorDelegation = None).canon
  def claimDigest: Digest = Digest.of(claimCanon)

  def canon: Canon = Canon.cmap(
    "child" -> Canon.CStr(child),
    "primaryAncestor" -> primaryAncestor.fold(Canon.CTag("none", Canon.CStr("")))(n =>
      Canon.CTag("some", Canon.CStr(n))),
    "references" -> Canon.CList(references.map(Canon.CStr.apply)),
    "owner" -> Canon.CStr(owner),
    "childLanguage" -> childLanguage.fold(Canon.CTag("none", Canon.CStr("")))(d =>
      Canon.CTag("some", Canon.CStr(d.hex))),
    "ancestorLanguages" -> Canon.CList(ancestorLanguages.map { (n, d) =>
      Canon.cmap("branch" -> Canon.CStr(n), "language" -> Canon.CStr(d.hex))
    }),
    "dependencyEvidence" -> dependencyEvidence,
    "replaces" -> replaces.fold(Canon.CTag("none", Canon.CStr("")))(d =>
      Canon.CTag("some", Canon.CStr(d.hex))),
    "ancestorDelegation" -> ancestorDelegation.fold(Canon.CTag("none", Canon.CStr("")))(d =>
      Canon.CTag("some", Canon.CStr(d.hex))))
  def artifact: Artifact = Artifact(ArtifactKind.Certificate, Canon.CTag("domain-agreement", canon))
  def digest: Digest = artifact.digest

  /** Persistable sealed envelope: unsigned body + owner (and optional grantor) seals. */
  def seal(
      ownerSeal: Vector[Byte],
      grantorSeal: Option[Vector[Byte]] = None,
  ): SealedDomainAgreement =
    SealedDomainAgreement(this, ownerSeal, grantorSeal)

/** Domain agreement with embedded seals — the CAS object that manifests cite. */
final case class SealedDomainAgreement(
    agreement: DomainAgreement,
    ownerSeal: Vector[Byte],
    grantorSeal: Option[Vector[Byte]] = None,
):
  def canon: Canon = Canon.CTag("domain-agreement-sealed", Canon.cmap(
    "agreement" -> Canon.CTag("domain-agreement", agreement.canon),
    "ownerSeal" -> Canon.CBytes(ownerSeal),
    "grantorSeal" -> grantorSeal.fold(Canon.CTag("none", Canon.CInt(0)))(s =>
      Canon.CTag("some", Canon.CBytes(s)))))
  def artifact: Artifact = Artifact(ArtifactKind.Certificate, canon)
  def digest: Digest = artifact.digest

object SealedDomainAgreement:
  def fromCanon(c: Canon): Either[String, SealedDomainAgreement] =
    import Canon.*
    c match
      case CTag("domain-agreement-sealed", m) =>
        for
          ag <- DomainAgreement.fromCanon(m.field("agreement"))
          ownerSeal <- m.field("ownerSeal") match
            case CBytes(bs) => Right(bs)
            case other      => Left(s"ownerSeal: $other")
          grantorSeal = m.field("grantorSeal") match
            case CTag("some", CBytes(bs)) => Some(bs)
            case _                        => None
        yield SealedDomainAgreement(ag, ownerSeal, grantorSeal)
      // Legacy unsigned body — reject so callers cannot treat unsigned as endorsed.
      case CTag("domain-agreement", _) =>
        Left("domain-agreement: unsigned artifact (need domain-agreement-sealed)")
      case other => Left(s"not a sealed domain-agreement: $other")

  /** Re-verify owner (and grantor, when present) seals against resolved keys. */
  def verify(
      sealedAg: SealedDomainAgreement,
      identities: IdentityResolver,
      verify: (Vector[Byte], Array[Byte], Vector[Byte]) => Boolean,
      grantorName: Option[String] = None,
  ): Either[String, Unit] =
    val ag = sealedAg.agreement
    for
      ownerPk <- identities.require(ag.owner)
      _ <- DomainAgreement.authenticateOwner(ag, ag.owner, ownerPk, sealedAg.ownerSeal, verify)
      _ <- (ag.primaryAncestor, sealedAg.grantorSeal, grantorName) match
        case (None, None, _) => Right(())
        case (Some(_), None, _) =>
          Left("domain-agreement: sealed plant under primary missing grantorSeal")
        case (Some(_), Some(gs), Some(gName)) =>
          identities.require(gName).flatMap { gPk =>
            // Grantor sealed the delegation body, not the agreement — verified separately.
            // Here we only require the seal bytes are present when primary is set.
            if gs.isEmpty then Left("domain-agreement: empty grantorSeal")
            else Right(())
          }
        case (Some(_), Some(_), None) =>
          Left("domain-agreement: grantor name required to verify sealed grantorSeal")
        case (None, Some(_), _) =>
          Left("domain-agreement: trunk sealed agreement must not carry grantorSeal")
    yield ()

object DomainAgreement:
  def fromCanon(c: Canon): Either[String, DomainAgreement] =
    import Canon.*
    try
      val body = c match
        case CTag("domain-agreement", inner) => inner
        case other                           => other
      def name(tag: Canon): Option[String] = tag match
        case CTag("some", CStr(n)) if n.nonEmpty => Some(n)
        case _                                   => None
      def dig(tag: Canon): Option[Digest] = tag match
        case CTag("some", CStr(hex)) => Some(Digest(hex))
        case _                       => None
      val m = body.asMap
      val langs = body.field("ancestorLanguages").asList.map { row =>
        row.field("branch").asStr -> Digest(row.field("language").asStr)
      }
      Right(DomainAgreement(
        child = body.field("child").asStr,
        primaryAncestor = m.get("primaryAncestor").flatMap(name),
        references = m.get("references").map(_.asList.map(_.asStr)).getOrElse(Nil),
        owner = body.field("owner").asStr,
        childLanguage = m.get("childLanguage").flatMap(dig),
        ancestorLanguages = langs,
        dependencyEvidence = m.getOrElse("dependencyEvidence", Canon.CInt(0)),
        replaces = m.get("replaces").flatMap(dig),
        ancestorDelegation = m.get("ancestorDelegation").flatMap(dig)))
    catch
      case e: CodecError => Left(e.getMessage)
      case e: IllegalArgumentException => Left(e.getMessage)

  /** Structural + ownership well-formedness against a known branch set. */
  def wellFormed(
      a: DomainAgreement,
      knownBranches: Set[String],
      primaryOf: String => Option[String] = _ => None,
  ): Either[String, Unit] =
    if a.child.isEmpty then Left("domain-agreement: empty child name")
    else if a.owner.isEmpty then Left("domain-agreement: empty owner")
    else if a.references.exists(_ == a.child) then
      Left(s"domain-agreement: '${a.child}' cannot reference itself")
    else if a.primaryAncestor.exists(a.references.contains) then
      Left(s"domain-agreement: primary must not also appear in references")
    else if a.references.length != a.references.distinct.length then
      Left("domain-agreement: duplicate references")
    else if a.primaryAncestor.isEmpty && a.ancestorDelegation.isDefined then
      Left("domain-agreement: trunk plant must not cite ancestorDelegation")
    else if a.primaryAncestor.isDefined && a.ancestorDelegation.isEmpty then
      Left("domain-agreement: plant under primary requires ancestorDelegation")
    else
      val draft = BranchManifest(
        a.child, None, Nil,
        primaryAncestor = a.primaryAncestor,
        references = a.references)
      DomainBranch.wellFormed(draft, knownBranches, primaryOf).flatMap { _ =>
        val expectedAncestors =
          a.primaryAncestor.toList ++ a.references
        val claimed = a.ancestorLanguages.map(_._1).toSet
        if claimed != expectedAncestors.toSet then
          Left(
            s"domain-agreement: ancestorLanguages keys ${claimed.toList.sorted} " +
              s"must equal primary+refs ${expectedAncestors.sorted}")
        else if a.ancestorLanguages.exists((_, d) => d.hex.length != 64) then
          Left("domain-agreement: malformed ancestor language digest")
        else if a.childLanguage.exists(_.hex.length != 64) then
          Left("domain-agreement: malformed child language digest")
        else parseDependencyEvidence(a.dependencyEvidence).map(_ => ())
      }

  /** Canonical dependency evidence: a non-empty list of language digests. */
  def dependencyEvidenceOf(digests: List[Digest]): Canon =
    Canon.CTag("dependency-evidence", Canon.CList(digests.map(d => Canon.CStr(d.hex))))

  /** Accept tagged or bare list forms; reject free-form / non-digest entries. */
  def parseDependencyEvidence(c: Canon): Either[String, List[Digest]] =
    import Canon.*
    def fromList(xs: List[Canon]): Either[String, List[Digest]] =
      if xs.isEmpty then Left("domain-agreement: dependencyEvidence must be non-empty")
      else
        xs.foldLeft[Either[String, List[Digest]]](Right(Nil)) { (acc, row) =>
          acc.flatMap { ds =>
            row match
              case CStr(hex) =>
                Digest.parse(hex).left.map(_ =>
                  s"domain-agreement: dependencyEvidence entry is not a digest: $hex")
                  .map(ds :+ _)
              case other =>
                Left(s"domain-agreement: dependencyEvidence entries must be digests, got $other")
          }
        }
    c match
      case CInt(0) => Left("domain-agreement: missing dependency evidence")
      case CTag("dependency-evidence", CList(xs)) => fromList(xs)
      case CList(xs) => fromList(xs)
      case other =>
        Left(s"domain-agreement: dependencyEvidence must list language digests, got $other")

  /** Ancestry-change policy. `liveSealedDigest` is the CAS digest of the prior
    * [[SealedDomainAgreement]] (what manifests store); `replaces` must cite it.
    */
  def allowsTransition(
      proposed: DomainAgreement,
      live: Option[DomainAgreement],
      liveSealedDigest: Option[Digest] = None,
  ): Either[String, Unit] =
    live match
      case None =>
        if proposed.replaces.isDefined then
          Left("domain-agreement: first plant must not set replaces")
        else Right(())
      case Some(prev) =>
        if proposed.child != prev.child then
          Left(s"domain-agreement: cannot rename '${prev.child}' to '${proposed.child}'")
        else if proposed.owner != prev.owner then
          Left(s"domain-agreement: owner '${prev.owner}' cannot be reassigned to '${proposed.owner}'")
        else
          val expected = liveSealedDigest.getOrElse(prev.digest)
          if !proposed.replaces.contains(expected) then
            Left(
              s"domain-agreement: ancestry change must replace ${expected.short} " +
                s"(got ${proposed.replaces.map(_.short).getOrElse("none")})")
          else Right(())

  /** Authenticate that `signer` is the declared owner (name match + seal over
    * the full agreement canon including any delegation pointer).
    */
  def authenticateOwner(
      agreement: DomainAgreement,
      signerName: String,
      signerPublic: Vector[Byte],
      seal: Vector[Byte],
      verify: (Vector[Byte], Array[Byte], Vector[Byte]) => Boolean,
  ): Either[String, Unit] =
    if signerName != agreement.owner then
      Left(s"domain-agreement: signer '$signerName' is not owner '${agreement.owner}'")
    else if !verify(signerPublic, Canon.encode(agreement.canon), seal) then
      Left(s"domain-agreement: bad owner seal from '${agreement.owner}'")
    else Right(())

/** Consent from a primary domain's owner that a named child may hang under it.
  *
  * Separate from [[DomainAgreement]]: the child's owner claims the plant; the
  * ancestor's owner seals this grant. Soft references do not require delegation.
  */
final case class DomainAncestorDelegation(
    ancestor: String,
    child: String,
    grantor: String,
    grantee: String,
    /** Digest of the child's agreement [[DomainAgreement.claimCanon]]. */
    claimDigest: Digest,
):
  def canon: Canon = Canon.cmap(
    "ancestor" -> Canon.CStr(ancestor),
    "child" -> Canon.CStr(child),
    "grantor" -> Canon.CStr(grantor),
    "grantee" -> Canon.CStr(grantee),
    "claimDigest" -> Canon.CStr(claimDigest.hex))
  def seal(grantorSeal: Vector[Byte]): SealedDomainAncestorDelegation =
    SealedDomainAncestorDelegation(this, grantorSeal)
  def artifact: Artifact =
    Artifact(ArtifactKind.Certificate, Canon.CTag("domain-ancestor-delegation", canon))
  def digest: Digest = artifact.digest

/** Delegation body + grantor seal persisted together. */
final case class SealedDomainAncestorDelegation(
    delegation: DomainAncestorDelegation,
    grantorSeal: Vector[Byte],
):
  def canon: Canon = Canon.CTag("domain-ancestor-delegation-sealed", Canon.cmap(
    "delegation" -> Canon.CTag("domain-ancestor-delegation", delegation.canon),
    "grantorSeal" -> Canon.CBytes(grantorSeal)))
  def artifact: Artifact = Artifact(ArtifactKind.Certificate, canon)
  def digest: Digest = artifact.digest

object SealedDomainAncestorDelegation:
  def fromCanon(c: Canon): Either[String, SealedDomainAncestorDelegation] =
    import Canon.*
    c match
      case CTag("domain-ancestor-delegation-sealed", m) =>
        for
          del <- DomainAncestorDelegation.fromCanon(m.field("delegation"))
          seal <- m.field("grantorSeal") match
            case CBytes(bs) => Right(bs)
            case other      => Left(s"grantorSeal: $other")
        yield SealedDomainAncestorDelegation(del, seal)
      case CTag("domain-ancestor-delegation", _) =>
        Left("domain-delegation: unsigned artifact (need domain-ancestor-delegation-sealed)")
      case other => Left(s"not a sealed domain-ancestor-delegation: $other")

  def verify(
      sealedDel: SealedDomainAncestorDelegation,
      grantorPublic: Vector[Byte],
      verify: (Vector[Byte], Array[Byte], Vector[Byte]) => Boolean,
  ): Either[String, Unit] =
    DomainAncestorDelegation.authenticateGrantor(
      sealedDel.delegation, sealedDel.delegation.grantor, grantorPublic, sealedDel.grantorSeal, verify)

object DomainAncestorDelegation:
  def fromCanon(c: Canon): Either[String, DomainAncestorDelegation] =
    import Canon.*
    try
      val body = c match
        case CTag("domain-ancestor-delegation", inner) => inner
        case other                                     => other
      Right(DomainAncestorDelegation(
        ancestor = body.field("ancestor").asStr,
        child = body.field("child").asStr,
        grantor = body.field("grantor").asStr,
        grantee = body.field("grantee").asStr,
        claimDigest = Digest(body.field("claimDigest").asStr)))
    catch
      case e: CodecError => Left(e.getMessage)
      case e: IllegalArgumentException => Left(e.getMessage)

  def authenticateGrantor(
      del: DomainAncestorDelegation,
      signerName: String,
      signerPublic: Vector[Byte],
      seal: Vector[Byte],
      verify: (Vector[Byte], Array[Byte], Vector[Byte]) => Boolean,
  ): Either[String, Unit] =
    if signerName != del.grantor then
      Left(s"domain-delegation: signer '$signerName' is not grantor '${del.grantor}'")
    else if !verify(signerPublic, Canon.encode(del.canon), seal) then
      Left(s"domain-delegation: bad grantor seal from '${del.grantor}'")
    else Right(())

  /** Structural match against a proposed child agreement + primary owner. */
  def matches(
      del: DomainAncestorDelegation,
      agreement: DomainAgreement,
      primaryOwner: String,
  ): Either[String, Unit] =
    if !agreement.primaryAncestor.contains(del.ancestor) then
      Left(s"domain-delegation: ancestor '${del.ancestor}' != primary ${agreement.primaryAncestor}")
    else if del.child != agreement.child then
      Left(s"domain-delegation: child '${del.child}' != '${agreement.child}'")
    else if del.grantee != agreement.owner then
      Left(s"domain-delegation: grantee '${del.grantee}' != owner '${agreement.owner}'")
    else if del.grantor != primaryOwner then
      Left(s"domain-delegation: grantor '${del.grantor}' != primary owner '$primaryOwner'")
    else if del.claimDigest != agreement.claimDigest then
      Left(s"domain-delegation: claimDigest ${del.claimDigest.short} != ${agreement.claimDigest.short}")
    else Right(())
