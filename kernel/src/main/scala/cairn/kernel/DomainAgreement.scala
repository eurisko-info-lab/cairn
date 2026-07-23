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
        else a.dependencyEvidence match
          case Canon.CInt(0) =>
            Left("domain-agreement: missing dependency evidence")
          case _ => Right(())
      }

  /** Ancestry-change policy: first plant has no `replaces`; amendments must
    * cite the live agreement digest, keep the same owner, and keep the same
    * child name. Primary/refs may change only under that amendment.
    *
    * When `live` cannot be decoded the caller must fail closed — this helper
    * only sees a successfully loaded prior agreement.
    */
  def allowsTransition(
      proposed: DomainAgreement,
      live: Option[DomainAgreement],
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
        else if !proposed.replaces.contains(prev.digest) then
          Left(
            s"domain-agreement: ancestry change must replace ${prev.digest.short} " +
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
  * ancestor's owner seals this grant. Stored as Certificate tagged
  * `domain-ancestor-delegation`. Soft references do not require delegation.
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
  def artifact: Artifact =
    Artifact(ArtifactKind.Certificate, Canon.CTag("domain-ancestor-delegation", canon))
  def digest: Digest = artifact.digest

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
