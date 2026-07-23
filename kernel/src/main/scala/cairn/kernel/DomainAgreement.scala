package cairn.kernel

/** Certified claim that a domain child may hang under a primary ancestor (and
  * soft references) with an owning authority and language-dependency evidence.
  *
  * The domain graph alone is structurally validated ([[DomainBranch]]); a
  * [[DomainAgreement]] is the *governance* object: who may plant the name,
  * which language digests the child/ancestors claim, and whether an ancestry
  * change is an allowed amendment of a prior agreement.
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
):
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
        replaces = m.get("replaces").flatMap(dig)))
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
        else Right(())
      }

  /** Ancestry-change policy: first plant has no `replaces`; amendments must
    * cite the live agreement digest, keep the same owner, and keep the same
    * child name. Primary/refs may change only under that amendment.
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
