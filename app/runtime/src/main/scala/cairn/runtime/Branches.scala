package cairn.runtime

import cairn.kernel.*
import cairn.core.*
import cairn.systeminterface.Cas
import cairn.systeminterface.PackAccess
import cairn.systemhandler.{EffectContext, Ed25519, Keypair}
import java.nio.file.Path

enum ConflictResolutionOutcome:
  case Deferred(resolution: Artifact, unresolved: List[ConflictDelta.Unresolved])
  case Accepted(manifest: BranchManifest, resolution: Artifact)

/** Semantic acceptance façade over a [[BranchRefStore]]: named branch refs,
  * with every head advance either ΔL-replayed and policy-checked
  * ([[commitTip]] / [[merge]] / [[mergeBranches]]) or an explicit,
  * governance-guarded bootstrap ([[importModule]]) — never a raw ref write.
  * `load`/`list` and the other ref-store primitives are plain delegations to
  * [[refs]]; this class adds ΔL/policy/domain-governance semantics on top.
  *
  * Merge-aware (M17): [[merge]] / [[mergeBranches]] run
  * [[cairn.core.SemanticRepository.integrate]] then either advance the target
  * head or persist the conflict artifact. [[commitTip]] accepts only
  * [[AcceptedTip]]; change-sets are replay-checked on load.
  * Causal digests are also written into [[BranchManifest]] (CAS-backed);
  * refs sidecars remain for compatibility.
  *
  * [[mergeBranches]] finds a causal LCA by shared module-result digests
  * (not only identical linear change prefixes), then merges divergent suffixes.
  *
  * Accepts use a journaled transactional path: CAS blobs → accept journal →
  * refs (history + tip + branch) → optional ledger publish → journal clear.
  * [[recoverPendingAccepts]] rolls forward interrupted accepts.
  * [[reclaimOrphanBlobs]] recovers then mark/sweeps CAS with `liveCasRoots`
  * (branch refs, change history, pending journals) — the reclaim path for
  * unreferenced accept blobs left by a crash before the journal was written.
  *
  * Ledger `SetBranchHead` remains opt-in via [[publishHead]] or
  * `publish = Some(...)` on merge.
  */
final class Branches(cas: Cas, refsDir: Path, ctx: EffectContext):
  private val refs = BranchRefStore(cas, refsDir, ctx)

  export refs.{load, list, liveCasRoots, reclaimOrphanBlobs, recoverPendingAccepts,
    nativeRepository, pullChanges, pushChanges, pullChangeArtifacts, pushChangeArtifacts}

  /** Publication is governed by the same constitution that accepted the
    * current head; the raw ref-store publisher is never exported. */
  def publishHead(
      branch: String,
      node: cairn.systemhandler.Node,
      authority: Keypair,
      authorities: Map[String, Vector[Byte]],
  ): Either[String, cairn.kernel.Block] =
    for
      evidenceDigest <- refs.load(branch).acceptanceEvidence.toRight(
        s"branch '$branch' has no acceptance evidence")
      evidenceArtifact <- refs.getByDigest(evidenceDigest)
      evidence <- AcceptanceEvidence.fromCanon(evidenceArtifact.body)
      constitutionDigest <- evidence.constitution.toRight(
        s"branch '$branch' was accepted without an acceptance constitution")
      constitutionArtifact <- refs.getByDigest(constitutionDigest)
      constitution <- AcceptanceConstitution.fromArtifact(constitutionArtifact)
      _ <- Either.cond(constitution.publicationRules != PublicationRules.Forbidden, (),
        "acceptance constitution forbids publication")
      _ <- Either.cond(
        constitution.authorityRules.required.isEmpty || constitution.authorityRules.required.contains(authority.name), (),
        s"authority '${authority.name}' is not allowed to publish by the acceptance constitution")
      block <- refs.publishHead(branch, node, authority, authorities)
    yield block

  private def acceptanceFacts(
      branch: String,
      migration: Option[(LangMigration, ComposedLanguage)],
      publish: Option[Publish],
  ): Either[String, AcceptanceFacts] =
    val manifest = refs.load(branch)
    manifest.certificates.foldLeft[Either[String, List[CertificateFact]]](Right(Nil)) { (acc, digest) =>
      for
        xs <- acc
        artifact <- refs.getByDigest(digest)
        _ <- CertificateAttach.check(artifact, manifest.head.map(_.valueHash))
        issuer = artifact.body match
          case Canon.CMap(fields) => fields.toMap.get("issuer").map(_.asStr)
          case _                  => None
      yield xs :+ CertificateFact(digest, artifact.kind, issuer)
    }.map { certificates =>
      AcceptanceFacts(
        domainAgreement = manifest.domainAgreement,
        certificates = certificates,
        authorities = publish.map(_.authority.name).toSet,
        migration = migration.map(_._1.artifact.digest),
        publicationRequested = publish.nonEmpty)
    }

  /** Load + replay-check a change-set artifact against `language`/`model`. */
  private def loadVcs(
      language: ComposedLanguage, digest: Digest, model: ChangeModel = ChangeModel.default,
  ): Either[String, Delta.ValidatedChangeSet] =
    refs.getByDigest(digest).flatMap { a =>
      if a.kind != ArtifactKind.ChangeSet then
        Left(s"digest ${digest.short} is ${a.kind.name}, not a change-set")
      else
        val claim = Delta.ValidatedChangeSet.decodeClaim(a.body)
        refs.getByDigest(claim.base).flatMap { baseArt =>
          if baseArt.kind != ArtifactKind.Ir then
            Left(s"base ${claim.base.short} is not a module")
          else Delta.ValidatedChangeSet.check(language, model, Module.fromCanon(baseArt.body), claim)
        }
    }

  /** Compose stacked ValidatedChangeSets into one change relative to the
    * oldest base. Entries must chain: `hist(i).result == hist(i+1).base`.
    */
  private def composeHistory(
      language: ComposedLanguage,
      hist: List[Delta.ValidatedChangeSet],
  ): Either[String, (Digest, Cst)] =
    if hist.isEmpty then Left("empty change history")
    else
      val broken = hist.zip(hist.tail).collectFirst {
        case (a, b) if a.result != b.base =>
          s"change history is not a causal chain: ${a.result.short} ↛ ${b.base.short}"
      }
      broken match
        case Some(err) => Left(err)
        case None =>
          val composed = hist.map(_.change).reduceLeft(ChangeAlgebra.compose(language, _, _))
          Right((hist.head.base, composed))

  /** Longest identical linear prefix (change-equal). Retained for diagnostics. */
  private def commonAncestorPrefix(
      a: List[Delta.ValidatedChangeSet],
      b: List[Delta.ValidatedChangeSet],
  ): Int =
    a.zip(b).takeWhile((x, y) => x.result == y.result && x.base == y.base && x.change == y.change).length

  /** Causal LCA by shared module-result digests: tip-proximal shared state,
    * even when the change objects that produced it differ (diamond / alternate
    * paths). Returns indices into `a`/`b` of the last shared result, or
    * `(-1,-1)` when only a shared starting base applies.
    */
  private def causalLca(
      a: List[Delta.ValidatedChangeSet],
      b: List[Delta.ValidatedChangeSet],
  ): (Int, Int) =
    val bByResult: Map[Digest, Int] =
      b.zipWithIndex.map((v, i) => v.result -> i).toMap
    a.zipWithIndex.reverse.collectFirst {
      case (v, i) if bByResult.contains(v.result) => (i, bByResult(v.result))
    }.getOrElse((-1, -1))

  /** Prefer [[PatchGraph]] DAG LCA when change-set digests form an explicit
    * parent graph; fall back to module-result [[causalLca]].
    */
  private def patchAwareLca(
      a: List[Delta.ValidatedChangeSet],
      b: List[Delta.ValidatedChangeSet],
  ): (Int, Int) =
    val fallback = causalLca(a, b)
    if a.isEmpty || b.isEmpty then fallback
    else
      val entriesA = a.map(v => (v.artifact.digest, v.base, v.result))
      val entriesB = b.map(v => (v.artifact.digest, v.base, v.result))
      val merged =
        PatchGraph.Graph.linear(entriesA).flatMap { g0 =>
          entriesB.zipWithIndex.foldLeft[Either[String, PatchGraph.Graph]](Right(g0)) {
            case (Left(e), _) => Left(e)
            case (Right(g), ((id, base, result), i)) =>
              if g.contains(id) then Right(g)
              else
                val parent = if i == 0 then None else Some(entriesB(i - 1)._1)
                val parents = parent.toList.filter(g.contains)
                g.add(PatchGraph.Node(id, parents, base, result))
          }
        }
      merged match
        case Left(_) => fallback
        case Right(g) =>
          g.lca(a.last.artifact.digest, b.last.artifact.digest) match
            case None => fallback
            case Some(lcaId) =>
              val iA = a.indexWhere(_.artifact.digest == lcaId)
              val iB = b.indexWhere(_.artifact.digest == lcaId)
              if iA >= 0 && iB >= 0 then (iA, iB) else fallback

  /** Pull a domain branch off the ledger trunk (`primary = None`) or off an
    * existing primary ancestor. Soft [[references]] name additional ancestors
    * (e.g. SDS primary=LAW, references=CHEMISTRY). Optionally seeds a tip via
    * [[importModule]]; domain fields survive subsequent advances.
    */
  private def primaryOf(known: Set[String])(name: String): Option[String] =
    if known.contains(name) then refs.load(name).primaryAncestor else None

  /** Plant or amend a domain branch under a [[DomainAgreement]].
    *
    * Cryptographic identity: `ownerSeal` is **required**. The public key is
    * resolved via [[IdentityResolver]] — never taken from the caller. Under a
    * primary, `grantorSeal` is required and the grantor key is likewise resolved
    * (primary agreement owner).
    *
    * `knownLanguages` is **mandatory** (non-empty): derive it from
    * [[PackAccess.loadClosed]] via the PackAccess overload.
    */
  def plantGoverned(
      agreement: DomainAgreement,
      identities: IdentityResolver,
      ownerSeal: (String, Vector[Byte]),
      knownLanguages: Set[Digest],
      module: Option[Module] = None,
      grantorSeal: Option[(String, Vector[Byte])] = None,
      verify: (Vector[Byte], Array[Byte], Vector[Byte]) => Boolean = Ed25519.verify,
  ): Either[String, BranchManifest] =
    if knownLanguages.isEmpty then
      Left("domain-agreement: language index required (derive from PackAccess.loadClosed)")
    else
      plantGovernedChecked(
        agreement, identities, ownerSeal, module, grantorSeal, verify, knownLanguages)

  /** Prefer this entrypoint: language digests come from loaded packs. */
  def plantGoverned(
      agreement: DomainAgreement,
      identities: IdentityResolver,
      ownerSeal: (String, Vector[Byte]),
      packs: PackAccess,
  ): Either[String, BranchManifest] =
    val idx = packs.loadClosed().values.map(_.digest).toSet
    if idx.isEmpty then
      Left("domain-agreement: PackAccess.loadClosed returned no languages")
    else
      plantGoverned(agreement, identities, ownerSeal, idx)

  def plantGoverned(
      agreement: DomainAgreement,
      identities: IdentityResolver,
      ownerSeal: (String, Vector[Byte]),
      packs: PackAccess,
      grantorSeal: (String, Vector[Byte]),
  ): Either[String, BranchManifest] =
    val idx = packs.loadClosed().values.map(_.digest).toSet
    if idx.isEmpty then
      Left("domain-agreement: PackAccess.loadClosed returned no languages")
    else
      plantGoverned(
        agreement, identities, ownerSeal, idx,
        grantorSeal = Some(grantorSeal))

  /** Audit the complete sealed agreement + delegation graph for `child`. */
  def auditGoverned(
      child: String,
      identities: IdentityResolver,
      packs: PackAccess,
      verify: (Vector[Byte], Array[Byte], Vector[Byte]) => Boolean = Ed25519.verify,
  ): Either[String, Unit] =
    val idx = packs.loadClosed().values.map(_.digest).toSet
    if idx.isEmpty then Left("domain-audit: PackAccess.loadClosed returned no languages")
    else if !refs.list().contains(child) then Left(s"domain-audit: unknown branch '$child'")
    else
      def walk(name: String, seen: Set[String]): Either[String, Unit] =
        if seen.contains(name) then Left(s"domain-audit: cycle at '$name'")
        else
          val m = refs.load(name)
          m.domainAgreement match
            case None => Left(s"domain-audit: '$name' is not governed")
            case Some(d) =>
              for
                art <- refs.getByDigest(d).left.map(e => s"domain-audit: $e")
                sealedAg <- SealedDomainAgreement.fromCanon(art.body)
                _ <- Either.cond(
                  m.primaryAncestor == sealedAg.agreement.primaryAncestor, (),
                  s"domain-audit: '$name' manifest primary ${m.primaryAncestor} " +
                    s"!= agreement ${sealedAg.agreement.primaryAncestor}")
                _ <- Either.cond(
                  m.references == sealedAg.agreement.references, (),
                  s"domain-audit: '$name' manifest refs ${m.references} " +
                    s"!= agreement ${sealedAg.agreement.references}")
                gName = sealedAg.agreement.primaryAncestor.flatMap { p =>
                  refs.load(p).domainAgreement.flatMap { pd =>
                    refs.getByDigest(pd).toOption
                      .flatMap(a => SealedDomainAgreement.fromCanon(a.body).toOption)
                      .map(_.agreement.owner)
                  }
                }
                _ <- SealedDomainAgreement.verify(sealedAg, identities, verify, gName)
                _ <- verifyLanguageEvidence(sealedAg.agreement, idx)
                _ <- sealedAg.agreement.primaryAncestor match
                  case None => Right(())
                  case Some(p) =>
                    for
                      delDig <- sealedAg.agreement.ancestorDelegation.toRight(
                        s"domain-audit: '$name' missing ancestorDelegation")
                      delArt <- refs.getByDigest(delDig).left.map(e => s"domain-audit: $e")
                      sealedDel <- SealedDomainAncestorDelegation.fromCanon(delArt.body)
                      primaryDig <- refs.load(p).domainAgreement.toRight(
                        s"domain-audit: primary '$p' ungoverned")
                      primaryArt <- refs.getByDigest(primaryDig).left.map(e => s"domain-audit: $e")
                      primarySealed <- SealedDomainAgreement.fromCanon(primaryArt.body)
                      gPk <- identities.require(primarySealed.agreement.owner)
                      _ <- SealedDomainAncestorDelegation.verify(sealedDel, gPk, verify)
                      _ <- DomainAncestorDelegation.matches(
                        sealedDel.delegation, sealedAg.agreement, primarySealed.agreement.owner)
                      _ <- walk(p, seen + name)
                    yield ()
                _ <- sealedAg.agreement.references.foldLeft[Either[String, Unit]](Right(())) {
                  (acc, ref) => acc.flatMap(_ => walk(ref, seen + name))
                }
              yield ()
      walk(child, Set.empty)

  private def plantGovernedChecked(
      agreement: DomainAgreement,
      identities: IdentityResolver,
      ownerSeal: (String, Vector[Byte]),
      module: Option[Module],
      grantorSeal: Option[(String, Vector[Byte])],
      verify: (Vector[Byte], Array[Byte], Vector[Byte]) => Boolean,
      knownLanguages: Set[Digest],
  ): Either[String, BranchManifest] =
    val known = refs.list().toSet
    val (ownerName, ownerSig) = ownerSeal
    val liveE: Either[String, Option[SealedDomainAgreement]] =
      if !known.contains(agreement.child) then Right(None)
      else
        refs.load(agreement.child).domainAgreement match
          case None => Right(None)
          case Some(d) =>
            refs.getByDigest(d) match
              case Left(e) =>
                Left(s"domain-agreement: cannot load live ${d.short}: $e")
              case Right(art) =>
                SealedDomainAgreement.fromCanon(art.body) match
                  case Left(e) =>
                    Left(s"domain-agreement: cannot decode live ${d.short}: $e")
                  case Right(sealedAg) =>
                    val gName = sealedAg.agreement.primaryAncestor.flatMap { p =>
                      refs.load(p).domainAgreement.flatMap { pd =>
                        refs.getByDigest(pd).toOption
                          .flatMap(a => SealedDomainAgreement.fromCanon(a.body).toOption)
                          .map(_.agreement.owner)
                      }
                    }
                    SealedDomainAgreement.verify(sealedAg, identities, verify, gName).map(_ => Some(sealedAg))
    for
      liveSealed <- liveE
      live = liveSealed.map(_.agreement)
      ownerPk <- identities.require(agreement.owner)
      _ <- DomainAgreement.authenticateOwner(agreement, ownerName, ownerPk, ownerSig, verify)
      _ <- DomainAgreement.wellFormed(agreement, known, primaryOf(known))
      _ <- verifyLanguageEvidence(agreement, knownLanguages)
      _ <- verifyAncestorDelegation(agreement, known, identities, grantorSeal, verify)
      _ <- DomainAgreement.allowsTransition(
        agreement, live, liveSealedDigest = liveSealed.map(_.digest))
      sealedAg = agreement.seal(ownerSig, grantorSeal.map(_._2))
      art = sealedAg.artifact
      _ = refs.putArt(art)
      certDigests = art.digest :: agreement.ancestorDelegation.toList
      base <-
        if known.contains(agreement.child) then
          val cur = refs.load(agreement.child)
          val next = cur.copy(
            primaryAncestor = agreement.primaryAncestor,
            references = agreement.references,
            domainAgreement = Some(art.digest),
            certificates = (cur.certificates ++ certDigests).distinct)
          DomainBranch.wellFormed(next, known, primaryOf(known)).map(_ => refs.storeManifest(next))
        else
          forkFrom(
            agreement.child,
            agreement.primaryAncestor,
            module = None,
            agreement.references).map { planted =>
            refs.storeManifest(planted.copy(
              domainAgreement = Some(art.digest),
              certificates = (planted.certificates ++ certDigests).distinct))
          }
    yield module match
      case None    => base
      case Some(m) => importModule(agreement.child, m)

  /** Mint and store a sealed [[DomainAncestorDelegation]] for `claim`.
    *
    * The grantor must be the primary agreement's owner (resolved via
    * [[IdentityResolver]]). Returns the claim with `ancestorDelegation` set to
    * the sealed artifact digest, plus the grantor seal pair for [[plantGoverned]].
    */
  def mintAncestorDelegation(
      claim: DomainAgreement,
      grantorSeal: (String, Vector[Byte]),
      identities: IdentityResolver,
      verify: (Vector[Byte], Array[Byte], Vector[Byte]) => Boolean = Ed25519.verify,
  ): Either[String, (DomainAgreement, (String, Vector[Byte]))] =
    val known = refs.list().toSet
    val (gName, gSig) = grantorSeal
    claim.primaryAncestor match
      case None =>
        Left("domain-delegation: trunk claims do not need ancestorDelegation")
      case Some(p) if !known.contains(p) =>
        Left(s"domain-delegation: unknown primary ancestor '$p'")
      case Some(p) =>
        for
          grantorExpected <- refs.load(p).domainAgreement match
            case None =>
              // Primary not yet governed — seal is still stored; plantGoverned will refuse.
              Right(gName)
            case Some(d) =>
              refs.getByDigest(d).left.map(e =>
                s"domain-delegation: primary agreement unloadable: $e").flatMap { art =>
                SealedDomainAgreement.fromCanon(art.body).map(_.agreement.owner)
              }
          _ <- Either.cond(gName == grantorExpected, (),
            s"domain-delegation: grantor '$gName' is not primary owner '$grantorExpected'")
          gPk <- identities.require(gName)
          del = DomainAncestorDelegation(
            ancestor = p,
            child = claim.child,
            grantor = gName,
            grantee = claim.owner,
            claimDigest = claim.claimDigest)
          _ <- DomainAncestorDelegation.authenticateGrantor(del, gName, gPk, gSig, verify)
          sealedDel = del.seal(gSig)
          _ = refs.putArt(sealedDel.artifact)
        yield (claim.copy(ancestorDelegation = Some(sealedDel.digest)), (gName, gSig))

  private def verifyLanguageEvidence(
      agreement: DomainAgreement,
      knownLanguages: Set[Digest],
  ): Either[String, Unit] =
    // Schema is enforced in DomainAgreement.wellFormed; membership is optional
    // until a pack index is supplied.
    if knownLanguages.isEmpty then Right(())
    else
      for
        deps <- DomainAgreement.parseDependencyEvidence(agreement.dependencyEvidence)
        claimed = agreement.childLanguage.toList ++
          agreement.ancestorLanguages.map(_._2) ++ deps
        missing = claimed.filterNot(knownLanguages.contains).distinct
        _ <- Either.cond(
          missing.isEmpty, (),
          s"domain-agreement: unknown language digest(s): ${missing.map(_.short).mkString(",")}")
      yield ()

  /** Primary consent: governed primary + sealed DomainAncestorDelegation. */
  private def verifyAncestorDelegation(
      agreement: DomainAgreement,
      known: Set[String],
      identities: IdentityResolver,
      grantorSeal: Option[(String, Vector[Byte])],
      verify: (Vector[Byte], Array[Byte], Vector[Byte]) => Boolean,
  ): Either[String, Unit] =
    agreement.primaryAncestor match
      case None =>
        if grantorSeal.isDefined then
          Left("domain-agreement: trunk plant must not supply grantorSeal")
        else Right(())
      case Some(p) if !known.contains(p) =>
        Left(s"domain-agreement: unknown primary ancestor '$p'")
      case Some(p) =>
        for
          delDig <- agreement.ancestorDelegation.toRight(
            "domain-agreement: plant under primary requires ancestorDelegation")
          primarySealed <- refs.load(p).domainAgreement match
            case None =>
              Left(
                s"domain-agreement: primary '$p' must be governed before children " +
                  "can plantGoverned under it")
            case Some(d) =>
              refs.getByDigest(d) match
                case Left(e) =>
                  Left(s"domain-agreement: primary '$p' agreement ${d.short} unloadable: $e")
                case Right(art) =>
                  SealedDomainAgreement.fromCanon(art.body).left.map(e =>
                    s"domain-agreement: primary '$p' agreement decode: $e")
          primaryAg = primarySealed.agreement
          delArt <- refs.getByDigest(delDig).left.map(e =>
            s"domain-delegation: cannot load ${delDig.short}: $e")
          sealedDel <- SealedDomainAncestorDelegation.fromCanon(delArt.body).left.map(e =>
            s"domain-delegation: cannot decode ${delDig.short}: $e")
          del = sealedDel.delegation
          _ <- DomainAncestorDelegation.matches(del, agreement, primaryAg.owner)
          sealPair <- grantorSeal.toRight(
            "domain-agreement: plant under primary requires grantorSeal")
          (gName, gSig) = sealPair
          _ <- Either.cond(gSig == sealedDel.grantorSeal, (),
            "domain-delegation: grantorSeal does not match sealed delegation artifact")
          gPk <- identities.require(primaryAg.owner)
          _ <- SealedDomainAncestorDelegation.verify(sealedDel, gPk, verify)
          _ <- DomainAncestorDelegation.authenticateGrantor(del, gName, gPk, gSig, verify)
        yield ()

  def forkFrom(
      child: String,
      primary: Option[String],
      module: Option[Module] = None,
      references: List[String] = Nil,
  ): Either[String, BranchManifest] =
    val known = refs.list().toSet
    // Reject malformed proposals rather than silently normalizing them — same
    // posture as Canon decode / ΔL apply (repairing would hide caller bugs).
    if references.exists(_ == child) then
      Left(s"domain: '$child' cannot reference itself")
    else if primary.exists(p => references.contains(p)) then
      Left(s"domain: primary ancestor '${primary.get}' must not also appear in references")
    else if references.length != references.distinct.length then
      Left(s"domain: references contain duplicates: ${references.mkString(",")}")
    else
      val draft = BranchManifest(
        child, None, Nil, primaryAncestor = primary, references = references)
      DomainBranch.wellFormed(draft, known, primaryOf(known)).flatMap { _ =>
        if known.contains(child) then
          val cur = refs.load(child)
          if cur.primaryAncestor == primary && cur.references == references then
            module match
              case None    => Right(cur)
              case Some(m) => Right(importModule(child, m))
          else
            Left(
              s"domain: branch '$child' already exists " +
                s"(primary=${cur.primaryAncestor}, refs=${cur.references})")
        else
          refs.storeManifest(draft)
          module match
            case None    => Right(refs.load(child))
            case Some(m) => Right(importModule(child, m))
      }

  /** Add a soft cross-domain reference (not the primary ancestor).
    *
    * Governed branches must amend via [[plantGoverned]] — direct mutation would
    * desync the branch manifest from its sealed [[DomainAgreement]].
    */
  def referTo(branch: String, other: String): Either[String, BranchManifest] =
    val known = refs.list().toSet
    if !known.contains(branch) then Left(s"domain: branch '$branch' does not exist")
    else if !known.contains(other) then Left(s"domain: reference '$other' is not a known branch")
    else
      val cur = refs.load(branch)
      if cur.domainAgreement.isDefined then
        Left(
          s"domain: '$branch' is governed — amend references via plantGoverned " +
            "(referTo would desync the sealed DomainAgreement)")
      else if cur.primaryAncestor.contains(other) then
        Left(s"domain: '$other' is already the primary ancestor of '$branch'")
      else if cur.references.contains(other) then Right(cur)
      else
        val next = cur.copy(references = cur.references :+ other)
        DomainBranch.wellFormed(next, known, primaryOf(known)).map(_ => refs.storeManifest(next))

  /** Load the module at a branch head (heads are [[ArtifactKind.Ir]] modules). */
  def headModule(branch: String): Either[String, Module] =
    refs.load(branch).head.toRight(s"branch '$branch' has no head").flatMap { key =>
      refs.getKey(key).flatMap { a =>
        if a.kind != ArtifactKind.Ir then Left(s"branch '$branch' head is ${a.kind.name}, not a module")
        else Right(Module.fromCanon(a.body))
      }
    }

  /** Bootstrap / import acceptance: plant a module tip **without** a
    * [[Delta.ValidatedChangeSet]]. Ordinary semantic advancement must use
    * [[commitTip]] (ValidatedTip / ΔL only).
    *
    * Only legal on a branch that has never been governed: no head, history,
    * accepted change, conflict, certificate, gate/acceptance evidence, or
    * domain agreement recorded yet. `primaryAncestor`/`references` are
    * exempt — [[forkFrom]] legitimately establishes a domain fork's ancestry
    * before planting its first module, and that ancestry alone is not
    * governance. Re-importing over an already-governed branch would let
    * import silently undo the very acceptance discipline [[commitTip]] /
    * [[merge]] exist to enforce; administrative replacement of a governed
    * branch is a distinct, not-yet-designed operation.
    */
  def importModule(branch: String, module: Module): BranchManifest =
    val cur = refs.load(branch)
    val pristine =
      cur.head.isEmpty && cur.history.isEmpty && cur.acceptedChange.isEmpty &&
      cur.changeHistory.isEmpty && cur.causalHistoryRoot.isEmpty &&
      cur.conflictState.isEmpty && cur.certificates.isEmpty &&
      cur.gateEvidence.isEmpty && cur.acceptanceEvidence.isEmpty &&
      cur.domainAgreement.isEmpty
    if !pristine then
      throw RuntimeException(
        s"importModule: branch '$branch' is already governed (bootstrap/import only applies " +
          "to a branch with no prior head, history, or acceptance evidence)")
    val constitution = AcceptanceConstitution.open()
    val facts = AcceptanceFacts()
    AcceptanceConstitutionEvaluator.check(
      constitution, ModuleGate.passthrough, ChangeModel.default.digest, module, facts)
      .fold(e => throw RuntimeException(e), identity)
    // Bootstrap has no object-language descriptor yet; bind evidence to a
    // stable explicit bootstrap language marker instead of omitting policy.
    val bootstrapLanguage = Digest.of(Canon.CStr("cairn/bootstrap-import"))
    val evidence = AcceptanceEvidence(
      bootstrapLanguage, module.digest, None, module.digest,
      AcceptancePolicy.open.digest, "", ChangeModel.default.digest,
      constitution = Some(constitution.digest))
    refs.putArt(constitution.artifact)
    refs.putArt(evidence.artifact)
    refs.advanceRaw(branch, refs.putArt(module.artifact), acceptanceEvidence = Some(evidence.digest))

  /** @deprecated Use [[importModule]] for bootstrap/import seeds; [[commitTip]] for ΔL. */
  def commitModule(branch: String, module: Module): BranchManifest =
    importModule(branch, module)

  /** Persist an [[AcceptedTip]] via journaled transactional accept (CAS →
    * journal → refs → clear). `AcceptedTip` is only mintable after both ΔL
    * replay and an explicit [[AcceptancePolicy]] succeed — there is no
    * overload of this method that skips either check.
    */
  def commitTip(
      branch: String,
      accepted: AcceptedTip,
      migration: Option[(LangMigration, ComposedLanguage)] = None,
  ): BranchManifest =
    val cur = refs.load(branch)
    val actualFacts = acceptanceFacts(branch, migration, None).fold(e => throw RuntimeException(e), identity)
    if accepted.facts != actualFacts then
      throw RuntimeException(
        "commitTip acceptance facts do not match branch ancestry/certificates/authority/migration/publication state")
    val histRoot = cur.causalHistoryRoot.orElse(Some(accepted.vcs.base))
    val evidence = accepted.evidence
    refs.transactionalAccept(
      branch,
      accepted.module,
      accepted.vcs,
      parents = cur.head.toList.map(_.valueHash),
      causalHistoryRoot = histRoot,
      publish = None,
      provenanceParents = List(accepted.base.digest),
      provenanceTool = "semantic-commit",
      historyAppend = accepted.runtime.isEmpty,
      extraPuts = List(accepted.base.artifact, accepted.constitution.artifact, evidence.artifact) ++ accepted.runtimeArtifacts,
      gateJudgment = if accepted.policy.gate.judgment.isEmpty then None else Some(accepted.policy.gate.judgment),
      acceptanceEvidence = Some(evidence.digest),
      domainRuntime = accepted.runtime,
      contextLocations = accepted.accessTrace.toList.flatMap(_.accesses.map(_.location)).toSet,
    ).fold(e => throw RuntimeException(e), identity)

  /** Load + replay-check the tip ValidatedChangeSet.
    * Prefers [[BranchManifest.acceptedChange]]; `.change` sidecar is a cache.
    */
  def loadChange(
      branch: String, language: ComposedLanguage, model: ChangeModel = ChangeModel.default,
  ): Either[String, Delta.ValidatedChangeSet] =
    val fromManifest = refs.load(branch).acceptedChange
    val fromSidecar =
      val p = refs.changeRefPath(branch)
      if refs.refsExists(p) then Some(Digest(refs.refsRead(p).trim)) else None
    fromManifest.orElse(fromSidecar) match
      case Some(d) => loadVcs(language, d, model)
      case None =>
        Left(s"branch '$branch' has no persisted change (commit via commitTip)")

  /** Full change history (oldest → newest), each entry replay-checked.
    * Prefers [[BranchManifest.changeHistory]]; `.changes` sidecar is a cache.
    */
  def loadChangeHistory(
      branch: String, language: ComposedLanguage, model: ChangeModel = ChangeModel.default,
  ): Either[String, List[Delta.ValidatedChangeSet]] =
    val manifestHist = refs.load(branch).changeHistory
    val digests =
      if manifestHist.nonEmpty then manifestHist
      else
        val hist = refs.changeHistoryPath(branch)
        if refs.refsExists(hist) then
          refs.refsRead(hist).linesIterator.map(_.trim).filter(_.nonEmpty).map(Digest(_)).toList
        else Nil
    if digests.nonEmpty then
      digests.foldLeft[Either[String, List[Delta.ValidatedChangeSet]]](Right(Nil)) { (acc, d) =>
        acc.flatMap(xs => loadVcs(language, d, model).map(xs :+ _))
      }
    else loadChange(branch, language, model).map(List(_))

  /** Runtime-governed history is derived from the causal graph head view.
    * BranchManifest.changeHistory is intentionally ignored. */
  def loadChangeHistory(
      branch: String, runtime: ResolvedDomainRuntime,
  ): Either[String, List[Delta.ValidatedChangeSet]] =
    for
      graph <- refs.nativeRepository
      heads <- graph.heads.get(branch).toRight(s"branch '$branch' has no native repository head")
      changes <- graph.transfer(heads, Set.empty)
      history <- changes.foldLeft[Either[String, List[Delta.ValidatedChangeSet]]](Right(Nil)) { (acc, causal) =>
        for xs <- acc; vcs <- loadVcs(runtime.language, causal.change, runtime.changeModel) yield xs :+ vcs }
    yield history

  /** Reconstruct a [[SemanticRepository.ValidatedTip]] (replay-checked). */
  def loadTip(
      branch: String, language: ComposedLanguage, model: ChangeModel = ChangeModel.default,
  ): Either[String, SemanticRepository.ValidatedTip] =
    for
      vcs <- loadChange(branch, language, model)
      tipMod <- headModule(branch)
      baseArt <- refs.getByDigest(vcs.base)
      base <-
        if baseArt.kind != ArtifactKind.Ir then Left(s"base ${vcs.base.short} is not a module")
        else Right(Module.fromCanon(baseArt.body))
      _ <- Either.cond(tipMod.digest == vcs.result, (),
        s"branch '$branch' head ${tipMod.digest.short} does not match change result ${vcs.result.short}")
      checked <- SemanticRepository.ValidatedTip.check(
        language, SemanticRepository.Tip(base, tipMod, vcs.change), model)
    yield checked

  /** Persist a merge/gate conflict on `into` — conflict artifact stored,
    * `conflictState` recorded on the branch manifest, head left unchanged.
    * Shared by every acceptance path (two-sided merge, fast-forward,
    * one-sided) so a domain-gate rejection is recorded identically no matter
    * which path produced it.
    */
  private def markConflict(into: String, conflict: Merge.Conflict, context: Option[StoredConflict] = None,
      causalBranches: List[String] = Nil): Merge.Conflict =
    val repositoryGraph = refs.recordNativeConflict(into, causalBranches, conflict)
      .fold(e => throw RuntimeException(e), identity)
    val conflictKey = refs.putArt(conflict.artifact)
    val cur = refs.load(into)
    val marked = BranchManifest(
      into, cur.head, cur.history, cur.causalHistoryRoot, cur.parents,
      cur.acceptedChange, conflictState = Some(conflictKey.valueHash),
      changeHistory = cur.changeHistory,
      certificates = cur.certificates,
      gateEvidence = cur.gateEvidence,
      acceptanceEvidence = cur.acceptanceEvidence,
      domainRuntime = cur.domainRuntime,
      repositoryGraph = repositoryGraph.orElse(cur.repositoryGraph),
      primaryAncestor = cur.primaryAncestor,
      references = cur.references,
      domainAgreement = cur.domainAgreement)
    refs.putArt(marked.artifact) // conflictState recorded; head unchanged
    refs.refsMkdirs()
    refs.refsWrite(refs.conflictRefPath(into), conflictKey.valueHash.hex)
    context.foreach { stored =>
      val key = refs.putArt(stored.artifact)
      refs.refsWrite(refs.conflictContextRefPath(into), key.valueHash.hex)
    }
    if context.isEmpty then refs.clearConflictContext(into)
    conflict

  /** Semantic merge into `into`: integrate two change histories relative to
    * `base`, then journaled transactional accept. On conflict, the conflict
    * artifact is stored in CAS and the branch head is left unchanged.
    * Ledger publish is opt-in via `publish`.
    *
    * `policy` has no default — every merge must name an explicit
    * [[AcceptancePolicy]] ([[AcceptancePolicy.open]] for "no domain gate,"
    * a real one otherwise). Domain validation can no longer be skipped by
    * omission.
    *
    * @return `Right(manifest)` on accept, `Left(conflict)` on overlap.
    */
  def merge(
      language: ComposedLanguage,
      into: String,
      base: Module,
      changeOurs: Cst,
      changeTheirs: Cst,
      migration: Option[(LangMigration, ComposedLanguage)] = None,
      publish: Option[Publish] = None,
      parentBranches: List[String] = Nil,
      policy: AcceptancePolicy,
      constitution: Option[AcceptanceConstitution] = None,
      model: ChangeModel = ChangeModel.default,
      targetModel: ChangeModel = ChangeModel.default,
      runtime: Option[ResolvedDomainRuntime] = None,
  ): Either[String, Either[Merge.Conflict, BranchManifest]] =
    val governingModel = if migration.nonEmpty then targetModel.digest else model.digest
    val governing = constitution.getOrElse(AcceptanceConstitution.fromPolicy(policy, governingModel))
    acceptanceFacts(into, migration, publish).flatMap { facts =>
    SemanticRepository.integrate(language, base, changeOurs, changeTheirs, migration, policy.gate, model, targetModel).flatMap {
      case SemanticRepository.Outcome.Conflicted(conflict) =>
        val bound = runtime.fold(conflict)(r => conflict.copy(runtime = Some(r.digest)))
        Right(Left(markConflict(into, bound,
          Some(StoredConflict(language.digest, base, changeOurs, changeTheirs, bound)), parentBranches)))
      case outcome @ SemanticRepository.Outcome.Accepted(module, vcs, _, _) =>
        val checked = runtime match
          case Some(r) => AcceptedTip.checkMerged(r, base, outcome, facts)
          case None    => AcceptedTip.checkMerged(language, base, outcome, policy, governing, facts)
        checked.flatMap { accepted =>
          val parentDigests =
            if parentBranches.nonEmpty then
              parentBranches.flatMap(b => refs.load(b).head.map(_.valueHash))
            else refs.load(into).head.toList.map(_.valueHash)
          val evidence = accepted.evidence
          refs.transactionalAccept(
            into, module, vcs, parentDigests, Some(base.digest), publish,
            provenanceParents = List(base.digest),
            provenanceTool = "semantic-merge",
            historyAppend = runtime.isEmpty,
            extraPuts = List(base.artifact, governing.artifact, evidence.artifact) ++ accepted.runtimeArtifacts,
            gateJudgment = if policy.gate.judgment.isEmpty then None else Some(policy.gate.judgment),
            acceptanceEvidence = Some(evidence.digest),
            domainRuntime = accepted.runtime,
            contextLocations = accepted.accessTrace.toList.flatMap(_.accesses.map(_.location)).toSet,
          ).map(Right(_))
        }
    }
    }

  /** Everyday merge: causal LCA via [[PatchGraph]] when histories form a DAG,
    * falling back to shared module-result digests. Loaded change-sets are
    * replay-checked. Ledger publish remains opt-in via `publish`.
    */
  def mergeBranches(
      runtime: ResolvedDomainRuntime,
      into: String,
      ours: String,
      theirs: String,
  ): Either[String, Either[Merge.Conflict, BranchManifest]] =
    mergeBranches(runtime.language, into, ours, theirs,
      policy = AcceptancePolicy(runtime.moduleGate()),
      constitution = Some(runtime.acceptance), model = runtime.changeModel,
      targetModel = runtime.changeModel, runtime = Some(runtime))

  def mergeBranches(
      language: ComposedLanguage,
      into: String,
      ours: String,
      theirs: String,
      migration: Option[(LangMigration, ComposedLanguage)] = None,
      publish: Option[Publish] = None,
      policy: AcceptancePolicy,
      constitution: Option[AcceptanceConstitution] = None,
      model: ChangeModel = ChangeModel.default,
      targetModel: ChangeModel = ChangeModel.default,
      runtime: Option[ResolvedDomainRuntime] = None,
  ): Either[String, Either[Merge.Conflict, BranchManifest]] =
    for
      histA <- runtime.fold(loadChangeHistory(ours, language, model))(loadChangeHistory(ours, _))
      histB <- runtime.fold(loadChangeHistory(theirs, language, model))(loadChangeHistory(theirs, _))
      (idxA, idxB) = patchAwareLca(histA, histB)
      // Prefer identical linear prefix when it reaches the same LCA tip.
      linear = commonAncestorPrefix(histA, histB)
      (iA, iB) =
        if linear > 0 && linear - 1 == idxA && linear - 1 == idxB then (linear - 1, linear - 1)
        else (idxA, idxB)
      suffixA = if iA < 0 then histA else histA.drop(iA + 1)
      suffixB = if iB < 0 then histB else histB.drop(iB + 1)
      baseDig <-
        if iA >= 0 then Right(histA(iA).result)
        else if histA.nonEmpty && histB.nonEmpty && histA.head.base == histB.head.base then
          Right(histA.head.base)
        else if histA.isEmpty || histB.isEmpty then Left("empty branch history")
        else Left(s"branch histories do not share a causal ancestor: ${histA.head.base.short} vs ${histB.head.base.short}")
      stackedA <-
        if suffixA.isEmpty then Right(None)
        else composeHistory(language, suffixA).map(Some(_))
      stackedB <-
        if suffixB.isEmpty then Right(None)
        else composeHistory(language, suffixB).map(Some(_))
      baseArt <- refs.getByDigest(baseDig)
      base <-
        if baseArt.kind != ArtifactKind.Ir then Left(s"base ${baseDig.short} is not a module")
        else Right(Module.fromCanon(baseArt.body))
      parentDigests = List(ours, theirs).flatMap(b => refs.load(b).head.map(_.valueHash))
      gate = policy.gate
      governingModel = if migration.nonEmpty then targetModel.digest else model.digest
      governing = constitution.getOrElse(AcceptanceConstitution.fromPolicy(policy, governingModel))
      facts <- acceptanceFacts(into, migration, publish)
      gateJudgment = if gate.judgment.isEmpty then None else Some(gate.judgment)
      // `validatedChangeSet` is always the real `vcs.artifact.digest` — the
      // SAME digest `advanceRaw`/`transactionalAccept` record as
      // `BranchManifest.acceptedChange` — never a bespoke raw-change digest,
      // so `AcceptanceEvidence.verify` can replay it. `None` only for a true
      // fast-forward of a branch that itself has no ΔL change (pure import).
      evidenceFor = (vcsDigest: Option[Digest], result: Module, changeModel: Digest) => AcceptanceEvidence(
        language = language.digest, base = baseDig, validatedChangeSet = vcsDigest,
        result = result.digest, policy = policy.digest, judgment = gate.judgment,
        changeModel = changeModel, validationModel = gate.descriptor, providers = gate.providers,
        constitution = Some(governing.digest), domainAgreement = facts.domainAgreement,
        certificates = facts.certificates.map(_.digest), authorities = facts.authorities.toList.sorted,
        migration = facts.migration, publicationRequested = facts.publicationRequested,
        runtime = runtime.map(_.digest))
      out <- (stackedA, stackedB) match
        case (None, None) =>
          headModule(ours).flatMap { m =>
            // Fast-forward: no new ΔL apply happens here (already covered by
            // the replay-side loadChangeHistory check above), but `into` is
            // still accepting a module it has never gate-checked under
            // `into`'s own domain gate — require it, same as every other
            // acceptance path. No apply means no vcs to read a real model
            // digest from, so the caller-supplied `model` IS the identity
            // this transition is recorded under.
            gate(m) match
              case Left(w) =>
                Right(Left(markConflict(into, Merge.Conflict(Set.empty, baseDig, m.digest, Some(w)),
                  causalBranches = List(ours, theirs))))
              case Right(()) =>
                AcceptanceConstitutionEvaluator.check(governing, gate, model.digest, m, facts) match
                  case Left(e) =>
                    val witness = Merge.ConflictWitness.DomainValidationFailed("acceptance-constitution", Canon.CStr(e))
                    Right(Left(markConflict(into, Merge.Conflict(Set.empty, baseDig, m.digest, Some(witness)),
                      causalBranches = List(ours, theirs))))
                  case Right(()) =>
                    val modKey = refs.putArt(m.artifact)
                    val evidence = evidenceFor(refs.load(ours).acceptedChange, m, model.digest)
                    refs.putArt(governing.artifact)
                    refs.putArt(evidence.artifact)
                    runtime.toList.flatMap(_.artifacts).foreach(refs.putArt)
                    val repositoryGraph = refs.copyNativeHeads(into, ours).fold(e => throw RuntimeException(e), identity)
                    Right(Right(refs.advanceRaw(
                      into, modKey,
                      acceptedChange = refs.load(ours).acceptedChange,
                      parents = parentDigests,
                      causalHistoryRoot = Some(baseDig),
                      gateEvidence = gateJudgment.map(g => List(g -> m.digest)).getOrElse(Nil),
                      acceptanceEvidence = Some(evidence.digest),
                      domainRuntime = runtime.map(_.digest),
                      repositoryGraph = repositoryGraph,
                      appendChangeHistory = runtime.isEmpty)))
          }
        case (Some((_, chA)), Some((_, chB))) =>
          merge(language, into, base, chA, chB, migration, publish, List(ours, theirs), policy,
            constitution, model, targetModel, runtime)
        case (Some((_, chA)), None) =>
          SemanticRepository.commit(language, base, chA, model).flatMap { (mod, vcs) =>
            gate(mod) match
              case Left(w) =>
                val chgDig = Artifact(ArtifactKind.ChangeSet, Cst.toCanon(chA)).digest
                Right(Left(markConflict(into, Merge.Conflict(Set.empty, chgDig, baseDig, Some(w)),
                  causalBranches = List(ours, theirs))))
              case Right(()) =>
                AcceptanceConstitutionEvaluator.check(governing, gate, vcs.changeModel, mod, facts) match
                  case Left(e) =>
                    val chgDig = Artifact(ArtifactKind.ChangeSet, Cst.toCanon(chA)).digest
                    val witness = Merge.ConflictWitness.DomainValidationFailed("acceptance-constitution", Canon.CStr(e))
                    Right(Left(markConflict(into, Merge.Conflict(Set.empty, chgDig, baseDig, Some(witness)),
                      causalBranches = List(ours, theirs))))
                  case Right(()) =>
                    val evidence = evidenceFor(Some(vcs.artifact.digest), mod, vcs.changeModel)
                    refs.transactionalAccept(
                      into, mod, vcs, parentDigests, Some(baseDig), publish,
                      provenanceParents = List(base.digest),
                      provenanceTool = "semantic-merge",
                      historyAppend = runtime.isEmpty,
                      extraPuts = List(governing.artifact, evidence.artifact) ++ runtime.toList.flatMap(_.artifacts),
                      gateJudgment = gateJudgment,
                      acceptanceEvidence = Some(evidence.digest),
                      domainRuntime = runtime.map(_.digest),
                      contextLocations = runtime.toList.flatMap(r =>
                        ChangeAlgebra.accessTrace(r.language, base, chA, r.changeModel).toOption.toList
                          .flatMap(_.accesses.map(_.location))).toSet,
                    ).map(Right(_))
          }
        case (None, Some((_, chB))) =>
          SemanticRepository.commit(language, base, chB, model).flatMap { (mod, vcs) =>
            gate(mod) match
              case Left(w) =>
                val chgDig = Artifact(ArtifactKind.ChangeSet, Cst.toCanon(chB)).digest
                Right(Left(markConflict(into, Merge.Conflict(Set.empty, baseDig, chgDig, Some(w)),
                  causalBranches = List(ours, theirs))))
              case Right(()) =>
                AcceptanceConstitutionEvaluator.check(governing, gate, vcs.changeModel, mod, facts) match
                  case Left(e) =>
                    val chgDig = Artifact(ArtifactKind.ChangeSet, Cst.toCanon(chB)).digest
                    val witness = Merge.ConflictWitness.DomainValidationFailed("acceptance-constitution", Canon.CStr(e))
                    Right(Left(markConflict(into, Merge.Conflict(Set.empty, baseDig, chgDig, Some(witness)),
                      causalBranches = List(ours, theirs))))
                  case Right(()) =>
                    val evidence = evidenceFor(Some(vcs.artifact.digest), mod, vcs.changeModel)
                    refs.transactionalAccept(
                      into, mod, vcs, parentDigests, Some(baseDig), publish,
                      provenanceParents = List(base.digest),
                      provenanceTool = "semantic-merge",
                      historyAppend = runtime.isEmpty,
                      extraPuts = List(governing.artifact, evidence.artifact) ++ runtime.toList.flatMap(_.artifacts),
                      gateJudgment = gateJudgment,
                      acceptanceEvidence = Some(evidence.digest),
                      domainRuntime = runtime.map(_.digest),
                      contextLocations = runtime.toList.flatMap(r =>
                        ChangeAlgebra.accessTrace(r.language, base, chB, r.changeModel).toOption.toList
                          .flatMap(_.accesses.map(_.location))).toSet,
                    ).map(Right(_))
          }
    yield out

  /** Interpret a ΔConflict program into an ordinary validated ΔL change.
    * Partial programs persist their explicit deferred/split remainder but do
    * not advance the head. Complete programs pass the same constitution gate
    * as any other accept and cite both conflicting changes in provenance. */
  def resolveConflict(
      runtime: ResolvedDomainRuntime,
      branch: String,
      base: Module,
      left: Cst,
      right: Cst,
      program: Cst,
  ): Either[String, ConflictResolutionOutcome] =
    resolveConflict(runtime.language, branch, base, left, right, program,
      AcceptancePolicy(runtime.moduleGate()), Some(runtime.acceptance), runtime.changeModel, Some(runtime))

  def resolveConflict(
      language: ComposedLanguage,
      branch: String,
      base: Module,
      left: Cst,
      right: Cst,
      program: Cst,
      policy: AcceptancePolicy,
      constitution: Option[AcceptanceConstitution] = None,
      model: ChangeModel = ChangeModel.default,
      runtime: Option[ResolvedDomainRuntime] = None,
  ): Either[String, ConflictResolutionOutcome] =
    for
      conflictDigest <- refs.pendingConflict(branch).toRight(s"branch '$branch' has no unresolved conflict")
      conflictArtifact <- refs.getByDigest(conflictDigest)
      conflict <- Merge.Conflict.fromArtifact(conflictArtifact)
      governing = constitution.getOrElse(AcceptanceConstitution.fromPolicy(policy, model.digest))
      facts <- acceptanceFacts(branch, None, None)
      resolution <- ConflictDelta.resolve(
        language, base, conflict, left, right, program, model, policy.gate, governing, facts,
        conflictDigest = Some(conflictDigest))
      outcome <-
        if resolution.unresolved.nonEmpty then
          val artifact = resolution.artifact
          refs.putArt(artifact)
          Right(ConflictResolutionOutcome.Deferred(artifact, resolution.unresolved))
        else
          val evidence = AcceptanceEvidence(
            language.digest, base.digest, Some(resolution.validatedChange.artifact.digest),
            resolution.result.digest, policy.digest, policy.gate.judgment,
            model.digest, policy.gate.descriptor, policy.gate.providers,
            Some(governing.digest), facts.domainAgreement, facts.certificates.map(_.digest),
            facts.authorities.toList.sorted, facts.migration, facts.publicationRequested,
            runtime.map(_.digest))
          refs.transactionalAccept(
            branch, resolution.result, resolution.validatedChange,
            parents = refs.load(branch).head.toList.map(_.valueHash),
            causalHistoryRoot = Some(base.digest), publish = None,
            provenanceParents = base.digest :: resolution.causalChanges,
            provenanceTool = "conflict-resolution",
            historyAppend = runtime.isEmpty,
            extraPuts = List(base.artifact, governing.artifact, evidence.artifact) ++ runtime.toList.flatMap(_.artifacts),
            gateJudgment = Option.when(policy.gate.judgment.nonEmpty)(policy.gate.judgment),
            acceptanceEvidence = Some(evidence.digest),
            domainRuntime = runtime.map(_.digest),
            contextLocations = runtime.toList.flatMap(r =>
              ChangeAlgebra.accessTrace(r.language, base, resolution.change, r.changeModel).toOption.toList
                .flatMap(_.accesses.map(_.location))).toSet,
            resolves = runtime.map(_ => conflictDigest),
            conflictResolution = Some(resolution.artifact)).map { manifest =>
              ConflictResolutionOutcome.Accepted(manifest, resolution.artifact)
            }
    yield outcome

  /** Load the causal material persisted with a merge conflict so Studio can
    * elaborate semantic buttons into ΔConflict without asking for digests or
    * raw change terms. */
  def studioConflictContext(branch: String): Either[String, StoredConflict] =
    for
      digest <- refs.pendingConflictContext(branch)
        .toRight(s"branch '$branch' has no Studio-resolvable conflict context")
      artifact <- refs.getByDigest(digest)
      stored <- StoredConflict.fromArtifact(artifact)
    yield stored

  /** Put a certificate artifact and append its digest to the branch manifest
    * `certificates` list (linked CAS reference from branch state).
    *
    * Kernel [[CertificateAttach.check]] is mandatory: kind/issuer/tip structure,
    * and tip must match the current branch head when a head exists. Free-form
    * or tip-mismatched certificates cannot become privileged branch state.
    */
  def attachCertificate(branch: String, cert: Artifact): Either[String, (BranchManifest, Digest)] =
    val cur = refs.load(branch)
    val expectedTip = cur.head.map(_.valueHash)
    CertificateAttach.check(cert, expectedTip).flatMap { _ =>
      val dig = refs.putArt(cert).valueHash
      if cur.certificates.contains(dig) then Right((cur, dig))
      else
        val next = cur.copy(certificates = cur.certificates :+ dig)
        val key = refs.putArt(next.artifact)
        refs.refsMkdirs()
        refs.refsWrite(refs.refPath(branch), key.valueHash.hex)
        Right((next, dig))
    }

  /** Read-only Studio status assembled from durable branch/conflict/evidence
    * artifacts. It carries semantic overlap locations, never just names. */
  def studioStatus(branch: String): Either[String, StudioBranchStatus] =
    val manifest = refs.load(branch)
    val conflictDigest = refs.pendingConflict(branch)
    for
      overlap <- conflictDigest.fold[Either[String, Set[SemanticLocation]]](Right(Set.empty)) { digest =>
        refs.getByDigest(digest).flatMap(Merge.Conflict.fromArtifact).map(_.overlap)
      }
      migration <- manifest.acceptanceEvidence.fold[Either[String, Option[Digest]]](Right(None)) { digest =>
        refs.getByDigest(digest).flatMap { artifact =>
          if artifact.kind != ArtifactKind.AcceptanceEvidence then Left("branch acceptance evidence has the wrong kind")
          else AcceptanceEvidence.fromCanon(artifact.body).map(_.migration)
        }
      }
    yield StudioBranchStatus(branch, manifest.changeHistory, conflictDigest, overlap, migration,
      manifest.acceptanceEvidence, manifest.certificates)

  /** Open the ordinary Studio entry point: a capability-selected language,
    * live branch head, governing constitution, and acting authority. */
  def openStudio(
      capabilities: ResolvedLanguageCapabilities,
      branch: String,
      constitution: AcceptanceConstitution,
      authority: String,
      profile: Option[StudioProfile] = None,
      resolveProvider: Digest => Option[ComposedLanguage] = _ => None,
  ): Either[String, StudioSession] =
    for
      _ <- Either.cond(authority.nonEmpty, (), "Studio authority is required")
      _ <- Either.cond(constitution.changeModel == capabilities.changeModel.digest, (),
        "Studio constitution selects another change model")
      selectedProfile <- profile.fold(capabilities.studioProfile)(p => Right(Some(p)))
      _ <- selectedProfile.fold[Either[String, Unit]](Right(()))(_.validate(capabilities.language))
      base <- headModule(branch)
      status <- studioStatus(branch)
      gate = capabilities.moduleGate(resolveProvider)
      workspace = StudioWorkspace(capabilities.language, base, model = capabilities.changeModel, gate = gate)
    yield StudioSession(capabilities, branch, constitution, authority, base, status, workspace,
      selectedProfile, acceptanceGate = gate)

  /** The sole Studio submission path. It rechecks the complete staged change
    * under the live branch facts and constitution, mints AcceptedTip, then uses
    * the existing transactional branch accept. */
  def submitStudio(session: StudioSession): Either[String, BranchManifest] =
    for
      proposal <- session.workspace.proposal.toRight("Studio workspace has no validated proposal")
      live <- headModule(session.branch)
      _ <- Either.cond(live.digest == session.base.digest && proposal.base == live.digest, (),
        "Studio branch head changed; review or rebase the proposal")
      facts <- acceptanceFacts(session.branch,
        session.migration.map(m => m.model -> m.target.language), None)
      _ <- Either.cond(
        session.constitution.authorityRules.required.isEmpty || facts.authorities.contains(session.authority), (),
        s"Studio authority '${session.authority}' is not eligible under the constitution")
      policy = AcceptancePolicy.gated(session.acceptanceGate)
      tip = SemanticRepository.Tip(session.base, proposal.result, proposal.change)
      accepted <- AcceptedTip.checkTip(session.capabilities.language, tip, policy,
        session.constitution, facts, session.capabilities.changeModel)
      manifest <- scala.util.Try(commitTip(session.branch, accepted,
        session.migration.map(m => m.model -> m.target.language))).toEither.left.map(_.getMessage)
    yield manifest
