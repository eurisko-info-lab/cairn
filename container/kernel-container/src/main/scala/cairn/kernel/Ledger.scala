package cairn.kernel

/** L0 ledger state transition (S35–S38) — PURE, no I/O (§3). `Tx`/`SignedTx`/
  * `Block` (the wire types this operates on) live in `container/ledger-types`
  * so `system-interface` can depend on them without pulling in this
  * Merkle-backed state machine.
  *
  * The ledger is the publication spine, not a runtime DB (§4.9): it records
  * artifact digests, branch heads, identities and policy certificates — never
  * artifact bodies (those live in CAS).
  */

/** Ledger state: identities, published digests, branch heads, certificates
  * (with methods), on-chain authorities, and branch policies. The state ROOT
  * (M35) is a Merkle commitment: one subtree per component, so light clients
  * can verify membership with inclusion proofs.
  */
final case class LedgerState(
    identities: Map[String, Vector[Byte]],
    published: Set[String],            // typed-key renders
    heads: Map[String, TypedKey],
    certificates: Map[String, String], // digest hex -> method
    authorities: Map[String, Vector[Byte]] = Map.empty,
    policies: Map[String, Cst] = Map.empty,
):
  def identityEntries: List[(String, Canon)] = identities.toList.sortBy(_._1).map((n, pk) => n -> Canon.CBytes(pk))
  def publishedEntries: List[(String, Canon)] = published.toList.sorted.map(k => k -> Canon.CInt(1))
  def headEntries: List[(String, Canon)] = heads.toList.sortBy(_._1).map((b, k) => b -> Canon.CStr(k.render))
  def certificateEntries: List[(String, Canon)] = certificates.toList.sortBy(_._1).map((h, m) => h -> Canon.CStr(m))
  def authorityEntries: List[(String, Canon)] = authorities.toList.sortBy(_._1).map((n, pk) => n -> Canon.CBytes(pk))
  def policyEntries: List[(String, Canon)] = policies.toList.sortBy(_._1).map((b, p) => b -> Cst.toCanon(p))

  /** Merkle root over the six component subtree roots (M35). */
  def root: Digest = Digest.of(Canon.cmap(
    "identities" -> Canon.CStr(Merkle.root(identityEntries).hex),
    "published" -> Canon.CStr(Merkle.root(publishedEntries).hex),
    "heads" -> Canon.CStr(Merkle.root(headEntries).hex),
    "certificates" -> Canon.CStr(Merkle.root(certificateEntries).hex),
    "authorities" -> Canon.CStr(Merkle.root(authorityEntries).hex),
    "policies" -> Canon.CStr(Merkle.root(policyEntries).hex)))

  /** Inclusion proof that `render` is published (M35): the published-subtree
    * proof plus the component roots to rebuild the top commitment.
    */
  def provePublished(render: String): Either[String, (Merkle.Proof, Map[String, Digest])] =
    Merkle.prove(publishedEntries, render).map(p => (p, componentRoots))
  def proveHead(branch: String): Either[String, (Merkle.Proof, Map[String, Digest])] =
    Merkle.prove(headEntries, branch).map(p => (p, componentRoots))
  def componentRoots: Map[String, Digest] = Map(
    "identities" -> Merkle.root(identityEntries),
    "published" -> Merkle.root(publishedEntries),
    "heads" -> Merkle.root(headEntries),
    "certificates" -> Merkle.root(certificateEntries),
    "authorities" -> Merkle.root(authorityEntries),
    "policies" -> Merkle.root(policyEntries))

object LedgerState:
  val genesis: LedgerState = LedgerState(Map.empty, Set.empty, Map.empty, Map.empty)

  /** Light-client verification (M35): membership under a state root using
    * only component roots + one subtree proof.
    */
  def verifyInclusion(stateRoot: Digest, component: String,
                      componentRoots: Map[String, Digest], proof: Merkle.Proof): Boolean =
    val topOk = Digest.of(Canon.cmap(
      "identities" -> Canon.CStr(componentRoots("identities").hex),
      "published" -> Canon.CStr(componentRoots("published").hex),
      "heads" -> Canon.CStr(componentRoots("heads").hex),
      "certificates" -> Canon.CStr(componentRoots("certificates").hex),
      "authorities" -> Canon.CStr(componentRoots("authorities").hex),
      "policies" -> Canon.CStr(componentRoots("policies").hex))) == stateRoot
    topOk && componentRoots.contains(component) && Merkle.verify(componentRoots(component), proof)

/** Pure kernel transition (S37): signature verification is injected so the
  * kernel stays free of crypto-provider I/O concerns while remaining the sole
  * certifier (§4.6).
  */
object LedgerKernel:
  val genesisParent: Digest = Digest.of(Canon.CStr("cairn-genesis"))

  type Verify = (publicKey: Vector[Byte], message: Array[Byte], signature: Vector[Byte]) => Boolean

  /** Majority quorum over the CURRENT authority set (M36). */
  private def quorumOk(state: LedgerState, tx: Tx, approvals: List[(String, Vector[Byte])], verify: Verify): Either[String, Unit] =
    if state.authorities.isEmpty then Right(()) // bootstrap: first authority self-establishes
    else
      val payload = Tx.approvalPayload(tx)
      val valid = approvals.distinctBy(_._1).count { (signer, sig) =>
        state.authorities.get(signer).exists(pk => verify(pk, payload, sig)) }
      val needed = state.authorities.size / 2 + 1
      if valid >= needed then Right(())
      else Left(s"authority quorum not met: $valid of $needed required approvals")

  /** M37: evaluate the branch policy term against a head-update. */
  private def policyOk(state: LedgerState, branch: String, signer: String, certRef: Option[Digest]): Either[String, Unit] =
    state.policies.get(branch) match
      case None => Right(())
      case Some(Cst.Node("polOpen", _)) => Right(())
      case Some(policy @ Cst.Node("polReq", List(Cst.Leaf(_), Cst.Leaf(method), Cst.Leaf(requiredSigner)))) =>
        for
          _ <- if signer == requiredSigner then Right(())
               else Left(s"policy for '$branch' requires signer '$requiredSigner', tx signed by '$signer' (policy: ${policy.render})")
          cert <- certRef.toRight(s"policy for '$branch' requires a $method certificate reference (policy: ${policy.render})")
          m <- state.certificates.get(cert.hex).toRight(s"certificate ${cert.short} not recorded on ledger")
          _ <- if m == method then Right(())
               else Left(s"policy for '$branch' requires method '$method', certificate ${cert.short} has '$m'")
        yield ()
      case Some(other) => Left(s"unintelligible policy term for '$branch': ${other.render}")

  def applyTx(state: LedgerState, stx: SignedTx, verify: Verify): Either[String, LedgerState] =
    // Identity registration may be self-signed (bootstrap); all else needs a known signer.
    val signerKey: Either[String, Vector[Byte]] = stx.tx match
      case Tx.RegisterIdentity(name, pk) if name == stx.signer => Right(pk)
      case Tx.AddAuthority(name, pk, _) if name == stx.signer && state.authorities.isEmpty => Right(pk)
      case _ => state.identities.get(stx.signer)
        .orElse(state.authorities.get(stx.signer))
        .toRight(s"unknown signer '${stx.signer}'")
    signerKey.flatMap { pk =>
      val msg = Canon.encode(Tx.toCanon(stx.tx))
      if !verify(pk, msg, stx.signature) then Left(s"bad signature on tx ${Tx.digest(stx.tx).short} by '${stx.signer}'")
      else stx.tx match
        case Tx.RegisterIdentity(name, key) =>
          state.identities.get(name) match
            case Some(existing) if existing != key => Left(s"identity '$name' already registered with a different key")
            case _ => Right(state.copy(identities = state.identities + (name -> key)))
        case Tx.PublishArtifact(k) =>
          Right(state.copy(published = state.published + k.render))
        case Tx.SetBranchHead(branch, head, certRef) =>
          if !state.published.contains(head.render) then
            Left(s"branch '$branch' head ${head.valueHash.short} not published")
          else policyOk(state, branch, stx.signer, certRef)
            .map(_ => state.copy(heads = state.heads + (branch -> head)))
        case Tx.RecordCertificate(cert, method) =>
          Right(state.copy(certificates = state.certificates + (cert.hex -> method)))
        case tx @ Tx.AddAuthority(name, key, approvals) =>
          quorumOk(state, tx, approvals, verify).flatMap { _ =>
            state.authorities.get(name) match
              case Some(existing) if existing != key => Left(s"authority '$name' already exists with a different key")
              case _ => Right(state.copy(authorities = state.authorities + (name -> key)))
          }
        case tx @ Tx.RemoveAuthority(name, approvals) =>
          quorumOk(state, tx, approvals, verify).flatMap { _ =>
            if !state.authorities.contains(name) then Left(s"authority '$name' not in the set")
            else if state.authorities.size == 1 then Left("cannot remove the last authority")
            else Right(state.copy(authorities = state.authorities - name))
          }
        case Tx.SetPolicy(branch, policy) =>
          // only an authority may set policy (when a set exists)
          if state.authorities.nonEmpty && !state.authorities.contains(stx.signer) then
            Left(s"policy for '$branch' may only be set by an authority")
          else Right(state.copy(policies = state.policies + (branch -> policy)))
    }

  /** The authority expected to seal height h under round-robin rotation (M36).
    * Only enforced once an ON-CHAIN authority set exists.
    */
  def expectedSealer(state: LedgerState, height: Long): Option[String] =
    if state.authorities.isEmpty then None
    else
      val rotation = state.authorities.keys.toList.sorted
      Some(rotation((height % rotation.size).toInt))

  /** Atomic: a block applies fully or not at all. Verifies parent link,
    * height, authority seal (from on-chain set when present, else the given
    * bootstrap set), round-robin rotation, and the claimed state root.
    */
  def applyBlock(state: LedgerState, expectedParent: Digest, expectedHeight: Long,
                 authorities: Map[String, Vector[Byte]], block: Block, verify: Verify): Either[String, LedgerState] =
    val effectiveAuthorities = if state.authorities.nonEmpty then state.authorities else authorities
    for
      _ <- if block.parent == expectedParent then Right(())
           else Left(s"block ${block.height}: parent ${block.parent.short} != expected ${expectedParent.short}")
      _ <- if block.height == expectedHeight then Right(())
           else Left(s"block height ${block.height} != expected $expectedHeight")
      _ <- expectedSealer(state, expectedHeight) match
        case Some(expected) if expected != block.authority =>
          Left(s"round-robin violation at height $expectedHeight: expected sealer '$expected', got '${block.authority}'")
        case _ => Right(())
      authKey <- effectiveAuthorities.get(block.authority).toRight(s"unknown authority '${block.authority}'")
      _ <- if verify(authKey, Canon.encode(block.unsealedCanon), block.seal) then Right(())
           else Left(s"invalid authority seal on block ${block.height}")
      after <- block.txs.foldLeft[Either[String, LedgerState]](Right(state)) { (acc, stx) =>
        acc.flatMap(applyTx(_, stx, verify)) }
      _ <- if after.root == block.stateRoot then Right(())
           else Left(s"state root mismatch: computed ${after.root.short}, block claims ${block.stateRoot.short}")
    yield after

  /** Validate a whole chain from genesis. */
  def replay(authorities: Map[String, Vector[Byte]], blocks: List[Block], verify: Verify): Either[String, LedgerState] =
    blocks.zipWithIndex.foldLeft[Either[String, (LedgerState, Digest)]](
      Right((LedgerState.genesis, genesisParent))) {
      case (acc, (block, i)) =>
        acc.flatMap { (st, parent) =>
          applyBlock(st, parent, i.toLong, authorities, block, verify).map(st2 => (st2, block.digest)) }
    }.map(_._1)
