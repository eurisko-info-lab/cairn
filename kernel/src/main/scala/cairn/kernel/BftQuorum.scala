package cairn.kernel

/** Research / simulation slice of a BFT-shaped quorum protocol.
  *
  * **Honesty bounds (read before using):**
  * - Kernel protocol object — in-process by default; network deployment lives
  *   in system-handler (`BftFinality`: signed HTTP messages + certificates).
  * - Assumes authenticated channels and a static replica set known a priori.
  * - Safety claimed only under `faulty < n/3` (classic quorum intersection).
  * - Liveness assumes partial synchrony after GST: view-change elects a new
  *   primary after timeout; prepared proofs carry locked values into the new view.
  * - Elevates existing PoA quorum *shape* toward an explicit BFT experiment;
  *   network finality certifies sealed PoA digests — does **not** replace M36
  *   PoA sealing or claim open-membership mainnet readiness.
  * - No cryptocurrency / tokenomics.
  *
  * Protocol (PBFT-lite): primary proposes → replicas prepare → commit when
  * `2f+1` matching prepares → decide when `2f+1` matching commits.
  * On timeout: ViewChange → NewView → new primary re-proposes prepared values.
  */
object BftQuorum:
  final case class ReplicaId(id: String)
  final case class Value(bytes: Vector[Byte]):
    def digest: Digest = Digest.of(Canon.CBytes(bytes))

  /** Proof that a replica prepared `valueDigest` at `seq` in `preparedView`. */
  final case class PreparedCert(seq: Int, valueDigest: Digest, preparedView: Int)

  enum Msg:
    case PrePrepare(view: Int, seq: Int, value: Value, from: ReplicaId)
    case Prepare(view: Int, seq: Int, valueDigest: Digest, from: ReplicaId)
    case Commit(view: Int, seq: Int, valueDigest: Digest, from: ReplicaId)
    case ViewChange(newView: Int, prepared: List[PreparedCert], from: ReplicaId)
    case NewView(newView: Int, prepared: List[PreparedCert], from: ReplicaId)

  final case class Decision(view: Int, seq: Int, value: Value, commits: List[ReplicaId])

  def maxFaults(n: Int): Int =
    if n <= 0 then 0 else (n - 1) / 3

  def quorumSize(n: Int): Int =
    2 * maxFaults(n) + 1

  def validReplicaCount(n: Int): Boolean =
    n >= 1 && n == 3 * maxFaults(n) + 1

  final case class Slot(
      prePrepare: Option[Msg.PrePrepare] = None,
      prepares: Map[ReplicaId, Digest] = Map.empty,
      commits: Map[ReplicaId, Digest] = Map.empty,
      decided: Option[Value] = None)

  final case class ReplicaState(
      id: ReplicaId,
      n: Int,
      faulty: Boolean,
      view: Int = 0,
      slots: Map[(Int, Int), Slot] = Map.empty,
      viewChanges: Map[Int, Map[ReplicaId, Msg.ViewChange]] = Map.empty):
    def f: Int = maxFaults(n)
    def q: Int = quorumSize(n)

  def designatedPrimary(replicaIds: List[ReplicaId], view: Int): Option[ReplicaId] =
    val sorted = replicaIds.sortBy(_.id)
    if sorted.isEmpty then None
    else Some(sorted(Math.floorMod(view, sorted.length)))

  def selectPrepared(vcs: Iterable[Msg.ViewChange]): List[PreparedCert] =
    vcs.toList.flatMap(_.prepared)
      .groupBy(_.seq)
      .values
      .map(_.maxBy(p => (p.preparedView, p.valueDigest.hex)))
      .toList
      .sortBy(_.seq)

  def preparedFromSlots(state: ReplicaState, minSeqExclusive: Int = -1): List[PreparedCert] =
    state.slots.toList.collect {
      case ((v, seq), slot) if seq > minSeqExclusive =>
        slot.prePrepare.filter { pp =>
          val d = pp.value.digest
          slot.prepares.count(_._2 == d) >= state.q
        }.map(pp => PreparedCert(seq, pp.value.digest, v))
    }.flatten
      .groupBy(_.seq)
      .values
      .map(_.maxBy(_.preparedView))
      .toList
      .sortBy(_.seq)

  private def findValue(state: ReplicaState, dig: Digest): Option[Value] =
    state.slots.values.view.flatMap(_.prePrepare).find(_.value.digest == dig).map(_.value)

  /** Deliver one message; ViewChange/NewView require [[deliverViewChange]] /
    * [[deliverNewView]] with the full replica-id list for primary checks.
    */
  def deliver(state: ReplicaState, msg: Msg): (ReplicaState, List[Msg]) =
    if state.faulty then (state, Nil)
    else msg match
      case pp @ Msg.PrePrepare(view, seq, value, _) =>
        if view != state.view then (state, Nil)
        else
          val key = (view, seq)
          val slot = state.slots.getOrElse(key, Slot())
          if slot.prePrepare.isDefined then (state, Nil)
          else
            val updated = slot.copy(prePrepare = Some(pp))
            (state.copy(slots = state.slots + (key -> updated)),
              List(Msg.Prepare(view, seq, value.digest, state.id)))

      case Msg.Prepare(view, seq, d, from) =>
        if view != state.view then (state, Nil)
        else
          val key = (view, seq)
          val slot = state.slots.getOrElse(key, Slot())
          val prepares = slot.prepares + (from -> d)
          val withPrep = slot.copy(prepares = prepares)
          val matching = prepares.count(_._2 == d)
          val follow =
            if slot.commits.contains(state.id) then Nil
            else if matching >= state.q && slot.prePrepare.exists(_.value.digest == d) then
              List(Msg.Commit(view, seq, d, state.id))
            else Nil
          val withCommitLog =
            if follow.nonEmpty then withPrep.copy(commits = withPrep.commits + (state.id -> d))
            else withPrep
          (state.copy(slots = state.slots + (key -> withCommitLog)), follow)

      case Msg.Commit(view, seq, d, from) =>
        if view != state.view then (state, Nil)
        else
          val key = (view, seq)
          val slot = state.slots.getOrElse(key, Slot())
          val commits = slot.commits + (from -> d)
          val matching = commits.count(_._2 == d)
          val decided =
            if slot.decided.isDefined then slot.decided
            else if matching >= state.q then
              slot.prePrepare.filter(_.value.digest == d).map(_.value)
            else slot.decided
          val updated = slot.copy(commits = commits, decided = decided.orElse(slot.decided))
          (state.copy(slots = state.slots + (key -> updated)), Nil)

      case _: Msg.ViewChange | _: Msg.NewView =>
        // Use deliverViewChange / deliverNewView with replica id list.
        (state, Nil)

  def deliverViewChange(
      state: ReplicaState,
      vc: Msg.ViewChange,
      replicaIds: List[ReplicaId],
  ): (ReplicaState, List[Msg]) =
    if state.faulty || vc.newView <= state.view then (state, Nil)
    else
      val forView = state.viewChanges.getOrElse(vc.newView, Map.empty) + (vc.from -> vc)
      val st2 = state.copy(viewChanges = state.viewChanges + (vc.newView -> forView))
      if forView.size >= st2.q && designatedPrimary(replicaIds, vc.newView).contains(st2.id) then
        (st2, List(Msg.NewView(vc.newView, selectPrepared(forView.values), st2.id)))
      else (st2, Nil)

  def deliverNewView(
      state: ReplicaState,
      nv: Msg.NewView,
      replicaIds: List[ReplicaId],
  ): (ReplicaState, List[Msg]) =
    if state.faulty || nv.newView <= state.view then (state, Nil)
    else if !designatedPrimary(replicaIds, nv.newView).contains(nv.from) then (state, Nil)
    else if state.viewChanges.getOrElse(nv.newView, Map.empty).size < state.q then (state, Nil)
    else
      // Advance view; new primary re-broadcasts PrePrepares so normal prepare/commit runs.
      val st2 = state.copy(view = nv.newView)
      val follow =
        if designatedPrimary(replicaIds, nv.newView).contains(state.id) then
          nv.prepared.flatMap { pc =>
            findValue(state, pc.valueDigest).map { v =>
              Msg.PrePrepare(nv.newView, pc.seq, v, state.id)
            }
          }
        else Nil
      (st2, follow)

  def runAgreement(
      replicaIds: List[ReplicaId],
      faultyIds: Set[ReplicaId],
      primary: ReplicaId,
      view: Int,
      seq: Int,
      value: Value,
      maxRounds: Int = 8
  ): Either[String, Map[ReplicaId, Option[Decision]]] =
    val n = replicaIds.length
    if !validReplicaCount(n) then Left(s"bft: n=$n is not a valid 3f+1 size")
    else if !replicaIds.contains(primary) then Left("primary not in replica set")
    else if faultyIds.size > maxFaults(n) then
      Left(s"faulty set size ${faultyIds.size} exceeds f=${maxFaults(n)} for n=$n")
    else
      var states: Map[ReplicaId, ReplicaState] =
        replicaIds.map(id => id -> ReplicaState(id, n, faultyIds.contains(id), view = view)).toMap
      var inbox: List[Msg] = List(Msg.PrePrepare(view, seq, value, primary))
      var round = 0
      while inbox.nonEmpty && round < maxRounds do
        val batch = inbox
        inbox = Nil
        batch.foreach { msg =>
          replicaIds.foreach { rid =>
            val (st2, out) = deliver(states(rid), msg)
            states = states + (rid -> st2)
            inbox = inbox ++ out
          }
        }
        round += 1
      Right(states.map { (id, st) =>
        val slot = st.slots.get((view, seq))
        id -> slot.flatMap(s =>
          s.decided.map(v => Decision(view, seq, v, s.commits.filter(_._2 == v.digest).keys.toList)))
      })

  def honestAgree(decisions: Map[ReplicaId, Option[Decision]], faulty: Set[ReplicaId]): Boolean =
    val honest = decisions.collect {
      case (id, Some(d)) if !faulty.contains(id) => d.value.digest
    }.toSet
    honest.size <= 1

  def quorumsIntersect(n: Int): Boolean =
    if !validReplicaCount(n) then false
    else
      val f = maxFaults(n)
      val q = quorumSize(n)
      q + q - n >= f + 1

  def runEquivocation(
      replicaIds: List[ReplicaId],
      faultyIds: Set[ReplicaId],
      primary: ReplicaId,
      view: Int,
      seq: Int,
      valueA: Value,
      valueB: Value,
      maxRounds: Int = 8
  ): Either[String, Map[ReplicaId, Option[Decision]]] =
    val n = replicaIds.length
    if !replicaIds.contains(primary) then Left("primary not in replica set")
    else if faultyIds.size > maxFaults(n) then
      Left(s"faulty set size ${faultyIds.size} exceeds f=${maxFaults(n)} for n=$n")
    else if valueA.digest == valueB.digest then Left("equivocation requires distinct digests")
    else
      var states: Map[ReplicaId, ReplicaState] =
        replicaIds.map(id => id -> ReplicaState(id, n, faultyIds.contains(id), view = view)).toMap
      var inbox: List[Msg] = List(
        Msg.PrePrepare(view, seq, valueA, primary),
        Msg.PrePrepare(view, seq, valueB, primary))
      var round = 0
      while inbox.nonEmpty && round < maxRounds do
        val batch = inbox
        inbox = Nil
        batch.foreach { msg =>
          replicaIds.foreach { rid =>
            val (st2, out) = deliver(states(rid), msg)
            states = states + (rid -> st2)
            inbox = inbox ++ out
          }
        }
        round += 1
      Right(states.map { (id, st) =>
        val slot = st.slots.get((view, seq))
        id -> slot.flatMap(s =>
          s.decided.map(v => Decision(view, seq, v, s.commits.filter(_._2 == v.digest).keys.toList)))
      })

  /** Primary for view 0 fails after prepares; view-change to 1 re-proposes and commits. */
  def runViewChange(
      replicaIds: List[ReplicaId],
      faultyIds: Set[ReplicaId],
      seq: Int,
      value: Value,
      maxRounds: Int = 32,
  ): Either[String, Map[ReplicaId, Option[Decision]]] =
    val n = replicaIds.length
    if !validReplicaCount(n) then Left(s"bft: n=$n is not a valid 3f+1 size")
    else if faultyIds.size > maxFaults(n) then
      Left(s"faulty set size ${faultyIds.size} exceeds f=${maxFaults(n)} for n=$n")
    else
      val primary0 = designatedPrimary(replicaIds, 0).get
      var states: Map[ReplicaId, ReplicaState] =
        replicaIds.map(id => id -> ReplicaState(id, n, faultyIds.contains(id), view = 0)).toMap
      var inbox: List[Msg] = List(Msg.PrePrepare(0, seq, value, primary0))
      var round = 0
      // Phase 1: exchange prepares only (drop commits) so the value is prepared but not decided.
      while inbox.nonEmpty && round < 6 do
        val batch = inbox
        inbox = Nil
        batch.foreach { msg =>
          replicaIds.foreach { rid =>
            val (st2, out) = deliver(states(rid), msg)
            states = states + (rid -> st2)
            inbox = inbox ++ out.collect { case p: Msg.Prepare => p }
          }
        }
        round += 1
      // Phase 2: view-change.
      inbox = replicaIds.filterNot(faultyIds.contains).map { rid =>
        Msg.ViewChange(1, preparedFromSlots(states(rid)), rid)
      }
      while inbox.nonEmpty && round < maxRounds do
        val batch = inbox
        inbox = Nil
        batch.foreach { msg =>
          replicaIds.foreach { rid =>
            val st = states(rid)
            val (st2, out) = msg match
              case vc: Msg.ViewChange => deliverViewChange(st, vc, replicaIds)
              case nv: Msg.NewView    => deliverNewView(st, nv, replicaIds)
              case other              => deliver(st, other)
            states = states + (rid -> st2)
            inbox = inbox ++ out
          }
        }
        round += 1
      Right(states.map { (id, st) =>
        val slot = st.slots.get((st.view, seq))
          .orElse(st.slots.get((1, seq)))
          .orElse(st.slots.get((0, seq)))
        id -> slot.flatMap(s =>
          s.decided.map(v => Decision(st.view, seq, v, s.commits.filter(_._2 == v.digest).keys.toList)))
      })
