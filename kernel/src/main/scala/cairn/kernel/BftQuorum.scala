package cairn.kernel

/** Research / simulation slice of a BFT-shaped quorum protocol.
  *
  * **Honesty bounds (read before using):**
  * - Kernel protocol object — in-process by default; network deployment lives
  *   in system-handler (`BftFinality`: signed HTTP messages + certificates).
  * - Assumes authenticated channels and a static replica set known a priori.
  * - Safety claimed only under `faulty < n/3` (classic quorum intersection).
  * - Liveness assumes partial synchrony after GST (not modeled here): the
  *   simulator runs round-based and always delivers messages in the round.
  * - Elevates existing PoA quorum *shape* toward an explicit BFT experiment;
  *   network finality certifies sealed PoA digests — does **not** replace M36
  *   PoA sealing or claim open-membership mainnet readiness.
  * - No cryptocurrency / tokenomics.
  *
  * Protocol (PBFT-lite): primary proposes → replicas prepare → commit when
  * `2f+1` matching prepares → decide when `2f+1` matching commits.
  */
object BftQuorum:
  final case class ReplicaId(id: String)
  final case class Value(bytes: Vector[Byte]):
    def digest: Digest = Digest.of(Canon.CBytes(bytes))

  enum Msg:
    case PrePrepare(view: Int, seq: Int, value: Value, from: ReplicaId)
    case Prepare(view: Int, seq: Int, valueDigest: Digest, from: ReplicaId)
    case Commit(view: Int, seq: Int, valueDigest: Digest, from: ReplicaId)

  final case class Decision(view: Int, seq: Int, value: Value, commits: List[ReplicaId])

  /** Maximum Byzantine faults tolerated for `n` replicas (`floor((n-1)/3)`). */
  def maxFaults(n: Int): Int =
    if n <= 0 then 0 else (n - 1) / 3

  def quorumSize(n: Int): Int =
    2 * maxFaults(n) + 1

  /** Classic PBFT sizes only: `n = 3f+1` (covers n=1 with f=0, and 4,7,10,…).
    * Arbitrary `n ≥ 4` is **not** safe with `q = 2f+1` — e.g. n=5,f=1,q=3
    * allows two quorums to intersect in a single (possibly Byzantine) replica.
    */
  def validReplicaCount(n: Int): Boolean =
    n >= 1 && n == 3 * maxFaults(n) + 1

  /** One replica's local logs for a single (view, seq) slot. */
  final case class Slot(
      prePrepare: Option[Msg.PrePrepare] = None,
      prepares: Map[ReplicaId, Digest] = Map.empty,
      commits: Map[ReplicaId, Digest] = Map.empty,
      decided: Option[Value] = None)

  final case class ReplicaState(
      id: ReplicaId,
      n: Int,
      faulty: Boolean,
      slots: Map[(Int, Int), Slot] = Map.empty):
    def f: Int = maxFaults(n)
    def q: Int = quorumSize(n)

  /** Deliver one message to a replica; returns updated state + follow-on msgs. */
  def deliver(state: ReplicaState, msg: Msg): (ReplicaState, List[Msg]) =
    if state.faulty then (state, Nil) // Byzantine: drop / mute (worst-case silence)
    else msg match
      case pp @ Msg.PrePrepare(view, seq, value, _) =>
        val key = (view, seq)
        val slot = state.slots.getOrElse(key, Slot())
        if slot.prePrepare.isDefined then (state, Nil)
        else
          val updated = slot.copy(prePrepare = Some(pp))
          val prepare = Msg.Prepare(view, seq, value.digest, state.id)
          (state.copy(slots = state.slots + (key -> updated)), List(prepare))

      case Msg.Prepare(view, seq, d, from) =>
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

  /** Round-based in-process run: primary proposes `value` at (view, seq). */
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
        replicaIds.map(id => id -> ReplicaState(id, n, faultyIds.contains(id))).toMap
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

  /** Safety check: all non-faulty decisions agree on the same value digest. */
  def honestAgree(decisions: Map[ReplicaId, Option[Decision]], faulty: Set[ReplicaId]): Boolean =
    val honest = decisions.collect {
      case (id, Some(d)) if !faulty.contains(id) => d.value.digest
    }.toSet
    honest.size <= 1

  /** Quorum intersection: any two quorums of size `2f+1` share **more than f**
    * replicas when `n = 3f+1` (so at least one is honest). Research/sim aid.
    */
  def quorumsIntersect(n: Int): Boolean =
    if !validReplicaCount(n) then false
    else
      val f = maxFaults(n)
      val q = quorumSize(n)
      // |Q1 ∩ Q2| ≥ 2q − n = f+1 > f
      q + q - n >= f + 1

  /** Equivocating primary: two PrePrepares for the same (view,seq) with
    * different values. Honest replicas lock the first; conflicting second is
    * ignored. Returns decisions — [[honestAgree]] must hold (may be undecided).
    */
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
        replicaIds.map(id => id -> ReplicaState(id, n, faultyIds.contains(id))).toMap
      // Deliver A first (locks), then B (ignored by honest locking rule).
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
