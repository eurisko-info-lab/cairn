package cairn.tests

import cairn.kernel.*

/** BFT-shaped quorum simulation — research/sim slice, not production. */
class BftQuorumSuite extends munit.FunSuite:
  import BftQuorum.*

  private val ids = List("a", "b", "c", "d").map(ReplicaId(_))
  private val value = Value("branch-head-digest".getBytes.toVector)

  test("maxFaults / quorumSize classic 3f+1"):
    assertEquals(maxFaults(4), 1)
    assertEquals(quorumSize(4), 3)
    assertEquals(maxFaults(7), 2)
    assertEquals(quorumSize(7), 5)

  test("agreement under f < n/3 with one mute Byzantine (partial sync rounds)"):
    val faulty = Set(ReplicaId("d"))
    val decisions = runAgreement(ids, faulty, primary = ReplicaId("a"), view = 0, seq = 1, value)
      .fold(e => fail(e), identity)
    val honestDecided = decisions.collect {
      case (id, Some(d)) if !faulty.contains(id) => id -> d.value.digest
    }
    assertEquals(honestDecided.size, 3, clues(decisions))
    assert(honestAgree(decisions, faulty))
    assert(honestDecided.values.toSet == Set(value.digest))

  test("all-honest 4-replica agreement"):
    val decisions = runAgreement(ids, Set.empty, primary = ReplicaId("b"), view = 1, seq = 0, value)
      .fold(e => fail(e), identity)
    assert(decisions.values.forall(_.isDefined), clues(decisions))
    assert(honestAgree(decisions, Set.empty))

  test("rejects faulty set above f"):
    val err = runAgreement(ids, ids.toSet, primary = ReplicaId("a"), view = 0, seq = 1, value)
    assert(err.isLeft)

  test("PoA quorum size is majority; BFT quorum is 2f+1 (documented divergence)"):
    // M36 PoA: n/2+1. BFT slice: 2f+1 with f=(n-1)/3.
    val n = 4
    val poaMajority = n / 2 + 1
    assertEquals(poaMajority, 3)
    assertEquals(quorumSize(n), 3) // coincides at n=4; diverge at n=7
    assertEquals(7 / 2 + 1, 4)
    assertEquals(quorumSize(7), 5)

  test("quorum intersection lemma at n=7 f=2"):
    assert(quorumsIntersect(7))
    assertEquals(maxFaults(7), 2)
    assertEquals(quorumSize(7), 5)
    assertEquals(5 + 5 - 7, 3) // |Q1 ∩ Q2| ≥ f+1

  test("validReplicaCount requires classic 3f+1 (rejects n=5)"):
    assert(validReplicaCount(1))
    assert(validReplicaCount(4))
    assert(validReplicaCount(7))
    assert(validReplicaCount(10))
    assert(!validReplicaCount(5))
    assert(!validReplicaCount(6))
    assert(!validReplicaCount(8))
    assert(!quorumsIntersect(5))
    assert(!quorumsIntersect(8))

  test("view-change: prepared value commits under new primary"):
    import BftQuorum.*
    val decisions = runViewChange(ids, Set.empty, seq = 0, value)
      .fold(e => fail(e), identity)
    assert(honestAgree(decisions, Set.empty), clues(decisions))
    val decided = decisions.collect { case (id, Some(d)) => id -> d.value.digest }
    assert(decided.nonEmpty, clues(decisions))
    assert(decided.values.toSet == Set(value.digest), clues(decided))
    assert(decisions.values.flatten.exists(_.view == 1), clues(decisions))

  test("equivocating primary: honestAgree holds (no split brain)"):
    val valueB = Value("other-digest".getBytes.toVector)
    val decisions = runEquivocation(
      ids, Set.empty, primary = ReplicaId("a"), view = 0, seq = 1, value, valueB)
      .fold(e => fail(e), identity)
    assert(honestAgree(decisions, Set.empty), clues(decisions))
    // First PrePrepare locks; honest may decide valueA or stay undecided — never valueB alone vs valueA
    val decided = decisions.values.flatten.map(_.value.digest).toSet
    assert(!decided.contains(valueB.digest) || decided == Set(value.digest), clues(decided))

  test("preparedLock: unprepared older slot does not mask later prepare (no collectFirst)"):
    val seq = 0
    val x = Value("unprepared-X".getBytes.toVector)
    val aVal = Value("prepared-A".getBytes.toVector)
    var st = ReplicaState(ReplicaId("b"), n = 4, faulty = false, view = 0)
    val (stX, _) = deliver(st, Msg.PrePrepare(0, seq, x, ReplicaId("a")))
    st = stX.copy(view = 1)
    val p1 = designatedPrimary(ids, 1).get
    val (stPp, _) = deliver(st, Msg.PrePrepare(1, seq, aVal, p1))
    st = stPp
    ids.foreach { rid =>
      val (s, _) = deliver(st, Msg.Prepare(1, seq, aVal.digest, rid))
      st = s
    }
    assertEquals(preparedLock(st, seq, beforeView = 2), Some(aVal.digest))
    // Slot-scan path alone (locks cleared) must still pick highest prepared view.
    assertEquals(preparedLock(st.copy(locks = Map.empty), seq, beforeView = 2), Some(aVal.digest))

  test("cross-view: NewView lock rejects conflicting PrePrepare (local prepare path)"):
    val seq = 0
    val x = Value("X".getBytes.toVector)
    val aVal = Value("A".getBytes.toVector)
    val bVal = Value("B".getBytes.toVector)
    var st = ReplicaState(ReplicaId("c"), n = 4, faulty = false, view = 0)
    val (stX, _) = deliver(st, Msg.PrePrepare(0, seq, x, ReplicaId("a")))
    st = stX.copy(view = 1)
    val p1 = designatedPrimary(ids, 1).get
    val (stPp, _) = deliver(st, Msg.PrePrepare(1, seq, aVal, p1))
    st = stPp
    ids.foreach { rid =>
      val (s, _) = deliver(st, Msg.Prepare(1, seq, aVal.digest, rid))
      st = s
    }
    assert(st.locks.get(seq).exists(_.valueDigest == aVal.digest), clues(st.locks))
    // NewView into view 2 selects A; every replica installs the lock.
    val pc = PreparedCert(seq, aVal.digest, preparedView = 1, value = Some(aVal))
    val vcs: List[Msg.ViewChange] = ids.map(rid => Msg.ViewChange(2, List(pc), rid))
    vcs.foreach { vc =>
      val (s, _) = deliverViewChange(st, vc, ids)
      st = s
    }
    val p2 = designatedPrimary(ids, 2).get
    val (stNv, _) = deliverNewView(st, Msg.NewView(2, List(pc), p2), ids)
    st = stNv
    assertEquals(st.view, 2)
    assertEquals(st.locks.get(seq).map(_.valueDigest), Some(aVal.digest))
    // Byzantine primary proposes conflicting B — must not emit Prepare.
    val (stB, out) = deliver(st, Msg.PrePrepare(2, seq, bVal, p2))
    assert(out.isEmpty, clues(out, stB.locks))
    assert(!stB.slots.get((2, seq)).exists(_.prePrepare.isDefined))

  test("cross-view: backup learns lock only via NewView; rejects conflicting B"):
    val seq = 0
    val aVal = Value("A-via-nv".getBytes.toVector)
    val bVal = Value("B-conflict".getBytes.toVector)
    // Backup never prepared A locally — only unprepared X in view 0.
    val x = Value("X-only".getBytes.toVector)
    var st = ReplicaState(ReplicaId("d"), n = 4, faulty = false, view = 0)
    val (stX, _) = deliver(st, Msg.PrePrepare(0, seq, x, ReplicaId("a")))
    st = stX
    assertEquals(preparedLock(st, seq, beforeView = 1), None)
    val pc = PreparedCert(seq, aVal.digest, preparedView = 1, value = Some(aVal))
    val vcs: List[Msg.ViewChange] = ids.map(rid => Msg.ViewChange(2, List(pc), rid))
    vcs.foreach { vc =>
      val (s, _) = deliverViewChange(st, vc, ids)
      st = s
    }
    val p2 = designatedPrimary(ids, 2).get
    val (stNv, _) = deliverNewView(st, Msg.NewView(2, List(pc), p2), ids)
    st = stNv
    assertEquals(st.locks.get(seq).map(_.valueDigest), Some(aVal.digest))
    val (stB, out) = deliver(st, Msg.PrePrepare(2, seq, bVal, p2))
    assert(out.isEmpty, clues(out))
    assertEquals(preparedLock(stB, seq, beforeView = 2), Some(aVal.digest))
