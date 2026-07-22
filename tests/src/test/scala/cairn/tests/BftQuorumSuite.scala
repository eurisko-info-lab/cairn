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
