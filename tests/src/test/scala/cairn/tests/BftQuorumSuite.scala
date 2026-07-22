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

  test("equivocating primary: honestAgree holds (no split brain)"):
    val valueB = Value("other-digest".getBytes.toVector)
    val decisions = runEquivocation(
      ids, Set.empty, primary = ReplicaId("a"), view = 0, seq = 1, value, valueB)
      .fold(e => fail(e), identity)
    assert(honestAgree(decisions, Set.empty), clues(decisions))
    // First PrePrepare locks; honest may decide valueA or stay undecided — never valueB alone vs valueA
    val decided = decisions.values.flatten.map(_.value.digest).toSet
    assert(!decided.contains(valueB.digest) || decided == Set(value.digest), clues(decided))
