package cairn.tests

import cairn.kernel.*
import cairn.core.*
import cairn.examples.stlc.Stlc
import cairn.runtime.{Branches, DurableWorkspaces, EffectContexts}
import cairn.systemhandler.{DiskCas, Keypair, MemCas}
import java.nio.file.Files

class DurableWorkspaceSuite extends munit.FunSuite:
  private val language = Stlc.language
  private val capabilities = LanguageCapabilities.standard(language)
  private val constitution = AcceptanceConstitution.open(capabilities.changeModel.digest)
  private val base = Module(List("a" -> Stlc.tru, "b" -> Stlc.fls))

  private def fixture() =
    val root = Files.createTempDirectory("cairn-pr20")
    val cas = DiskCas(root.resolve("cas"))
    val branches = Branches(cas, root.resolve("branches"), EffectContexts.forBranches())
    branches.importModule("main", base)
    (root, cas, branches)

  private def staged(branches: Branches, signer: Keypair, name: String = "a", value: Cst = Stlc.fls): StudioSession =
    val opened = branches.openStudio(capabilities, "main", constitution, signer.name)
      .fold(e => fail(e), identity)
    val workspace = opened.workspace.stage(StudioAction.Replace(name, value)).fold(e => fail(e), identity)
    opened.copy(workspace = workspace)

  test("WorkspaceDraft survives restart and has only content-addressed identity"):
    val (root, cas, branches) = fixture()
    val alice = Keypair.dev("alice")
    val first = DurableWorkspaces(cas, root.resolve("workspaces"), branches)
    val saved = first.save("acetone", staged(branches, alice), alice).fold(e => fail(e), identity)
    assertEquals(first.resolve("acetone"), Right(saved.digest))

    val restartedCas = DiskCas(root.resolve("cas"))
    val restartedBranches = Branches(restartedCas, root.resolve("branches"), EffectContexts.forBranches())
    val restarted = DurableWorkspaces(restartedCas, root.resolve("workspaces"), restartedBranches)
    assertEquals(restarted.reopen("acetone"), Right(saved))
    assertEquals(WorkspaceDraft.fromArtifact(restartedCas.getByDigest(saved.digest).toOption.get), Right(saved))

  test("offline revisions retain ancestry without touching the branch"):
    val (root, cas, branches) = fixture()
    val alice = Keypair.dev("alice")
    val store = DurableWorkspaces(cas, root.resolve("workspaces"), branches)
    val one = store.save("draft", staged(branches, alice), alice).toOption.get
    val two = store.save("draft", staged(branches, alice, "b", Stlc.tru), alice).toOption.get
    assertEquals(two.previous, Some(one.digest))
    assertEquals(branches.headModule("main").map(_.digest), Right(base.digest))
    assert(cas.contains(one.digest) && cas.contains(two.digest))

  test("reviews, approvals, and handoffs are signed actor artifacts"):
    val (root, cas, branches) = fixture()
    val alice = Keypair.dev("alice")
    val bob = Keypair.dev("bob")
    val store = DurableWorkspaces(cas, root.resolve("workspaces"), branches)
    val draft = store.save("draft", staged(branches, alice), alice).toOption.get
    val review = store.review(draft.digest, bob, WorkspaceReviewDecision.RecommendApproval, "looks good").toOption.get
    val approval = store.approval(draft.digest, Some(review.artifact.digest), bob).toOption.get
    val handoff = store.handoff(draft.digest, alice, WorkspaceActor(bob.name, bob.publicBytes)).toOption.get
    assert(store.verify(review) && store.verify(approval) && store.verify(handoff))
    assertEquals(WorkspaceReview.fromArtifact(review.artifact), Right(review))
    assertEquals(WorkspaceApproval.fromArtifact(approval.artifact), Right(approval))
    assertEquals(WorkspaceHandoff.fromArtifact(handoff.artifact), Right(handoff))
    assert(!store.verify(handoff.copy(to = WorkspaceActor("mallory", bob.publicBytes))))

  test("rebase replays the pending ΔL over a changed live head"):
    val (root, cas, branches) = fixture()
    val alice = Keypair.dev("alice")
    val bob = Keypair.dev("bob")
    val store = DurableWorkspaces(cas, root.resolve("workspaces"), branches)
    val draft = store.save("draft", staged(branches, alice, "a", Stlc.fls), alice).toOption.get
    branches.submitStudio(staged(branches, bob, "b", Stlc.tru)).fold(e => fail(e), identity)
    val rebased = store.rebase("draft", draft.digest, capabilities, constitution, alice)
      .fold(e => fail(e), identity)
    assertEquals(rebased.previous, Some(draft.digest))
    assertEquals(rebased.base, branches.headModule("main").toOption.get.digest)
    val result = Module.fromCanon(cas.getByDigest(rebased.result).toOption.get.body)
    assertEquals(result.get("a"), Some(Stlc.fls))
    assertEquals(result.get("b"), Some(Stlc.tru))

  test("replication pulls a complete verified workspace graph"):
    val (root, cas, branches) = fixture()
    val alice = Keypair.dev("alice")
    val bob = Keypair.dev("bob")
    val origin = DurableWorkspaces(cas, root.resolve("workspaces"), branches)
    val draft = origin.save("draft", staged(branches, alice), alice).toOption.get
    val review = origin.review(draft.digest, bob, WorkspaceReviewDecision.RecommendApproval, "ship it").toOption.get
    val approval = origin.approval(draft.digest, Some(review.artifact.digest), bob).toOption.get

    val replicaCas = MemCas()
    val replicaBranches = Branches(replicaCas, root.resolve("replica-branches"), EffectContexts.forBranches())
    val replica = DurableWorkspaces(replicaCas, root.resolve("replica-workspaces"), replicaBranches)
    val copied = replica.replicateFrom(cas, approval.artifact.digest).fold(e => fail(e), identity)
    assert(copied.contains(approval.artifact.digest) && copied.contains(draft.digest))
    assert(replicaCas.contains(review.artifact.digest))
    assertEquals(replica.load(draft.digest), Right(draft))
