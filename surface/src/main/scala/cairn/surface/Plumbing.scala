package cairn.surface

import cairn.kernel.*
import cairn.kernel.Authority.*
import cairn.core.{PackCompose, PolicyEval}
import cairn.runtime.PackLoader
import cairn.systemhandler.*
import java.nio.file.Path

/** Plumbing: named wrappers over Kernel / Core / System Handler engines.
  * Porcelain CLI verbs and transcript `porcelain NAME ;` steps call these.
  * See `docs/porcelain.md`.
  */
object Plumbing:

  final case class Env(
      node: Node,
      branches: Branches,
      casRoot: Path,
      authorities: Map[String, Vector[Byte]],
      packLoader: PackLoader,
      replay: ReplayStore,
  )

  def chainStatus(node: Node, authorities: Map[String, Vector[Byte]]): Either[String, String] =
    for
      digs <- Right(node.chainDigests)
      st <- node.state(authorities)
    yield
      val heads =
        if st.heads.isEmpty then "  (no heads)"
        else st.heads.toList.sortBy(_._1).map((b, k) => s"  $b -> ${k.valueHash.short}").mkString("\n")
      s"""chain status:
         |  height: ${digs.length}
         |  tip: ${digs.lastOption.map(_.short).getOrElse("(empty)")}
         |  identities: ${st.identities.size}
         |  published: ${st.published.size}
         |  heads:
         |$heads""".stripMargin

  def chainCompare(mine: Node, other: Node): String =
    Sync.compare(mine.chainDigests, other.chainDigests) match
      case Sync.Comparison.Same              => "chain compare: same"
      case Sync.Comparison.Ahead(by)         => s"chain compare: ahead by $by"
      case Sync.Comparison.Behind(by)        => s"chain compare: behind by $by"
      case Sync.Comparison.Diverged(h, a, b) =>
        s"chain compare: diverged at height $h mine=${a.short} other=${b.short}"

  def chainExport(node: Node): String =
    val lines = node.chainDigests.map(_.hex)
    s"chain export: ${lines.length} blocks\n" + lines.mkString("\n")

  def branchList(branches: Branches): String =
    val names = branches.list()
    if names.isEmpty then "(no local branch refs)"
    else
      names.map { n =>
        val m = branches.load(n)
        val prim = m.primaryAncestor.getOrElse("∅")
        val refs = if m.references.isEmpty then "-" else m.references.mkString(",")
        s"$n head=${m.head.map(_.valueHash.short).getOrElse("-")} primary=$prim refs=$refs hist=${m.history.length}"
      }.mkString("\n")

  def domainShow(branches: Branches): String =
    val names = branches.list()
    if names.isEmpty then "(no branches — plant a domain tree first)"
    else
      val known = names.toSet
      val lines = names.map { n =>
        val m = branches.load(n)
        DomainBranch.wellFormed(m, known) match
          case Left(err) => s"$n INVALID $err"
          case Right(_) =>
            val prim = m.primaryAncestor.getOrElse("trunk")
            s"$n primary=$prim refs=[${m.references.mkString(",")}]"
      }
      "domain tree:\n" + lines.mkString("\n")

  def authCheck(subject: String, actionName: String = "put"): String =
    val sub = Subject(subject)
    val putKey = EffectMeta.cas.actionKey(actionName)
    val policies = List(EffectPolicy(
      "porcelain-auth", sub, putKey, Resource("*", "*"), Decision.Allow))
    val req = EffectRequest(sub, putKey, EffectMeta.cas.resource.at("*"))
    PolicyEval.prove(req, policies, nowMillis = 0).flatMap { proof =>
      Authority.VerifiedCapability.fromProof(proof, policies)
    } match
      case Right(_) => s"auth check: ALLOW $subject cas/$actionName"
      case Left(e)  => s"auth check: DENY $subject cas/$actionName ($e)"

  def composeStatus(packLoader: PackLoader, lang: String): Either[String, String] =
    val packs = packLoader.loadRaw()
    PackCompose.close(lang, packs).left.map(_.map(_.render).mkString("; ")).map { closed =>
      val unmet = PackCompose.unmetRequires(lang, packs)
      s"""compose status $lang:
         |  closed: ${closed.digest.short}
         |  fragments: ${closed.fragments.length}
         |  unmetRequires: ${if unmet.isEmpty then "(none)" else unmet.mkString(",")}""".stripMargin
    }

  def catalogExport(packLoader: PackLoader): String =
    val closed = packLoader.loadClosed()
    closed.toList.sortBy(_._1).map { (n, l) =>
      s"$n\t${l.digest.hex}\tfrags=${l.fragments.length}"
    }.mkString("catalog export (TSV):\nname\tdigest\tfragments\n", "\n", "\n")

  def workflowList(packLoader: PackLoader): Either[String, String] =
    packLoader.loadClosed().get("sds-workflow").toRight("sds-workflow pack not loaded").map { l =>
      s"workflow list:\n  sds-workflow ${l.digest.short} frags=${l.fragments.length}"
    }

  def recover(branches: Branches, casRoot: Path): Either[String, String] =
    branches.reclaimOrphanBlobs(casRoot, None).map { report =>
      s"""recover:
         |  pendingAccepts: ${if report.recovered.isEmpty then "(none)" else report.recovered.mkString(",")}
         |  reclaim swept=${report.gc.swept} kept=${report.gc.kept} roots=${report.roots}""".stripMargin
    }

  def replaySnapshot(store: ReplayStore): String =
    val snap = store.snapshot
    s"""replay snapshot:
       |  issuers(nonces)=${snap.nonces.size} tokens=${snap.flatNonces.size}
       |  issuers(requestIds)=${snap.requestIds.size} tokens=${snap.flatRequestIds.size}""".stripMargin

  def txState(node: Node, authorities: Map[String, Vector[Byte]]): Either[String, String] =
    node.state(authorities).map { st =>
      s"""tx state:
         |  root: ${st.root.short}
         |  identities: ${st.identities.size}
         |  certificates: ${st.certificates.size}
         |  heads: ${st.heads.size}""".stripMargin
    }

  def lightVerify(node: Node, authorities: Map[String, Vector[Byte]]): Either[String, String] =
    node.state(authorities).map { st =>
      val roots = st.componentRoots.toList.sortBy(_._1)
        .map((k, d) => s"  $k ${d.short}").mkString("\n")
      s"light verify: stateRoot=${st.root.short}\n$roots"
    }

  /** Map a deferred Charb theme name onto porcelain/plumbing that exists today. */
  def charbTheme(name: String, env: Env): Either[String, String] =
    name match
      case "authorization" | "strict-assumptions-governance" =>
        Right(authCheck("alice"))
      case "branch-variant-registry" =>
        Right(branchList(env.branches))
      case "catalog-export" | "key-sbom-capability-registry" =>
        Right(catalogExport(env.packLoader))
      case "chain-json-export" | "governance-kernel-introspection" =>
        chainStatus(env.node, env.authorities)
      case "chain-recovery-negative" | "consistency-batch-mixed-failures" =>
        Right(chainCompare(env.node, env.node))
      case "chain-repair" | "recovery-suggestions" | "strict-governance-recovery-negative" =>
        recover(env.branches, env.casRoot)
      case "compose-registry" =>
        composeStatus(env.packLoader, "stlc")
      case "integration-cross-namespace" =>
        Right(domainShow(env.branches))
      case "interop-lightclient-phase4" =>
        lightVerify(env.node, env.authorities)
      case "replay-history-diff" | "replay-history-switch-and-diff" | "replay-root-switching" |
           "audit-registry" | "audit-mismatch" =>
        Right(replaySnapshot(env.replay))
      case "runner-registry" | "runner-selection" | "workflow-registry" | "distributed" =>
        workflowList(env.packLoader)
      case "strict-governance-publish-integration" | "governance-real-features" =>
        for
          st <- chainStatus(env.node, env.authorities)
          a = authCheck("alice")
        yield st + "\n" + a
      case "tx-state-phase2" =>
        txState(env.node, env.authorities)
      case "incentive-positive" =>
        Right(
          "porcelain incentive-positive: revocation plumbing exists; " +
            "token rewards remain §8 out of scope")
      case other =>
        Left(s"porcelain: theme '$other' has no plumbing mapping (still deferred / §8)")
