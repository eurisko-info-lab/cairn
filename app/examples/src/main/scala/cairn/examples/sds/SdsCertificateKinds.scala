package cairn.examples.sds
import cairn.runtime.EffectContexts

import cairn.kernel.*
import cairn.core.*
import cairn.runtime.PackLoader
import java.nio.file.Path

/** SDS workflow evidence certificate kinds as a Cairn language + checked module.
  *
  * Language: [[languages/sds-certificate.cairn]] (`certificateKindOk`).
  * Instance: [[languages/sds-certificate/workflow-kinds.cairn]] — approval /
  * tip-signature / publication chain.
  *
  * Host [[SdsCertificates]] still mints CAS `certificate` artifacts (Ed25519 /
  * tip digests); kind tags are judgment-checked against disk SoT so evidence
  * is ordinary content-addressed artifacts without Scala-private kind logic.
  */
object SdsCertificateKinds:
  private lazy val packs = PackLoader(EffectContexts.forPackLoader())
  lazy val language: ComposedLanguage = packs.requireClosed("sds-certificate")

  lazy val workflowKindsModule: Module =
    ChemicalSource.loadModule(language, Path.of("content/languages/sds-certificate/workflow-kinds.cairn"))
      .fold(e => throw RuntimeException(e), identity)

  def checkKind(tag: String): Boolean =
    Search.prove(
      CheckerCfg(language.judgments.values.toList),
      Cst.node("certificateKindOk", Cst.Leaf(tag))).isRight

  /** Ordered kind tags from the workflow evidence-chain module. */
  def workflowKindTags(m: Module = workflowKindsModule): Either[String, List[String]] =
    val kindDefs = m.defs.collect {
      case (n, Cst.Node("kind", List(Cst.Leaf(tag)))) => n -> tag
    }.toMap
    m.get("sdsWorkflowEvidence") match
      case Some(Cst.Node("evidenceChain", List(_, Cst.Node("list", refs)))) =>
        val errs = List.newBuilder[String]
        val tags = List.newBuilder[String]
        for r <- refs do r match
          case Cst.Leaf(ref) =>
            kindDefs.get(ref) match
              case None => errs += s"evidence chain references unknown kind '$ref'"
              case Some(tag) =>
                if !checkKind(tag) then errs += s"kind '$tag' fails certificateKindOk"
                tags += tag
          case other => errs += s"bad kind ref ${other.render}"
        val es = errs.result()
        if es.nonEmpty then Left(es.mkString("; "))
        else Right(tags.result())
      case Some(other) => Left(s"sdsWorkflowEvidence is not an evidenceChain: ${other.render}")
      case None => Left("no sdsWorkflowEvidence binding")

  lazy val workflowKinds: List[String] =
    workflowKindTags().fold(e => throw RuntimeException(e), identity)
