package cairn.examples.sds

import cairn.runtime.Branches
import cairn.kernel.*
import cairn.core.Module
import cairn.examples.pki.DemoPki
import cairn.examples.law.Law

/** Ledger domain trunk for the SDS exemplar DAG.
  *
  * Mirrors pack composition `PKI → Law → SDS` as branch ancestry, plus a soft
  * `CHEMISTRY` reference (chemical instances live beside Law, not under it):
  *
  * {{{
  *   trunk ── PKI
  *         └── CHEMISTRY
  *   PKI ── LAW
  *   LAW ── SDS  (references CHEMISTRY)
  *   SDS ── sds-* work branches (author / industrial / approved / …)
  * }}}
  */
object SdsDomainTree:
  final case class Planted(
      pki: BranchManifest,
      law: BranchManifest,
      chemistry: BranchManifest,
      sds: BranchManifest,
  ):
    def ok: Boolean =
      pki.primaryAncestor.isEmpty && pki.references.isEmpty &&
        law.primaryAncestor.contains("PKI") && law.references.isEmpty &&
        chemistry.primaryAncestor.isEmpty && chemistry.references.isEmpty &&
        sds.primaryAncestor.contains("LAW") && sds.references == List("CHEMISTRY")

  /** Seed modules for the four domain hubs (tips are import/bootstrap, not ΔL). */
  def pkiSeed: Module =
    Module(List("registry" -> DemoPki.hierarchy().registry)).sorted

  def lawSeed: Module = Law.modelAct

  def chemistrySeed: Module = Chemicals.Acetone.thinModule

  def sdsSeed: Module = SdsTutorial.acetoneBase

  /** Plant PKI / LAW / CHEMISTRY / SDS. Idempotent when ancestry already matches. */
  def plant(branches: Branches): Either[String, Planted] =
    for
      pki <- branches.forkFrom("PKI", primary = None, module = Some(pkiSeed))
      law <- branches.forkFrom("LAW", primary = Some("PKI"), module = Some(lawSeed))
      chem <- branches.forkFrom("CHEMISTRY", primary = None, module = Some(chemistrySeed))
      sds <- branches.forkFrom(
        "SDS", primary = Some("LAW"), module = Some(sdsSeed), references = List("CHEMISTRY"))
    yield Planted(pki, law, chem, sds)

  /** Pull a local work branch under the SDS domain hub. */
  def underSds(
      branches: Branches,
      child: String,
      module: Option[Module] = None,
  ): Either[String, BranchManifest] =
    branches.forkFrom(child, primary = Some("SDS"), module = module)

  /** Fail unless the planted hubs still show the expected ancestry. */
  def requirePlanted(branches: Branches): Either[String, Planted] =
    def load(name: String): Either[String, BranchManifest] =
      if !branches.list().contains(name) then Left(s"domain: missing hub branch '$name'")
      else Right(branches.load(name))
    for
      pki <- load("PKI")
      law <- load("LAW")
      chem <- load("CHEMISTRY")
      sds <- load("SDS")
      planted = Planted(pki, law, chem, sds)
      _ <- Either.cond(planted.ok, (),
        s"domain: unexpected ancestry " +
          s"PKI=${pki.primaryAncestor}/${pki.references} " +
          s"LAW=${law.primaryAncestor}/${law.references} " +
          s"CHEMISTRY=${chem.primaryAncestor}/${chem.references} " +
          s"SDS=${sds.primaryAncestor}/${sds.references}")
    yield planted
