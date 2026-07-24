package cairn.user.pki

import cairn.kernel.*
import cairn.systeminterface.PackAccess
import cairn.core.*

/** PKI pack façade — first link in `PKI → Law → SDS`.
  *
  * Object language: [[languages/pki.cairn]]. Crypto (Ed25519 sign/verify)
  * and chain validation live in the composition root (`examples` / effects),
  * not in this pack — pure pack load only.
  */
final class Pki(packs: PackAccess):
  lazy val fragments: List[Fragment] = packs.requireOwn("pki")

  lazy val language: ComposedLanguage = packs.requireClosed("pki")

  def revokedNames(m: Module): Set[String] =
    m.defs.collect {
      case (_, Cst.Node("revocation", List(Cst.Leaf(n), _, _))) => n
    }.toSet
