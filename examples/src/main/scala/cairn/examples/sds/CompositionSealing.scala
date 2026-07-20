package cairn.examples.sds

import cairn.kernel.*
import cairn.workbench.*
import cairn.ledger.Encryption
import java.security.{PrivateKey, PublicKey}

/** Cryptographic confidentiality for SDS composition — on par with GRANITE
  * `composition/CompositionSealing`. Confidential ingredients' true name +
  * exact percentage are sealed to recipient X25519 keys (typically from a
  * PKI encryption certificate); a coarse range band stays in the clear.
  */
object CompositionSealing:
  final case class SealedIngredient(rangeBand: (Double, Double), envelope: Encryption.SealedEnvelope)
  final case class RecoveredIngredient(name: String, exactPercent: Double)

  enum DisclosureEntry:
    case Open(name: String, exactPercent: Double)
    case Sealed(ing: SealedIngredient)

  final case class SealedComposition(entries: List[DisclosureEntry])

  /** GHS-style concentration band for public disclosure. */
  def bandFor(pct: Double): (Double, Double) =
    if pct < 1 then (0.0, 1.0)
    else if pct < 5 then (1.0, 5.0)
    else if pct < 10 then (5.0, 10.0)
    else if pct < 25 then (10.0, 25.0)
    else if pct < 50 then (25.0, 50.0)
    else if pct < 75 then (50.0, 75.0)
    else (75.0, 100.0)

  /** Seal confidential components; non-confidential stay in the clear. */
  def seal(
      m: Module,
      mixtureName: String,
      confidential: Set[String],
      recipients: List[(String, PublicKey)]
  ): Either[String, SealedComposition] =
    m.get(mixtureName) match
      case Some(Cst.Node("mixture", List(Cst.Node("list", comps)))) =>
        comps.foldLeft[Either[String, List[DisclosureEntry]]](Right(Nil)) { (acc, c) =>
          acc.flatMap { es =>
            c match
              case Cst.Node("component", List(Cst.Leaf(ref), Cst.Leaf(pctStr))) =>
                val pct = pctStr.toDouble
                val label = m.get(ref) match
                  case Some(Cst.Node("substance", List(_, Cst.Leaf(name)))) => name
                  case _ => ref
                if !confidential.contains(ref) then Right(es :+ DisclosureEntry.Open(label, pct))
                else
                  val payload = s"$label|$pct".getBytes("UTF-8")
                  val envelope = Encryption.seal(payload, recipients)
                  Right(es :+ DisclosureEntry.Sealed(SealedIngredient(bandFor(pct), envelope)))
              case other => Left(s"bad component: ${other.render}")
          }
        }.map(SealedComposition(_))
      case Some(other) => Left(s"'$mixtureName' is not a mixture: ${other.render}")
      case None        => Left(s"no mixture '$mixtureName'")

  def unseal(
      sealedIngredient: SealedIngredient,
      recipientKeyId: String,
      recipientPrivateKey: PrivateKey
  ): Option[RecoveredIngredient] =
    Encryption.open(sealedIngredient.envelope, recipientKeyId, recipientPrivateKey).flatMap { bytes =>
      new String(bytes, "UTF-8").split('|') match
        case Array(name, pctStr) => pctStr.toDoubleOption.map(RecoveredIngredient(name, _))
        case _                   => None
    }
