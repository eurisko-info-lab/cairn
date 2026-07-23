package cairn.proof

import cairn.kernel.*

/** M23: ∀-quantified claims over generated term distributions, certified by
  * seeded property runs with shrinking. The generator spec is data; a
  * certificate records seed + count so failures reproduce exactly.
  */
final case class GenSpec(name: String, seed: Long, count: Int, maxDepth: Int):
  def canon: Canon = Canon.cmap(
    "name" -> Canon.CStr(name),
    "seed" -> Canon.CInt(seed),
    "count" -> Canon.CInt(count),
    "maxDepth" -> Canon.CInt(maxDepth))
  def artifact: Artifact = Artifact(ArtifactKind.TestSuite, Canon.CTag("gen-spec", canon))

final case class PropertyFailure(original: Cst, shrunk: Cst, reason: String):
  def render: String = s"property failed on ${original.render}; shrunk to ${shrunk.render}: $reason"

object PropertyCert:
  /** Run `property` over `count` generated terms; on failure, shrink by
    * greedily descending into failing subterms accepted by `admissible`.
    */
  def forAll(
      claim: Claim,
      gen: GenSpec,
      generate: (scala.util.Random, Int) => Cst,
      admissible: Cst => Boolean,
      property: Cst => Either[String, Boolean],
  ): Either[PropertyFailure, Certificate] =
    val rnd = new scala.util.Random(gen.seed)
    def holds(t: Cst): Either[String, Boolean] = property(t)
    def shrink(t: Cst, reason: String): (Cst, String) = t match
      case Cst.Node(_, cs) =>
        cs.find(c => admissible(c) && holds(c).fold(_ => false, ok => !ok)) match
          case Some(smaller) =>
            val r2 = holds(smaller).swap.getOrElse(reason)
            shrink(smaller, r2)
          case None => (t, reason)
      case _ => (t, reason)
    var i = 0
    while i < gen.count do
      val t = generate(rnd, gen.maxDepth)
      holds(t) match
        case Right(true)   => ()
        case Right(false)  =>
          val (small, r) = shrink(t, "property returned false")
          return Left(PropertyFailure(t, small, r))
        case Left(err)     =>
          val (small, r) = shrink(t, err)
          return Left(PropertyFailure(t, small, r))
      i += 1
    Right(Certificate(claim.artifact.digest, "property", gen.artifact.digest))
