package cairn.systemhandler

/** Read-only view of revoked capability ids. Injected into
  * [[EffectContext]] / [[AuthorityGate.checkCapability]] so revocation is
  * mandatory on the live authorize path — not only ReplayReplication tests/UI.
  */
trait RevocationView:
  def isRevoked(capabilityId: String): Boolean

object RevocationView:
  val empty: RevocationView = new RevocationView:
    def isRevoked(capabilityId: String): Boolean = false

  def of(log: RevocationLog): RevocationView = new RevocationView:
    def isRevoked(capabilityId: String): Boolean = log.isRevoked(capabilityId)

  def ofIds(ids: Iterable[String]): RevocationView =
    val set = ids.toSet
    new RevocationView:
      def isRevoked(capabilityId: String): Boolean = set.contains(capabilityId)
