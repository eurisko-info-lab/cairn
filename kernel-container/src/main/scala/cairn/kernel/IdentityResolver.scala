package cairn.kernel

/** Resolve a subject name to its canonical Ed25519 public key.
  *
  * Prefer on-ledger [[LedgerState.identities]] / [[LedgerState.authorities]];
  * fall back to an explicit bootstrap map (PoA seal keys / test fixtures).
  * Callers must **not** supply an unverified public key alongside a name —
  * seals prove possession of *a* key; this resolver binds the name to *the*
  * key.
  */
final case class IdentityResolver(
    bootstrap: Map[String, Vector[Byte]] = Map.empty,
    ledger: Option[LedgerState] = None,
):
  def publicKey(name: String): Either[String, Vector[Byte]] =
    val fromLedger = ledger.flatMap { st =>
      st.identities.get(name).orElse(st.authorities.get(name))
    }
    fromLedger.orElse(bootstrap.get(name))
      .toRight(s"unknown identity '$name' (not on ledger and not in bootstrap)")

  def require(name: String): Either[String, Vector[Byte]] = publicKey(name)

object IdentityResolver:
  def bootstrapOnly(m: Map[String, Vector[Byte]]): IdentityResolver =
    IdentityResolver(bootstrap = m, ledger = None)

  def of(bootstrap: Map[String, Vector[Byte]], state: LedgerState): IdentityResolver =
    IdentityResolver(bootstrap, Some(state))
