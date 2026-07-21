package cairn.kernel

/** Phase 7: Kernel-side Meta validation — independent of Core elaboration.
  * Core elaborates/parses Meta; Kernel checks identity and composition
  * well-formedness of the resulting fragments.
  */
object MetaValidate:
  /** Check that a composed language's digest matches an expected bootstrap pin. */
  def checkFixpoint(language: ComposedLanguage, expectedDigest: Digest): Either[String, Unit] =
    if language.digest == expectedDigest then Right(())
    else Left(s"bootstrap fixpoint mismatch: got ${language.digest.short}, expected ${expectedDigest.short}")

  /** Validate a fragment list can compose under `name` (Kernel acceptance gate). */
  def checkComposable(name: String, fragments: List[Fragment]): Either[String, ComposedLanguage] =
    Compose.compose(name, fragments).left.map(_.map(_.render).mkString("; "))

  /** Check fragment names are unique and provides/requires are well-formed strings. */
  def checkFragmentShapes(fragments: List[Fragment]): Either[String, Unit] =
    val names = fragments.map(_.name)
    if names.distinct.length != names.length then
      Left(s"duplicate fragment names: ${names.diff(names.distinct).mkString(",")}")
    else if fragments.exists(f => f.provides.exists(_.isEmpty) || f.requires.exists(_.isEmpty)) then
      Left("empty provide/require interface name")
    else Right(())
