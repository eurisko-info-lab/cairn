package cairn.core

import cairn.kernel.*

/** Free change language over a conflict. Its object terms are resolution
  * programs; interpreting one always yields an ordinary ΔL change. */
object ConflictDelta:
  private def tag(language: ComposedLanguage, op: String): String = s"conflict-$op:${language.name}"

  def deltaOf(language: ComposedLanguage): Either[List[ComposeError], ComposedLanguage] =
    val item = s"ΔConflict.${language.name}.item"
    val program = s"ΔConflict.${language.name}.program"
    val fragment = Fragment(
      name = s"ΔConflict:${language.name}", provides = List(s"ΔConflict:${language.name}"), requires = Nil,
      sorts = List(SortDef(program, SortMode.Tree), SortDef(item, SortMode.Tree)),
      constructors = List(
        CtorDef(tag(language, "program"), program, List(item)),
        CtorDef(tag(language, "accept-left"), item, Nil),
        CtorDef(tag(language, "accept-right"), item, Nil),
        CtorDef(tag(language, "replace-at"), item, List("String", "String")),
        CtorDef(tag(language, "compose-resolution"), item, List("String")),
        CtorDef(tag(language, "defer"), item, List("String")),
        CtorDef(tag(language, "split"), item, List("String"))),
      grammar = GrammarPart(
        keywords = List("accept", "left", "right", "replace", "at", "with", "compose", "resolution", "defer", "split"),
        puncts = List("{", "}", ";", "-"),
        categories = List(
          CategorySpec(program, List(ConstructorSpec(tag(language, "program"), List(
            Elem.Tok("{"), Elem.Star(Elem.Cat(item)), Elem.Tok("}"))))),
          CategorySpec(item, List(
            ConstructorSpec(tag(language, "accept-left"), List(Elem.Tok("accept"), Elem.Tok("-"), Elem.Tok("left"), Elem.Tok(";"))),
            ConstructorSpec(tag(language, "accept-right"), List(Elem.Tok("accept"), Elem.Tok("-"), Elem.Tok("right"), Elem.Tok(";"))),
            ConstructorSpec(tag(language, "replace-at"), List(Elem.Tok("replace"), Elem.Tok("-"), Elem.Tok("at"), Elem.StrLeaf, Elem.Tok("with"), Elem.StrLeaf, Elem.Tok(";"))),
            ConstructorSpec(tag(language, "compose-resolution"), List(Elem.Tok("compose"), Elem.Tok("-"), Elem.Tok("resolution"), Elem.StrLeaf, Elem.Tok(";"))),
            ConstructorSpec(tag(language, "defer"), List(Elem.Tok("defer"), Elem.StrLeaf, Elem.Tok(";"))),
            ConstructorSpec(tag(language, "split"), List(Elem.Tok("split"), Elem.StrLeaf, Elem.Tok(";")))))),
        printRules = List(
          PrintRule(tag(language, "program"), List(PrintSeg.Lit("{"), PrintSeg.Newline, PrintSeg.IndentIn,
            PrintSeg.SepFields(0, "\n"), PrintSeg.Newline, PrintSeg.IndentOut, PrintSeg.Lit("}"))),
          PrintRule(tag(language, "accept-left"), List(PrintSeg.Lit("accept-left;"))),
          PrintRule(tag(language, "accept-right"), List(PrintSeg.Lit("accept-right;"))),
          PrintRule(tag(language, "replace-at"), List(PrintSeg.Lit("replace-at"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Space, PrintSeg.Lit("with"), PrintSeg.Space, PrintSeg.StrField(1), PrintSeg.Lit(";"))),
          PrintRule(tag(language, "compose-resolution"), List(PrintSeg.Lit("compose-resolution"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Lit(";"))),
          PrintRule(tag(language, "defer"), List(PrintSeg.Lit("defer"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Lit(";"))),
          PrintRule(tag(language, "split"), List(PrintSeg.Lit("split"), PrintSeg.Space, PrintSeg.StrField(0), PrintSeg.Lit(";")))),
        top = Some(program)))
    Compose.compose(s"ΔConflict(${language.name})", List(fragment))

  enum Disposition:
    case Pending, Deferred, Split
    def canon: Canon = Canon.CStr(toString.toLowerCase)

  final case class Unresolved(location: SemanticLocation, disposition: Disposition):
    def canon: Canon = Canon.cmap("location" -> location.canon, "disposition" -> disposition.canon)

  final case class ValidatedResolution(
      language: Digest,
      conflict: Digest,
      causalChanges: List[Digest],
      change: Cst,
      validatedChange: Delta.ValidatedChangeSet,
      result: Module,
      unresolved: List[Unresolved],
      constitution: Digest,
  ):
    def canon: Canon = Canon.cmap(
      "language" -> Canon.CStr(language.hex), "conflict" -> Canon.CStr(conflict.hex),
      "causalChanges" -> Canon.cstrs(causalChanges.map(_.hex)),
      "change" -> Cst.toCanon(change), "validatedChange" -> Canon.CStr(validatedChange.artifact.digest.hex),
      "result" -> Canon.CStr(result.digest.hex),
      "unresolved" -> Canon.CList(unresolved.sortBy(_.location.render).map(_.canon)),
      "constitution" -> Canon.CStr(constitution.hex))
    def artifact: Artifact = Artifact(ArtifactKind.ConflictResolution, canon)

  private def items(language: ComposedLanguage, program: Cst): Either[String, List[Cst]] = program match
    case Cst.Node(t, List(Cst.Node("list", xs))) if t == tag(language, "program") => Right(xs)
    case Cst.Node(t, xs) if t == tag(language, "program")                         => Right(xs)
    case other => Left(s"not a conflict-resolution program: ${other.render}")

  private def decodeHex(hex: String): Either[String, Cst] =
    try Canon.decode(java.util.HexFormat.of().parseHex(hex)).map(Cst.fromCanon)
    catch case e: Exception => Left(s"invalid canonical term hex: ${e.getMessage}")

  private def locationByRef(conflict: Merge.Conflict, ref: String): Either[String, SemanticLocation] =
    conflict.overlap.find(l => Digest.of(l.canon).hex == ref || l.render == ref)
      .toRight(s"resolution location '$ref' is not present in the conflict")

  def resolve(
      language: ComposedLanguage,
      base: Module,
      conflict: Merge.Conflict,
      left: Cst,
      right: Cst,
      program: Cst,
      model: ChangeModel,
      gate: ModuleGate,
      constitution: AcceptanceConstitution,
      facts: AcceptanceFacts = AcceptanceFacts(),
      conflictDigest: Option[Digest] = None,
  ): Either[String, ValidatedResolution] =
    val leftDigest = Artifact(ArtifactKind.ChangeSet, Cst.toCanon(left)).digest
    val rightDigest = Artifact(ArtifactKind.ChangeSet, Cst.toCanon(right)).digest
    if Set(leftDigest, rightDigest) != Set(conflict.changeA, conflict.changeB) then
      Left("resolution changes do not match the conflict's two causal change digests")
    else
      val initial = conflict.overlap.map(_ -> Disposition.Pending).toMap
      type State = (List[Cst], Map[SemanticLocation, Disposition])
      items(language, program).flatMap { operations =>
        operations.foldLeft[Either[String, State]](Right((Nil, initial))) { (acc, operation) =>
          acc.flatMap { (changes, unresolved) => operation match
            case Cst.Node(t, Nil) if t == tag(language, "accept-left") => Right((List(left), Map.empty))
            case Cst.Node(t, Nil) if t == tag(language, "accept-right") => Right((List(right), Map.empty))
            case Cst.Node(t, List(Cst.Leaf(ref), Cst.Leaf(termHex))) if t == tag(language, "replace-at") =>
              for
                location <- locationByRef(conflict, ref)
                replacement <- decodeHex(termHex)
                edit <- location match
                  case SemanticLocation.Subtree(name, path) => Right(Cst.Node(Delta.tag(language, "edit"), List(
                    Cst.Leaf(name), Cst.Node("list", path.indices.map(i => Cst.Leaf(i.toString))), replacement)))
                  case other => Left(s"replace-at requires a subtree semantic path, got ${other.render}")
              yield (changes :+ edit, unresolved - location)
            case Cst.Node(t, List(Cst.Leaf(changeHex))) if t == tag(language, "compose-resolution") =>
              decodeHex(changeHex).map(change => (changes :+ change, unresolved))
            case Cst.Node(t, List(Cst.Leaf(ref))) if t == tag(language, "defer") =>
              locationByRef(conflict, ref).map(location => (changes, unresolved.updated(location, Disposition.Deferred)))
            case Cst.Node(t, List(Cst.Leaf(ref))) if t == tag(language, "split") =>
              locationByRef(conflict, ref).map(location => (changes, unresolved.updated(location, Disposition.Split)))
            case other => Left(s"unknown conflict-resolution operation: ${other.render}")
          }
        }.flatMap { (changes, unresolved) =>
          val ordinary = changes.foldLeft(ChangeAlgebra.changeset(language, Nil))(
            ChangeAlgebra.compose(language, _, _))
          for
            applied <- Delta.apply(language, base, ordinary, model)
            (result, vcs) = applied
            _ <- AcceptanceConstitutionEvaluator.check(constitution, gate, model.digest, result, facts)
          yield ValidatedResolution(
            language.digest, conflictDigest.getOrElse(conflict.artifact.digest), List(conflict.changeA, conflict.changeB),
            ordinary, vcs, result,
            unresolved.toList.map((location, disposition) => Unresolved(location, disposition)),
            constitution.digest)
        }
      }
