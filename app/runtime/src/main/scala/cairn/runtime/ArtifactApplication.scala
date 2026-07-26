package cairn.runtime

import cairn.kernel.*
import cairn.core.*
import cairn.systeminterface.Cas

final case class ResolvedApplication(
    root: Digest,
    manifest: ApplicationManifest,
    languages: Map[String, ResolvedLanguageCapabilities],
    entries: Map[String, Artifact],
    installed: Set[Digest],
)

/** Installs and resolves an application from one digest. The resolver is the
  * composition root; it accepts no host language/entry maps. */
final class ArtifactApplicationResolver(local: Cas):
  def install(root: Digest, source: Cas): Either[String, Set[Digest]] =
    def pull(todo: List[Digest], seen: Set[Digest]): Either[String, Set[Digest]] = todo match
      case Nil => Right(seen)
      case digest :: rest if seen.contains(digest) => pull(rest, seen)
      case digest :: rest =>
        for
          bytes <- source.getBytes(digest)
          _ <- Either.cond(Digest.ofBytes(bytes) == digest, (), s"artifact ${digest.short} failed digest verification")
          artifact <- Artifact.decode(bytes)
          dependencies <- ArtifactDependencies.direct(artifact)
          _ = local.putBytes(bytes)
          result <- pull(dependencies ++ rest, seen + digest)
        yield result
    pull(List(root), Set.empty)

  private def language(spec: ApplicationLanguage): Either[String, ComposedLanguage] = for
    languageArtifact <- local.getByDigest(spec.language)
    _ <- Either.cond(languageArtifact.kind == ArtifactKind.Language, (), s"${spec.name}: language has wrong artifact kind")
    fragmentDigests <- ArtifactDependencies.direct(languageArtifact)
    fragments <- fragmentDigests.foldLeft[Either[String, List[Fragment]]](Right(Nil)) { (acc, digest) => for
      xs <- acc
      artifact <- local.getByDigest(digest)
      _ <- Either.cond(artifact.kind == ArtifactKind.Fragment, (), s"${spec.name}: dependency ${digest.short} is not a fragment")
      fragment <- scala.util.Try(FragmentCodec.fromCanon(artifact.body)).toEither.left.map(_.getMessage)
    yield xs :+ fragment }
    composed <- Compose.compose(spec.name, fragments).left.map(_.map(_.render).mkString("; "))
    grammarArtifact <- local.getByDigest(spec.grammar)
    _ <- Either.cond(grammarArtifact.kind == ArtifactKind.Grammar, (), s"${spec.name}: grammar has wrong artifact kind")
    grammar <- scala.util.Try(GrammarSpec.fromCanon(grammarArtifact.body)).toEither.left.map(_.getMessage)
    _ <- GrammarLint.errors(grammar) match
      case Nil => Right(())
      case errors => Left(errors.map(i => s"${i.where}: ${i.msg}").mkString("; "))
    specs = grammar.categories.flatMap(_.ctors).map(c => c.tag -> c).toMap
    constructors = composed.constructors.map { (tag, ctor) =>
      val labeled = specs.get(tag).map(s => ConstructorSpec.argLabels(s.elems, grammar.tokens.keywords.toSet))
        .filter(_.length == ctor.argSorts.length).map(labels => ctor.copy(argLabels = labels)).getOrElse(ctor)
      tag -> labeled }
    resolved = composed.copy(grammar = grammar, constructors = constructors)
    _ <- Either.cond(resolved.digest == spec.language, (), s"${spec.name}: reconstructed language digest mismatch")
  yield resolved

  def resolve(root: Digest): Either[String, ResolvedApplication] = for
    rootArtifact <- local.getByDigest(root)
    manifest <- ApplicationManifest.fromArtifact(rootArtifact)
    languagePairs <- manifest.languages.foldLeft[Either[String, List[(String, ResolvedLanguageCapabilities)]]](Right(Nil)) {
      (acc, spec) => for
        xs <- acc
        lang <- language(spec)
        descriptor <- local.getByDigest(spec.capabilities)
        _ <- Either.cond(descriptor.kind == ArtifactKind.LanguageCapabilities, (),
          s"${spec.name}: capabilities have wrong artifact kind")
        parsed <- scala.util.Try(LanguageCapabilities.fromCanon(descriptor.body)).toEither
          .left.map(e => s"${spec.name}: invalid capability descriptor: ${e.getMessage}")
        semantics <- local.getByDigest(parsed.changeSemantics)
        surface <- local.getByDigest(parsed.changeSurface)
        extensionDigests = parsed.validation.toList ++ parsed.migrations ++ parsed.queries ++ parsed.policies ++ parsed.projections
        extensions <- extensionDigests.foldLeft[Either[String, List[Artifact]]](Right(Nil)) { (a, d) =>
          for done <- a; artifact <- local.getByDigest(d) yield done :+ artifact }
        capabilities <- LanguageCapabilities.fromArtifacts(descriptor, lang, semantics, surface, extensions)
      yield xs :+ (spec.name -> capabilities) }
    entryPairs <- manifest.entries.foldLeft[Either[String, List[(String, Artifact)]]](Right(Nil)) { (acc, entry) => for
      xs <- acc
      artifact <- local.getByDigest(entry.artifact)
      _ <- Either.cond(artifact.kind == entry.kind, (), s"entry '${entry.name}' expected ${entry.kind.name}, got ${artifact.kind.name}")
    yield xs :+ (entry.name -> artifact) }
    installed <- audit(root)
  yield ResolvedApplication(root, manifest, languagePairs.toMap, entryPairs.toMap, installed)

  def audit(root: Digest): Either[String, Set[Digest]] =
    def walk(todo: List[Digest], seen: Set[Digest]): Either[String, Set[Digest]] = todo match
      case Nil => Right(seen)
      case d :: rest if seen(d) => walk(rest, seen)
      case d :: rest =>
        for
          artifact <- local.getByDigest(d)
          deps <- ArtifactDependencies.direct(artifact)
          result <- walk(deps ++ rest, seen + d)
        yield result
    walk(List(root), Set.empty)
