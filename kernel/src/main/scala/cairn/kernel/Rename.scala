package cairn.kernel

/** M13: interface renaming morphisms. An [[Import]] carries a rename map that
  * is applied to a fragment before amalgamation, so two fragments using
  * different names for a shared sort/constructor compose along a declared
  * span instead of colliding. Renaming is a total, mechanical morphism over
  * every place a name can occur: sorts, constructors, grammar tags, print
  * rules, rewrite/judgment term tags, interfaces, and the variable ctor.
  */
object Rename:
  def cst(t: Cst, m: Map[String, String]): Cst = t match
    case Cst.Leaf(_)      => t
    case Cst.Node(c, cs)  => Cst.Node(m.getOrElse(c, c), cs.map(cst(_, m)))

  private def elem(e: Elem, m: Map[String, String]): Elem = e match
    case Elem.Cat(n)       => Elem.Cat(m.getOrElse(n, n))
    case Elem.Opt(x)       => Elem.Opt(elem(x, m))
    case Elem.Star(x)      => Elem.Star(elem(x, m))
    case Elem.SepBy1(x, s) => Elem.SepBy1(elem(x, m), s)
    case Elem.Adjacent1(x) => Elem.Adjacent1(elem(x, m))
    case Elem.Block(n)     => Elem.Block(m.getOrElse(n, n))
    case Elem.Run(n)       => Elem.Run(m.getOrElse(n, n))
    case other             => other

  def apply(f: Fragment, m: Map[String, String]): Fragment =
    def r(s: String): String = m.getOrElse(s, s)
    Fragment(
      name = f.name,
      provides = f.provides.map(r),
      requires = f.requires.map(r),
      excludes = f.excludes.map(r),
      sorts = f.sorts.map(s => s.copy(name = r(s.name))),
      constructors = f.constructors.map(c => c.copy(
        name = r(c.name), sort = r(c.sort), argSorts = c.argSorts.map(r))),
      grammar = f.grammar.copy(
        categories = f.grammar.categories.map(cat => CategorySpec(r(cat.name),
          cat.ctors.map(cs => ConstructorSpec(
            if cs.tag == "$group" then cs.tag else r(cs.tag),
            cs.elems.map(elem(_, m)))))),
        precCategories = f.grammar.precCategories.map(p => PrecCategory(r(p.name), r(p.base),
          p.ops.map(o => o.copy(tag = r(o.tag))))),
        printRules = f.grammar.printRules.map(pr => pr.copy(tag = r(pr.tag))),
        top = f.grammar.top.map(r)),
      rewriteRules = f.rewriteRules.map(rr => RewriteRule(rr.name, cst(rr.pattern, m), cst(rr.template, m))),
      judgments = f.judgments.map(j => JudgmentDef(r(j.name),
        j.rules.map(ir => InferRule(ir.name, ir.premises.map(cst(_, m)), cst(ir.conclusion, m))))),
      varCtor = f.varCtor.map(r))

final case class Import(fragment: Fragment, renames: Map[String, String] = Map.empty):
  def resolved: Fragment = if renames.isEmpty then fragment else Rename(fragment, renames)

object ComposeImports:
  /** Pushout along declared spans: renames first, then ordinary amalgamation.
    * The result digest depends only on the RESOLVED fragments — the rename
    * spelling is invisible to identity.
    */
  def compose(name: String, imports: List[Import]): Either[List[ComposeError], ComposedLanguage] =
    Compose.compose(name, imports.map(_.resolved))

/** M14: parameterized fragments (functors over interface parameters).
  * `body` mentions parameter names as sorts; instantiation is a rename
  * morphism plus deterministic name mangling — hash-stable by construction.
  */
final case class FragmentFunctor(name: String, params: List[String], body: Fragment):
  def instantiate(args: Map[String, String]): Either[String, Fragment] =
    val missing = params.filterNot(args.contains)
    if missing.nonEmpty then Left(s"functor '$name' missing arguments: ${missing.mkString(", ")}")
    else
      val argList = params.map(args)
      val instName = s"$name[${argList.mkString(",")}]"
      // qualify every non-parameter definition name so two instantiations coexist
      val defNames = (body.sorts.map(_.name) ++ body.constructors.map(_.name) ++
        body.grammar.categories.map(_.name)).filterNot(params.contains).distinct
      val qualify = defNames.map(n => n -> s"$n[${argList.mkString(",")}]").toMap
      Right(Rename(body, args ++ qualify).copy(name = instName))
