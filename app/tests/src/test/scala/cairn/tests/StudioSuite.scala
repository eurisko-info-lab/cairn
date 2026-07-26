package cairn.tests

import cairn.kernel.*
import cairn.core.*

class StudioSuite extends munit.FunSuite:
  private val language = Meta.parseFile("""language studio-sds {
    |  fragment studio-sds {
    |    sort Mixture tree;
    |    sort Component tree;
    |    ctor mixture : Mixture(components: Component);
    |    ctor component : Component(ref: Ref, percentage: Percentage, phrase: Phrase);
    |    key Component by ref;
    |    keyword mixture;
    |    keyword component;
    |    keyword ref;
    |    keyword percentage;
    |    keyword phrase;
    |    punct "[";
    |    punct "]";
    |    punct ",";
    |    syntax Mixture += mixture : tok "mixture" tok "[" sepby1 cat Component "," tok "]";
    |    syntax Component += component : tok "component" tok "ref" name tok "percentage" name tok "phrase" str;
    |    top Mixture;
    |  }
    |}""".stripMargin).fold(e => fail(e), identity)
  private val acetone = Cst.node("component", Cst.Leaf("acetone"), Cst.Leaf("40"), Cst.Leaf("Acétone"))
  private val ethanol = Cst.node("component", Cst.Leaf("ethanol"), Cst.Leaf("60"), Cst.Leaf("Éthanol"))
  private val root = Cst.node("mixture", Cst.Node("list", List(acetone, ethanol)))
  private val base = Module(List("sheet" -> root))

  test("grammar-derived forms expose FieldIds and keyed-list identity"):
    val component = Studio.forms(language, "Component").head
    assertEquals(component.fields.map(_.fieldId), List(Some("ref"), Some("percentage"), Some("phrase")))
    assertEquals(component.fields.map(_.label), List(Some("ref"), Some("percentage"), Some("phrase")))
    assertEquals(component.keyedBy, Some("ref"))

  test("keyed-list widget emits an ordinary Δ term and never mutates the base module"):
    val path = Studio.pathFromTraversal(language, root, List(0, 1, 1)).fold(e => fail(e), identity)
    assert(path.steps.exists {
      case SemanticPath.Step.KeyedElement("Component", "ref", "ethanol") => true
      case _ => false
    })
    val proposal = Studio.propose(language, base,
      StudioAction.ReplaceAt("sheet", path, Cst.Leaf("55"))).fold(e => fail(e), identity)
    assertEquals(base.get("sheet"), Some(root))
    assertEquals(proposal.validatedChange.base, base.digest)
    assertEquals(proposal.validatedChange.result, proposal.result.digest)
    assert(proposal.change.render.contains(Delta.tag(language, "edit")))
    assert(proposal.accesses.accesses.exists(_.location == SemanticLocation.Subtree("sheet", path)))
    val changed = proposal.result.get("sheet").get
    assertEquals(Delta.subtreeAt(changed, List(0, 1, 1)), Right(Cst.Leaf("55")))

  test("multilingual phrase/shadow widget is the same semantic-path Δ edit"):
    val phrasePath = Studio.pathFromTraversal(language, root, List(0, 0, 2)).toOption.get
    val proposal = Studio.propose(language, base,
      StudioAction.ReplaceAt("sheet", phrasePath, Cst.Leaf("Acetone shadow"))).toOption.get
    assertEquals(Delta.subtreeAt(proposal.result.get("sheet").get, List(0, 0, 2)), Right(Cst.Leaf("Acetone shadow")))
    assert(proposal.accesses.accesses.map(_.location).contains(SemanticLocation.Subtree("sheet", phrasePath)))

  test("inline diagnostics point back to the rejected semantic widget"):
    val path = Studio.pathFromTraversal(language, root, List(0, 0, 1)).toOption.get
    val action = StudioAction.ReplaceAt("sheet", path, Cst.Leaf("101"))
    val gate = ModuleGate.host("percentage")(_ => Left("percentage must be between 0 and 100"))
    val diagnostics = Studio.diagnostics(language, base, action, gate = gate)
    assertEquals(diagnostics.map(_.location), List(Some(SemanticLocation.Subtree("sheet", path))))
    assert(diagnostics.head.message.contains("percentage must be between 0 and 100"))

  test("source-preserving proposal changes only the selected field"):
    val grammar = ModuleSurface.grammar(language)
    val source = """
      |sheet   =   mixture [component ref acetone percentage p40 phrase "Acétone", component ref ethanol percentage p60 phrase "Éthanol"] ;
      |""".stripMargin
    val parsedRoot = Parser.parse(grammar, source).flatMap(ModuleSurface.toModule)
      .flatMap(_.get("sheet").toRight("missing sheet")).fold(e => fail(e), identity)
    val path = Studio.pathFromTraversal(language, parsedRoot, List(0, 1, 2)).toOption.get
    val proposal = Studio.proposePreservingSource(language, grammar, source,
      StudioAction.ReplaceAt("sheet", path, Cst.Leaf("Ethanol shadow"))).fold(e => fail(e), identity)
    val preview = proposal.sourcePreview.get
    assert(preview.startsWith("\nsheet   =   mixture"))
    assert(preview.contains("component ref acetone percentage p40 phrase \"Acétone\""))
    assert(preview.contains("component ref ethanol percentage p60 phrase \"Ethanol shadow\""))

  test("semantic navigation exposes FieldIds and keyed identities without positional controls"):
    val navigation = Studio.navigateDefinition(language, "sheet", root).fold(e => fail(e), identity)
    def flatten(node: StudioNavigationNode): List[StudioNavigationNode] = node :: node.children.flatMap(flatten)
    val nodes = flatten(navigation)
    assert(nodes.exists(_.label == "percentage"))
    assert(nodes.exists(_.location match
      case SemanticLocation.Subtree(_, path) => path.steps.exists {
        case SemanticPath.Step.KeyedElement("Component", "ref", "acetone") => true
        case _ => false
      }
      case _ => false))
    assert(!nodes.exists(_.label.matches("[0-9]+")))

  test("capability-selected Studio profiles round-trip as pack projection artifacts"):
    val semantics = StudioProfileSemantics(language.digest, List("Mixture"),
      List(StudioView("Product", "Mixture", List("components"))),
      List(StudioCommand("edit percentage", StudioTemplate.ReplaceAt, Some("Percentage"))),
      List(List("Product", "components")), Nil)
    val surface = StudioProfileSurface(semantics.digest, Map("components" -> "Mixture"),
      Map("percentage" -> "Percentage"), List(StudioWidgetHint("Percentage", "decimal-percent")),
      Map("percentage" -> "Enter a value from 0 to 100"), List("Product", "components"))
    val standard = LanguageCapabilities.standard(language)
    val descriptor = standard.descriptor.copy(projections = List(semantics.digest, surface.digest))
    val capabilities = standard.copy(descriptor = descriptor, projections = List(semantics.artifact, surface.artifact))
    val selected = capabilities.studioProfile.fold(e => fail(e), identity).get
    assertEquals(selected.semantics, semantics)
    assertEquals(selected.surface.widgetHints.head.widget, "decimal-percent")
    assertEquals(LanguageCapabilities.standard(language).studioProfile, Right(None))

  test("proposal workspace stages one changeset and undo is proven by ChangeAlgebra inversion"):
    val phrase = Studio.pathFromTraversal(language, root, List(0, 0, 2)).toOption.get
    val percentage = Studio.pathFromTraversal(language, root, List(0, 1, 1)).toOption.get
    val workspace = StudioWorkspace(language, base)
      .stage(StudioAction.ReplaceAt("sheet", phrase, Cst.Leaf("Acetone"))).toOption.get
      .stage(StudioAction.ReplaceAt("sheet", percentage, Cst.Leaf("55"))).toOption.get
    assertEquals(workspace.actions.length, 2)
    assertEquals(workspace.proposal.get.validatedChange.base, base.digest)
    assertEquals(workspace.proposal.get.accesses.accesses.count(_.mode == AccessMode.Write), 2)
    val undone = workspace.undoLast.fold(e => fail(e), identity)
    assertEquals(undone.actions.length, 1)
    assertEquals(Delta.subtreeAt(undone.proposal.get.result.get("sheet").get, List(0, 1, 1)), Right(Cst.Leaf("60")))
