import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.model.diagram._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
      package scala.test.scaladoc.diagrams

      trait A
      trait B extends A
      trait C extends B
      trait D extends C with A
      trait E extends C with A with D
    """

  // diagrams must be started. In case there's an error with dot, it should not report anything
  def scaladocSettings = "-diagrams -implicits"

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    def diagramString(rootPackage: Package) = {
      val base = rootPackage._package("scala")._package("test")._package("scaladoc")._package("diagrams")
      val A = base._trait("A")
      val B = base._trait("B")
      val C = base._trait("C")
      val D = base._trait("D")
      val E = base._trait("E")

      base.contentDiagram.get.toString + "\n" +
         A.inheritanceDiagram.get.toString + "\n" +
         B.inheritanceDiagram.get.toString + "\n" +
         C.inheritanceDiagram.get.toString + "\n" +
         D.inheritanceDiagram.get.toString + "\n" +
         E.inheritanceDiagram.get.toString
    }

    // 1. check that several runs produce the same output
    val run0 = diagramString(rootPackage)
    val run1 = diagramString(model.getOrElse({sys.error("Scaladoc Model Test ERROR: No universe generated!")}).rootPackage)
    val run2 = diagramString(model.getOrElse({sys.error("Scaladoc Model Test ERROR: No universe generated!")}).rootPackage)
    val run3 = diagramString(model.getOrElse({sys.error("Scaladoc Model Test ERROR: No universe generated!")}).rootPackage)

    // any variance in the order of the diagram elements should crash the following tests:
    assert(run0 == run1)
    assert(run1 == run2)
    assert(run2 == run3)

    // 2. check the order in the diagram: this node, subclasses, and then implicit conversions
    def assertRightOrder(template: DocTemplateEntity, diagram: Diagram) =
      for ((node, subclasses) <- diagram.edges)
        assert(subclasses == subclasses.filter(_.isThisNode) :::
                             subclasses.filter(node => node.isNormalNode || node.isOutsideNode) :::
                             subclasses.filter(_.isImplicitNode),
               "Diagram order for " + template + ": " + subclasses)

    val base = rootPackage._package("scala")._package("test")._package("scaladoc")._package("diagrams")
    assertRightOrder(base, base.contentDiagram.get)
    assertRightOrder(base._trait("A"), base._trait("A").inheritanceDiagram.get)
    assertRightOrder(base._trait("B"), base._trait("B").inheritanceDiagram.get)
    assertRightOrder(base._trait("C"), base._trait("C").inheritanceDiagram.get)
    assertRightOrder(base._trait("D"), base._trait("D").inheritanceDiagram.get)
    assertRightOrder(base._trait("E"), base._trait("E").inheritanceDiagram.get)
  }
}