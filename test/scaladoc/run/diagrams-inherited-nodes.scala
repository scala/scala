import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.model.diagram._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
        package scala.test.scaladoc.diagrams.inherited.nodes {

          /** @contentDiagram
           *  @inheritanceDiagram hideDiagram */
          trait T1 {
            trait A1
            trait A2 extends A1
            trait A3 extends A2
          }

          /** @contentDiagram
           *  @inheritanceDiagram hideDiagram */
          trait T2 extends T1 {
            trait B1 extends A1
            trait B2 extends A2 with B1
            trait B3 extends A3 with B2
          }

          /** @contentDiagram
           *  @inheritanceDiagram hideDiagram */
          trait T3 {
            self: T1 with T2 =>
            trait C1 extends B1
            trait C2 extends B2 with C1
            trait C3 extends B3 with C2
          }

          /** @contentDiagram
           *  @inheritanceDiagram hideDiagram */
          trait T4 extends T3 with T2 with T1 {
            trait D1 extends C1
            trait D2 extends C2 with D1
            trait D3 extends C3 with D2
          }
        }
    """

  // diagrams must be started. In case there's an error with dot, it should not report anything
  def scaladocSettings = "-diagrams"

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    // base package
    // Assert we have 7 nodes and 6 edges
    val base = rootPackage._package("scala")._package("test")._package("scaladoc")._package("diagrams")._package("inherited")._package("nodes")

    def checkDiagram(t: String, nodes: Int, edges: Int) = {
      // trait T1
      val T = base._trait(t)
      val TDiag = T.contentDiagram.get
      assert(TDiag.nodes.length == nodes, t + ": " + TDiag.nodes + ".length == " + nodes)
      assert(TDiag.edges.map(_._2.length).sum == edges, t + ": " + TDiag.edges.mkString("List(\n", ",\n", "\n)") + ".map(_._2.length).sum == " + edges)
    }

    checkDiagram("T1", 3, 2)
    checkDiagram("T2", 6, 7)
    checkDiagram("T3", 3, 2)
    checkDiagram("T4", 12, 17)
  }
}