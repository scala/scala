import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.model.diagram._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
        package scala.test.scaladoc

        /** @contentDiagram hideNodes "scala.test.*.A" "java.*", hideEdges ("*G" -> "*E") */
        package object diagrams {
          def foo = 4
        }

        package diagrams {
          import language.implicitConversions

          /** @inheritanceDiagram hideIncomingImplicits, hideNodes "*E" */
          trait A
          trait AA extends A
          trait B
          trait AAA extends B

          /** @inheritanceDiagram hideDiagram */
          trait C
          trait AAAA extends C

          /** @inheritanceDiagram hideEdges("*E" -> "*A") */
          class E extends A with B with C
          class F extends E
          /** @inheritanceDiagram hideNodes "*G" "G" */
          class G extends E
          private class H extends E /* since it's private, it won't go into the diagram */
          class T { def t = true }
          object E {
            implicit def eToT(e: E) = new T
            implicit def eToA(e: E) = new A { }
          }
        }
    """

  // diagrams must be started. In case there's an error with dot, it should not report anything
  def scaladocSettings = "-diagrams -implicits"

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    // base package
    // Assert we have 7 nodes and 6 edges
    val base = rootPackage._package("scala")._package("test")._package("scaladoc")._package("diagrams")
    val packDiag = base.contentDiagram.get
    assert(packDiag.nodes.length == 6)
    assert(packDiag.edges.map(_._2.length).sum == 5)

    // trait A
    // Assert we have just 3 nodes and 2 edges
    val A = base._trait("A")
    val ADiag = A.inheritanceDiagram.get
    assert(ADiag.nodes.length == 3, s"${ADiag.nodes} has length ${ADiag.nodes.length}, expected 3")
    assert(ADiag.edges.map(_._2.length).sum == 2)

    // trait C
    val C = base._trait("C")
    assert(!C.inheritanceDiagram.isDefined)

    // trait G
    val G = base._class("G")
    assert(!G.inheritanceDiagram.isDefined)

    // trait E
    val E = base._class("E")
    val EDiag = E.inheritanceDiagram.get

    // there must be a single this node
    assert(EDiag.nodes.filter(_.isThisNode).length == 1)

    // 1. check class E diagram
    val (incoming, outgoing) = EDiag.edges.partition(!_._1.isThisNode)
    assert(incoming.length == 2) // F and G
    assert(outgoing.head._2.length == 3) // B, C and T

    val (outgoingSuperclass, outgoingImplicit) = outgoing.head._2.partition(_.isNormalNode)
    assert(outgoingSuperclass.length == 2) // B and C
    assert(outgoingImplicit.length == 1, outgoingImplicit) // T

    val (incomingSubclass, incomingImplicit) = incoming.partition(_._1.isNormalNode)
    assert(incomingSubclass.length == 2) // F and G
    assert(incomingImplicit.length == 0)

    assert(EDiag.nodes.length == 6) // E, B and C, F and G and the implicit conversion to T
  }
}