import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.model.diagram._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
      package scala.test.scaladoc.diagrams

      import language.implicitConversions

      trait A
      trait B
      trait C
      class E extends A with B with C
      object E { implicit def eToT(e: E) = new T }

      class F extends E
      class G extends E
      private class H extends E /* since it's private, it won't go into the diagram */
      class T { def t = true }

      class X
      object X { implicit def xToE(x: X) = new E}
      class Y extends X
      class Z
      object Z { implicit def zToE(z: Z) = new E}
    """

  // diagrams must be started. In case there's an error with dot, it should not report anything
  def scaladocSettings = "-diagrams -implicits"

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val base = rootPackage._package("scala")._package("test")._package("scaladoc")._package("diagrams")
    val E = base._class("E")
    val diag = E.inheritanceDiagram.get

    // there must be a single this node
    assert(diag.nodes.filter(_.isThisNode).length == 1)

    // 1. check class E diagram
    assert(diag.isInheritanceDiagram)

    val (incoming, outgoing) = diag.edges.partition(!_._1.isThisNode)
    assert(incoming.length == 5)
    assert(outgoing.head._2.length == 4, s"${outgoing.head._2} has length ${outgoing.head._2.length}, expecting 4")

    val (outgoingSuperclass, outgoingImplicit) = outgoing.head._2.partition(_.isNormalNode)
    assert(outgoingSuperclass.length == 3)
    assert(outgoingImplicit.length == 1)

    val (incomingSubclass, incomingImplicit) = incoming.partition(_._1.isNormalNode)
    assert(incomingSubclass.length == 2)
    assert(incomingImplicit.length == 3)

    val classDiag = diag.asInstanceOf[InheritanceDiagram]
    assert(classDiag.incomingImplicits.length == 3)
    assert(classDiag.outgoingImplicits.length == 1)

    // 2. check package diagram
    // NOTE: Z should be eliminated because it's isolated
    val packDiag = base.contentDiagram.get
    assert(packDiag.isContentDiagram)
    assert(packDiag.nodes.length == 8) // check singular object removal
    assert(packDiag.edges.length == 4)
    assert(packDiag.edges.foldLeft(0)(_ + _._2.length) == 6)

    // TODO: Should check numbering
  }
}