import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  // Working around the fact that usecases have the form Coll[T] and not Coll[T, U], as required by Map
  override def code = """
      package a {
        class A { class Z }
        class C extends b.B { class X extends Y }
      }

      package b {
        /** @contentDiagram */
        class B extends a.A { class Y extends Z }
        /** @contentDiagram */
        class D extends a.C { class V extends X }
      }
  """

  // no need for special settings
  def scaladocSettings = "-diagrams -skip-packages a"

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    // just need to check the member exists, access methods will throw an error if there's a problem
    assert(!rootPackage.templates.exists(_.name == "a"), "package a should not exist in the root package")
    assert(rootPackage.templates.exists(_.name == "b"),  "package b should exist in the root package")
    val b = rootPackage._package("b")
    val B = b._class("B")
    val D = b._class("D")
    testDiagram(B, B.contentDiagram, 2, 1)
    // unfortunately not all packages, as B1 extends A.this.A1 and it gets the wrong member -- maybe we should model
    // things as we do for symbols?
    testDiagram(D, D.contentDiagram, 3, 2)
  }
}