import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest
import language._

object Test extends ScaladocModelTest {

  // test a file instead of a piece of code
  override def resourceFile = "implicits-chaining-res.scala"

  // start implicits
  def scaladocSettings = "-implicits"

  def testModel(root: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    // SEE THE test/resources/implicits-chaining-res.scala FOR THE EXPLANATION OF WHAT'S CHECKED HERE:
    val base = root._package("scala")._package("test")._package("scaladoc")._package("implicits")._object("chaining")
    var conv: ImplicitConversion = null

//// class A ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val A = base._class("A")

    conv = A._conversion(base.qualifiedName + ".convertToZ")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 1)

//// class B ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val B = base._class("B")

    conv = B._conversion(base.qualifiedName + ".convertToZ")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 0)

//// class C ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val C = base._class("C")

    assert(C._conversions(base.qualifiedName + ".convertToZ").isEmpty)

//// class D ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val D = base._class("D")

    conv = D._conversion(base.qualifiedName + ".convertToZ")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 0)

//// class E ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val E = base._class("E")

    conv = E._conversion(base.qualifiedName + ".convertToZ")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 0)

//// class F ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val F = base._class("F")

    assert(F._conversions(base.qualifiedName + ".convertToZ").isEmpty)
  }
}
