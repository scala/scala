import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  // test a file instead of a piece of code
  override def resourceFile = "implicits-shadowing-res.scala"

  // start implicits
  def scaladocSettings = "-implicits"

  def testModel(root: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    def isShadowed(mbr: MemberEntity): Boolean =
      mbr.byConversion.map(_.source.implicitsShadowing.get(mbr).map(_.isShadowed).getOrElse(false)).getOrElse(false)

    // SEE THE test/resources/implicits-chaining-res.scala FOR THE EXPLANATION OF WHAT'S CHECKED HERE:
    val base = root._package("scala")._package("test")._package("scaladoc")._package("implicits")._package("shadowing")
    var conv: ImplicitConversion = null

//// class A ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val A = base._class("A")

    conv = A._conversion(base._object("A").qualifiedName + ".AtoZ")
    assert(conv.members.length == 11)
    assert(conv.members.forall(isShadowed(_)))
    assert(conv.constraints.length == 0)

//// class B ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val B = base._class("B")

    conv = B._conversion(base._object("A").qualifiedName + ".AtoZ")
    assert(conv.members.length == 11)
    assert(conv.members.forall(isShadowed(_)))
    assert(conv.constraints.length == 0)

//// class C ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val C = base._class("C")

    conv = C._conversion(base._object("A").qualifiedName + ".AtoZ")
    assert(conv.members.length == 11)
    assert(conv.members.forall(isShadowed(_)))
    assert(conv.constraints.length == 0)

//// class D ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val D = base._class("D")

    conv = D._conversion(base._object("A").qualifiedName + ".AtoZ")
    assert(conv.members.length == 11)
    assert(conv.members.forall(isShadowed(_)))
    assert(conv.constraints.length == 0)
  }
}
