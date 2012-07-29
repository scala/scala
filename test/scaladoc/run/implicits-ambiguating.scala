import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  // test a file instead of a piece of code
  override def resourceFile = "implicits-ambiguating-res.scala"

  // start implicits
  def scaladocSettings = "-implicits"

  def testModel(root: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    def isAmbiguous(mbr: MemberEntity): Boolean =
      mbr.byConversion.map(_.source.implicitsShadowing.get(mbr).map(_.isAmbiguous).getOrElse(false)).getOrElse(false)

    // SEE THE test/resources/implicits-chaining-res.scala FOR THE EXPLANATION OF WHAT'S CHECKED HERE:
    val base = root._package("scala")._package("test")._package("scaladoc")._package("implicits")._package("ambiguating")
    var conv1: ImplicitConversion = null
    var conv2: ImplicitConversion = null

//// class A ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val A = base._class("A")

    conv1 = A._conversion(base._object("A").qualifiedName + ".AtoX")
    conv2 = A._conversion(base._object("A").qualifiedName + ".AtoZ")
    assert(conv1.members.length == 11)
    assert(conv2.members.length == 11)
    assert(conv1.constraints.length == 0)
    assert(conv2.constraints.length == 0)

    /** - conv1-5 should be ambiguous
     *  - conv6-7 should not be ambiguous
     *  - conv8 should be ambiguous
     *  - conv9 should be ambiguous
     *  - conv10 and conv11 should not be ambiguous */
    def check1to9(cls: String): Unit = {
      for (conv <- (1 to 5).map("conv" + _)) {
        assert(isAmbiguous(conv1._member(conv)), cls + " - AtoX." + conv + " is ambiguous")
        assert(isAmbiguous(conv2._member(conv)), cls + " - AtoZ." + conv + " is ambiguous")
      }
      for (conv <- (6 to 7).map("conv" + _)) {
        assert(!isAmbiguous(conv1._member(conv)), cls + " - AtoX." + conv + " is not ambiguous")
        assert(!isAmbiguous(conv2._member(conv)), cls + " - AtoZ." + conv + " is not ambiguous")
      }
      assert(isAmbiguous(conv1._member("conv8")), cls + " - AtoX.conv8 is ambiguous")
      assert(isAmbiguous(conv2._member("conv8")), cls + " - AtoZ.conv8 is ambiguous")
      assert(isAmbiguous(conv1._member("conv9")), cls + " - AtoX.conv9 is ambiguous")
      assert(isAmbiguous(conv2._member("conv9")), cls + " - AtoZ.conv9 is ambiguous")
    }
    check1to9("A")
    assert(!isAmbiguous(conv1._member("conv10")), "A - AtoX.conv10 is not ambiguous")
    assert(!isAmbiguous(conv2._member("conv10")), "A - AtoZ.conv10 is not ambiguous")
    assert(!isAmbiguous(conv1._member("conv11")), "A - AtoX.conv11 is not ambiguous")
    assert(!isAmbiguous(conv2._member("conv11")), "A - AtoZ.conv11 is not ambiguous")

//// class B ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val B = base._class("B")

    conv1 = B._conversion(base._object("A").qualifiedName + ".AtoX")
    conv2 = B._conversion(base._object("A").qualifiedName + ".AtoZ")
    assert(conv1.members.length == 11)
    assert(conv2.members.length == 11)
    assert(conv1.constraints.length == 0)
    assert(conv2.constraints.length == 0)

    /** conv1-9 should be the same, conv10 should be ambiguous, conv11 should be okay */
    check1to9("B")
    assert(isAmbiguous(conv1._member("conv10")), "B - AtoX.conv10 is ambiguous")
    assert(isAmbiguous(conv2._member("conv10")), "B - AtoZ.conv10 is ambiguous")
    assert(!isAmbiguous(conv1._member("conv11")), "B - AtoX.conv11 is not ambiguous")
    assert(!isAmbiguous(conv2._member("conv11")), "B - AtoZ.conv11 is not ambiguous")

//// class C ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val C = base._class("C")

    conv1 = C._conversion(base._object("A").qualifiedName + ".AtoX")
    conv2 = C._conversion(base._object("A").qualifiedName + ".AtoZ")
    assert(conv1.members.length == 11)
    assert(conv2.members.length == 11)
    assert(conv1.constraints.length == 0)
    assert(conv2.constraints.length == 0)

    /** conv1-9 should be the same, conv10 and conv11 should not be ambiguous */
    check1to9("C")
    assert(!isAmbiguous(conv1._member("conv10")), "C - AtoX.conv10 is not ambiguous")
    assert(!isAmbiguous(conv2._member("conv10")), "C - AtoZ.conv10 is not ambiguous")
    assert(!isAmbiguous(conv1._member("conv11")), "C - AtoX.conv11 is not ambiguous")
    assert(!isAmbiguous(conv2._member("conv11")), "C - AtoZ.conv11 is not ambiguous")

//// class D ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val D = base._class("D")

    conv1 = D._conversion(base._object("A").qualifiedName + ".AtoX")
    conv2 = D._conversion(base._object("A").qualifiedName + ".AtoZ")
    assert(conv1.members.length == 11)
    assert(conv2.members.length == 11)
    assert(conv1.constraints.length == 0)
    assert(conv2.constraints.length == 0)

    /** conv1-9 should be the same, conv10 should not be ambiguous while conv11 should be ambiguous */
    check1to9("D")
    assert(!isAmbiguous(conv1._member("conv10")), "D - AtoX.conv10 is not ambiguous")
    assert(!isAmbiguous(conv2._member("conv10")), "D - AtoZ.conv10 is not ambiguous")
    assert(isAmbiguous(conv1._member("conv11")), "D - AtoX.conv11 is ambiguous")
    assert(isAmbiguous(conv2._member("conv11")), "D - AtoZ.conv11 is ambiguous")
  }
}