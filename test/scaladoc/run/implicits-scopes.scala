import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest
import language._

object Test extends ScaladocModelTest {

  // test a file instead of a piece of code
  override def resourceFile = "implicits-scopes-res.scala"

  // start implicits
  def scaladocSettings = "-implicits"

  def testModel(root: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._
    var conv: ImplicitConversion = null

    // SEE THE test/resources/implicits-chaining-res.scala FOR THE EXPLANATION OF WHAT'S CHECKED HERE:
    val base = root._package("scala")._package("test")._package("scaladoc")._package("implicits")._package("scopes")

//// test1 /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    val doTest1 = {
      val test1 = base._package("test1")
      val A = test1._class("A")

      conv = A._conversion(test1.qualifiedName + ".toB")
      assert(conv.members.length == 1)
      assert(conv.constraints.length == 0)
    }

//// test2 /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    val doTest2 = {
      val test2 = base._package("test2")
      val classes = test2._package("classes")
      val A = classes._class("A")

      conv = A._conversion(test2.qualifiedName + ".toB")
      assert(conv.members.length == 1)
      assert(conv.constraints.length == 0)
    }

//// test3 /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    val doTest3 = {
      val test3 = base._package("test3")
      val A = test3._class("A")

      conv = A._conversion(A.qualifiedName + ".toB")
      assert(conv.members.length == 1)
      assert(conv.constraints.length == 0)
    }

//// test4 /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    val doTest4 = {
      val test4 = base._package("test4")
      val A = test4._class("A")
      val S = test4._object("S")

      conv = A._conversion(S.qualifiedName + ".toB")
      assert(conv.members.length == 1)
      assert(conv.constraints.length == 0)
    }

//// test5 /////////////////////////////////////////////////////////////////////////////////////////////////////////////

    val doTest5 = {
      val test5 = base._package("test5")
      val scope = test5._object("scope")
      val A = scope._class("A")

      conv = A._conversion(scope.qualifiedName + ".toB")
      assert(conv.members.length == 1)
      assert(conv.constraints.length == 0)
    }
  }
}
