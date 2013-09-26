import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

// Don't dealias just to print a Function or Tuple type.
object Test extends ScaladocModelTest {

  override def code = """
  class Test {
     type FInt = Function0[Int]
     type TInt = Tuple1[Int]
     def foo: FInt
     def bar: TInt
  }
                      """

  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    import access._
    List("foo", "bar").foreach { name =>
      println("foo: " + rootPackage._class("Test")._method(name).resultType.name)
    }
  }
}
