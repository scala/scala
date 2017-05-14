import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  // Test code
  override def code = """
      // This example should compile without errors, and the pattern match should be correctly displayed

      import language.higherKinds

      abstract class Base[M[_, _]] {
        def foo[A, B]: M[(A, B), Any]
      }

      class Derived extends Base[PartialFunction] {
        def foo[A, B] /*: PartialFunction[(A, B) => Any]*/ = { case (a, b) => (a: A, b: B) }
      }

      object Test {
        lazy val lx = { println("hello"); 3 }
        def test1(x: Int = lx) = ???
        def test2(x: Int = lx match { case 0 => 1; case 3 => 4 }) = ???
      }
  """

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val Test = rootPackage._object("Test")
    val test1 = Test._method("test1")
    val test2 = Test._method("test2")

    def assertEqual(s1: String, s2: String) = assert(s1 == s2, s1 + " == " + s2)

    assertEqual(test1.valueParams(0)(0).defaultValue.get.expression, "lx")
    assertEqual(test2.valueParams(0)(0).defaultValue.get.expression, "lx match { case 0 => 1; case 3 => 4 }")
  }
}