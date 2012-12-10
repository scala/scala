import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  // Test code
  override def code = """
      // This the default values should be displayed

      object Test {
        def f   (a: Any = "".isEmpty) = ()
        def g[A](b: A   = null) = ()
      }
                      """

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val Test = rootPackage._object("Test")
    val f = Test._method("f")
    val g = Test._method("g")

    def assertEqual(s1: String, s2: String) = assert(s1 == s2, s1 + " == " + s2)

    assertEqual(f.valueParams(0)(0).defaultValue.get.expression, "\"\".isEmpty")
    assertEqual(g.valueParams(0)(0).defaultValue.get.expression, "null")
  }
}