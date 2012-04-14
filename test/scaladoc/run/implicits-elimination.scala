import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest
import language._

object Test extends ScaladocModelTest {

  // test a file instead of a piece of code
  override def resourceFile = "implicits-elimination-res.scala"

  // start implicits
  def scaladocSettings = "-implicits"

  def testModel(root: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    // SEE THE test/resources/implicits-elimination-res.scala FOR THE EXPLANATION OF WHAT'S CHECKED HERE:
    val base = root._package("scala")._package("test")._package("scaladoc")._package("implicits")._package("elimination")
    val A = base._class("A")

    assert(A._conversions(A.qualifiedName + ".toB").isEmpty)
  }
}
