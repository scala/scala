import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  def scaladocSettings = ""

  override def code = "object A { def $$ = 123 }"

  def testModel(rootPackage: Package) = {
    import access._

    val method = rootPackage._object("A")._method("$$")
    assert(method != null)
  }
}
