import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
      class Docable extends { /**Doc*/ val foo = 0 } with AnyRef
  """

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    import access._
    val comment = rootPackage._class("Docable")._value("foo").comment.map(_.body.toString.trim).getOrElse("")
    assert(comment.contains("Doc"), comment)
  }
}
