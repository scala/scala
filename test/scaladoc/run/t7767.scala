import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  // This caused an infinite recursion in method inline() in CommentFactory.scala
  override def code = """
      class Docable extends { /**Doc*/ val foo = 0 } with AnyRef
  """

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    import access._
    // if it doesn't hang, the test is passed
    val comment = rootPackage._class("Docable")._value("foo").comment.map(_.body.toString.trim).getOrElse("")
    assert(comment.contains("Doc"), comment)
  }
}
