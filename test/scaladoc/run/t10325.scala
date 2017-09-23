import scala.tools.nsc.doc.html.Page
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  override def code = """
    /**
      * @note note1
      * @note note2
      * @note note3
      */
     class Foo
  """

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    import scala.tools.nsc.doc.base.comment._
    import access._

    val foo = rootPackage._class("Foo")

    val notesInComment = foo.comment.get.note
    println(notesInComment)
  }
}
