import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest
import language._
import scala.tools.nsc.doc.base.comment.Comment

object Test extends ScaladocModelTest {

  override def resourceFile = "t10325.scala"

  override def scaladocSettings = ""

  def testModel(root: Package) = {
    import access._

    val base = root._package("scala")._package("test")._package("scaladoc")

    def printTags(tag: String) = {
      val t = base._trait(tag)
      val comment: Comment = t.comment.get
      comment.note foreach println
      comment.authors foreach println
      comment.see foreach println
      comment.todo foreach println
      comment.example foreach println
    }

    printTags("Note")
    printTags("Author")
    printTags("See")
    printTags("Todo")
    printTags("Example")
  }
}
