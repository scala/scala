
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def resourceFile = "t13129.scala"

  override def scaladocSettings = ""

  def testModel(root: Package) = {
    import access._

    val base = root._package("scala")._package("test")._package("scaladoc")._object("Main")

    println(extractCommentText(base._method("main").comment.get))
  }
}
