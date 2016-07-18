import scala.tools.nsc.doc.Universe
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def resourceFile = "SI-4826.java"

  // overridden to pass explicit files to newDocFactory.makeUniverse (rather than code strings)
  // since the .java file extension is required
  override def model: Option[Universe] = {
    val path = resourcePath + "/" + resourceFile
    newDocFactory.makeUniverse(Left(List(path)))
  }

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    import access._
    val Tag = ":marker:"

    val base = rootPackage._package("test")._package("scaladoc")
    val clazz = base._class("JavaComments")
    val method = clazz._method("answer")

    assert(extractCommentText(clazz.comment.get).contains(Tag))
    assert(extractCommentText(method.comment.get).contains(Tag))
  }
}
