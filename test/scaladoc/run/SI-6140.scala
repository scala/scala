import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  // This caused an infinite recursion in method inline() in CommentFactory.scala
  override def code = """
      /** {{ code? }} */
      class C
  """

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    // if it doesn't hang, the test is passed
  }
}
