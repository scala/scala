import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  override def code = """
class C {
  /**
   *  @usecase def zipWithIndex: $NotFound
   *
   */
  def zipWithIndex: Int = ???
}
  """

  def scaladocSettings = ""

  def testModel(root: Package) = {
    // just testing that it doesn't error out.
  }
}
