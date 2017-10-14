import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  override def code = """
object U {
  /**
   *  @usecase val a: Int = ???
   */
  val a: Int = 1
}
  """

  def scaladocSettings = ""

  def testModel(root: Package) = {
    import access._
    // just testing that it doesn't error out.
  }
}
