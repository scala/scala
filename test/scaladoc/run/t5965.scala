import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
        abstract class Param
        class Test
        object Test {
          def apply(i: Int): Test = new Test
          def apply(i: Int, p: Param = new Param { }): Test = new Test
        }
  """

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    import access._

    // just need to make sure the model exists
    val base = rootPackage._object("Test")
  }
}