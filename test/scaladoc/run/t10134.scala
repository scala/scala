import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocJavaModelTest

object Test extends ScaladocJavaModelTest {

  override def resourceFile = "t10134.java"
  override def scaladocSettings = ""
  override def code = """
import example.T10134_Row
@T10134_Row
object foo {}
  """

  def testModel(root: Package) = {
    import access._
    // just testing that it doesn't error out.
  }
}

