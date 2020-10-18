import scala.tools.nsc.doc.model._
import scala.tools.partest.{ScaladocJavaModelTest, ScaladocModelTest}

object Test extends ScaladocModelTest {

  override def resourceFile = "t10134_foo.scala"
  override def scaladocSettings = ""

  def testModel(root: Package) = {
    import access._
    // Just testing that we haven't hit a compiler error.
  }
}

