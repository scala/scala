import scala.tools.nsc.doc.Universe
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocJavaModelTest

object Test extends ScaladocJavaModelTest {

  override def resourceFile = "t10027.java"
  override def scaladocSettings = ""

  // just make sure it compiles
  def testModel(rootPackage: Package) = {}
}
