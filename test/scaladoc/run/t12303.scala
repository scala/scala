
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def resourceFile = "t12303.scala"

  override def scaladocSettings = "-Xlint"

  def testModel(root: Package) = {
  }
}
