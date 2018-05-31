import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest
import scala.tools.nsc.doc.{ DocFactory, Universe }

object Test extends ScaladocModelTest {
  override def resourceFile = "simulacrum_2.scala"

  def scaladocSettings = "-Ymacro-annotations"

  override def model: Option[Universe] = {
    val docf = newDocFactory

    // first compile the macro
    new docf.compiler.Run() compile List(new java.io.File(resourcePath + "/" + "simulacrum_1.scala").getPath)
    docf.makeUniverse(Right(code))
  }

  def testModel(rootPackage: Package) = {
    import access._

    rootPackage._class("TestSimulacrum").members
    rootPackage._trait("Applicative").members
  }
}