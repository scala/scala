import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest
import scala.tools.nsc.doc.{ DocFactory, Universe }

object Test extends ScaladocModelTest {
  override def resourceFile = "simulacrum_2.scala"

  def scaladocSettings = "-Ymacro-annotations"

  override def extraSettings = super.extraSettings + s" -cp ${testOutput.path}"

  override def model: Option[Universe] = {
    val macroCode = new reflect.io.File(new java.io.File(resourcePath.toString + "/" + "simulacrum_1.scala")).slurp()

    // before generating the model, first compile the macro (using a full compiler)
    compileString(newCompiler())(macroCode)

    super.model
  }

  def testModel(rootPackage: Package) = {
    import access._

    rootPackage._class("TestSimulacrum").members
    rootPackage._trait("Applicative").members
  }
}
