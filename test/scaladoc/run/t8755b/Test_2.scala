import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.model.diagram._
import scala.tools.partest.ScaladocModelTest
import scala.tools.nsc.plugins.PluginDescription
import scala.util.chaining._

object Test extends ScaladocModelTest {

  override def code = """
    class C
  """

  override def scaladocSettings = s"-Xplugin:$testOutput -Xplugin-require:ploogin"

  override def testModel(rootPackage: Package) = ()

  override def newDocFactory =
    super.newDocFactory.tap(df => println(df.compiler.phaseNames.mkString(" -> ")))

  override def show() = {
    val xml  = PluginDescription("ploogin", "t8755.Ploogin").toXML
    (testOutput / "scalac-plugin.xml").toFile.writeAll(xml)
    super.show()
  }
}
