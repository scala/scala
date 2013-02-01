import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def resourceFile: String = "SI-6511.scala"

  // no need for special settings
  def scaladocSettings = "-diagrams"

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val main = rootPackage._package("test")._package("scaladoc")._package("template")._package("diagrams")
    val X = main._trait("X")
    val Y = main._trait("Y")

    testDiagram(X, X.contentDiagram, nodes = 4, edges = 3)
    testDiagram(Y, Y.contentDiagram, nodes = 4, edges = 3)
  }
}