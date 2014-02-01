import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
  import scala.language.experimental.macros
  class Test {
     def print(): Unit = macro ???
  }
                      """

  def scaladocSettings = ""

  def testModel(root: Package) = {
    import access._
    val p = root._class("Test")._method("print")

    println(p.annotations) // no annotations
    println(p.flags) // a 'macro' flag
  }
}
