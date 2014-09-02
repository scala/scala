import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  override def code = """
  /** This should be `monospaced` */
  class A
  """

  def scaladocSettings = ""

  def testModel(root: Package) = {
    import access._
    root._class("A").comment foreach println
  }
}
