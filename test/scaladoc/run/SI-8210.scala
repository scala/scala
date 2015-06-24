import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  override def code = """
object Foo {
  trait Config {
    /** The bar obviously. */
    def bar: Int
  }
  class ConfigBuilder extends Config {
    /** @inheritdoc
      *
      * The default value is 1234.
      */
    var bar: Int = 1234
  }
}
  """

  def scaladocSettings = ""

  def testModel(root: Package) = ()
}
