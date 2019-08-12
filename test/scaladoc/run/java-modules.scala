import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
    /**
     * @see [[toBytes(i:java\.time\.Instant* ]]
     */
    class Foo
  """

  def scaladocSettings = ""

  def testModel(root: Package) = {
    import access._
    val foo = root._class("Foo")
    println(foo.comment.get.short)
  }
}
