import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  override def code = """
  object Foo {
    /**
    * Demonstrates a scala issue in which the closing link tag is duplicated
    * <a href="https://link">title</a>
    */
    def bar = ???
  }
  """

  def scaladocSettings = ""

  def testModel(root: Package) = {
    import access._
    val thing = root._object("Foo")._method("bar")
    println(thing.comment.get.short)
  }
}
