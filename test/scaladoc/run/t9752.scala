import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = s"""
  /**
    * Foo
    *
    * @example
    * {{{
    * class A
    *
    *
    * class B
    * }}}
    */
    object Foo
  """

  def scaladocSettings = ""

  def testModel(root: Package) = {
    import access._
    val obj = root._object("Foo")
    println(obj.comment.get.example)
  }
}
