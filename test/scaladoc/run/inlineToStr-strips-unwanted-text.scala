import scala.tools.nsc.doc.html.Page
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
  /** This comment contains ^superscript^ */
  class Foo {
    /** This comment contains ,,subscript,, */
    def bar = ???

    /** This comment contains a link [[https://scala.epfl.ch/]] */
    def baz = ???

    /** This comment contains an <strong>html tag</strong> */
    def qux = ???

    /** This comment contains a<br> single html tag */
    def quux = ???

    /** This comment contains nested <strong>html<br> tags</strong> */
    def quuz = ???
  }
  """
  def scaladocSettings = ""

  def testModel(root: Package) = {
    import scala.tools.nsc.doc.base.comment._
    import access._

    val foo = root._class("Foo")
    val bar = foo._method("bar")
    val baz = foo._method("baz")
    val qux = foo._method("qux")
    val quux = foo._method("quux")
    val quuz = foo._method("quuz")
    println(foo.comment.get.short)
    println(bar.comment.get.short)
    println(baz.comment.get.short)
    println(qux.comment.get.short)
    println(quux.comment.get.short)
    println(quuz.comment.get.short)
    println(Page.inlineToStr(foo.comment.get.short))
    println(Page.inlineToStr(bar.comment.get.short))
    println(Page.inlineToStr(baz.comment.get.short))
    println(Page.inlineToStr(qux.comment.get.short))
    println(Page.inlineToStr(quux.comment.get.short))
    println(Page.inlineToStr(quuz.comment.get.short))
  }
}
