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

    /** This comment contains a [[corge ,,link with a subscript title,,]] */
    def corge = ???
  }
  """
  def scaladocSettings = ""

  def testModel(root: Package) = {
    import scala.tools.nsc.doc.base.comment._
    import access._

    val foo = root._class("Foo")

    val fooStr = Page.inlineToStr(foo.comment.get.short)
    assert(fooStr == "This comment contains superscript", fooStr)

    val barStr = Page.inlineToStr(foo._method("bar").comment.get.short)
    assert(barStr == "This comment contains subscript", barStr)

    val bazStr = Page.inlineToStr(foo._method("baz").comment.get.short)
    assert(bazStr == "This comment contains a link https://scala.epfl.ch/", bazStr)

    val quxStr = Page.inlineToStr(foo._method("qux").comment.get.short)
    assert(quxStr == "This comment contains an html tag", quxStr)

    val quuxStr = Page.inlineToStr(foo._method("quux").comment.get.short)
    assert(quuxStr == "This comment contains a single html tag", quuxStr)

    val quuzStr = Page.inlineToStr(foo._method("quuz").comment.get.short)
    assert(quuzStr == "This comment contains nested html tags", quuzStr)

    val corgeStr = Page.inlineToStr(foo._method("corge").comment.get.short)
    assert(corgeStr == "This comment contains a link with a subscript title", corgeStr)
  }
}
