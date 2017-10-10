import scala.tools.nsc.doc.html.Page
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
   /** This is a multi-line comment
     *  <strong>containing</strong> html tags and
     *    extra white space between lines.
     */
     class Foo {
  }
  """
  def scaladocSettings = ""

  def testModel(root: Package) = {
    import scala.tools.nsc.doc.base.comment._
    import access._

    val foo = root._class("Foo")

    val fooStr = Page.inlineToStrForTitleTag(foo.comment.get.short)
    assert(fooStr == "This is a multi-line comment containing html tags and extra white space between lines.", fooStr)
  }
}
