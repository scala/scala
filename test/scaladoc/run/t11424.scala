import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = s"""
  /**
    *  Some scaladoc that contains string literal of HTML
    *  {{{
    *  val button = "<button>My Button</button>"
    *  }}}
    */
    object Foo
  """

  def scaladocSettings = ""

  def testModel(root: Package) = {
    import access._
    val obj = root._object("Foo")
    val safeTagMarker = '\u000E'
    obj.comment.get.body.blocks.foreach({
      case block: scala.tools.nsc.doc.base.comment.Code => assert(!block.data.exists(_ == safeTagMarker), s"there should be no safeTagMarkers left in the String, found atleast one $safeTagMarker in the string: ${block.data}")
      case _ => // do nothing
    })
  }
}