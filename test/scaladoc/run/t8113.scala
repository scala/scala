import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.base.comment._
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
  /**
   *  Check out [[http://www.scala-lang.org
   *  this great website]]!
   */
  class Test
  """

  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    import access._

    val test = rootPackage._class("Test")

    // find Link
    def find(body: Any): Option[Link] = body match {
        case l: Link => Some(l)
        case s: Seq[_]  => s.toList.map(find(_)).flatten.headOption
        case p: Product => p.productIterator.toList.map(find(_)).flatten.headOption
        case _          => None
      }

    val link = find(test.comment.get.body).collect { case Link(ta, Text(ti)) => (ta, ti) }
    assert(link.isDefined)
    val expected = ("http://www.scala-lang.org", "this great website")
    link.foreach {l => assert(l == expected, s"$l != $expected")}
  }
}
