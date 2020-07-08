import scala.language._
import scala.tools.nsc.doc.base.comment.Text
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

/**
 * Reproducer for https://github.com/scala/bug/issues/11021
 * The @throws tag shouldn't cause an error when -implicits is enabled.
 */
object Test extends ScaladocModelTest {

  // test a file instead of a piece of code
  override def resourceFile = "implicits-throws-res.scala"

  // start implicits
  def scaladocSettings = "-implicits"

  def testModel(root: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val base = root._package("scala")._package("test")._package("scaladoc")._package("implicits")._package("throws")

    val A = base._class("A")

    val expectedComment = "exception"

    A.comment
      .flatMap(comment => comment.throws.get("java.lang.Exception"))
      .flatMap(body => body.summary) match {
      case Some(Text(actualComment)) =>
        assert(actualComment == expectedComment, s"Expected: $expectedComment, actual: $actualComment")
      case _ =>
        assert(false, s"Failed to match the comment")
    }
  }
}
