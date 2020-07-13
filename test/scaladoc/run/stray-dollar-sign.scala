import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def resourceFile = "stray-dollar-sign-res.scala"

  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val comment = rootPackage._package("scaladoc")._package("resources")._object("O")._value("foo").comment
    assert(extractCommentText(comment.get) == "$a")
  }
}
