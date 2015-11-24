import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

// SI-5079 "Scaladoc can't link to an object (only a class or trait)"
// SI-4497 "Links in Scaladoc - Spec and implementation unsufficient"
// SI-4224 "Wiki-links should support method targets"
// SI-3695 "support non-fully-qualified type links in scaladoc comments"
// SI-6487 "Scaladoc can't link to inner classes"
// SI-6495 "Scaladoc won't pick up group name, priority and description from owner chain"
// SI-6501 "Scaladoc won't link to a @template type T as a template but as a member"
object Test extends ScaladocModelTest {

  override def resourceFile = "links.scala"

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    // just need to check the member exists, access methods will throw an error if there's a problem
    val base = rootPackage._package("scala")._package("test")._package("scaladoc")._package("links")
    val TEST = base._object("TEST")

    val memberLinks = countLinks(TEST.comment.get, _.link.isInstanceOf[LinkToMember[_, _]])
    val templateLinks = countLinks(TEST.comment.get, _.link.isInstanceOf[LinkToTpl[_]])
    assert(memberLinks == 18,  memberLinks +   " == 18 (the member links in object TEST)")
    assert(templateLinks == 6, templateLinks + " ==  6 (the template links in object TEST)")
  }
}
