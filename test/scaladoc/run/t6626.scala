import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.base.comment._
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """

package org.foo

class MyException extends Exception

class MyOtherException extends Exception

object Foo {
  /**
    * Test exception linking
    *
    * @throws org.foo.MyException linked with a fully-qualified name
    * @throws MyOtherException linked with a relative name
    * @throws SomeUnknownException not linked at all (but with some text)
    * @throws IOException
    */
  def test(): Unit = ???
}
    """

  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val a = rootPackage._package("org")._package("foo")._object("Foo")._method("test")
    val throws = a.comment.get.throws
    val allbodies = Body(throws.values.flatMap(_.blocks).toSeq)

    val links = countLinksInBody(allbodies, _.link.isInstanceOf[LinkToTpl[_]])
    assert(links == 2, links + " ==  2 (links to MyException and MyOtherException)")
  }
}
