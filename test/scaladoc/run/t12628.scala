import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.base.comment._
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """

package org.foo

class `Foo-Bar`

object Foo {
  /**
    * This returns [[org.foo.`Foo-Bar`]]
    */
  def test(): `Foo-Bar` = ???
}
    """

  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    import access._

    val body = rootPackage._package("org")._package("foo")._object("Foo")._method("test").comment.get.body
    
    val links = countLinksInBody(body, _.link.isInstanceOf[LinkToTpl[_]])
    assert(links == 1, links + " ==  1 (links to Foo-Bar)")
  }
}
