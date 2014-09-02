import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
      package scala.test.scaladoc.T8857

      /**
       *  A link:
       *
       * [[scala.Option$ object Option]].
       */
      class A
    """

  // a non-canonical path to scala-library.jar should still work
  // this is a bit fragile (depends on the current directory being the root of the repo ;
  // ant & partest seem to do that properly)
  def scaladocSettings = "-doc-external-doc build/pack/bin/../lib/scala-library.jar#http://www.scala-lang.org/api/current/"

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val a = rootPackage._package("scala")._package("test")._package("scaladoc")._package("T8857")._class("A")

    val links = countLinks(a.comment.get, _.link.isInstanceOf[LinkToExternal])
    assert(links == 1, links + " ==  1 (the links to external in class A)")
  }
}
