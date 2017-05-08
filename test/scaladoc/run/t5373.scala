import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
        import scala.annotation.bridge

        package scala.test {

          trait A {
            def foo = ()
          }

          trait B extends A {
            @bridge()
            def foo = ()
          }

          class C extends B
        }
    """

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    // just need to check the member exists, access methods will throw an error if there's a problem
    rootPackage._package("scala")._package("test")._class("C")._method("foo")
  }
}