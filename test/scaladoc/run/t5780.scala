import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.model.diagram._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
      package scala.test.scaladoc.SI5780

      object `package` { def foo: AnyRef = "hello"; class T /* so the package is not dropped */ }
    """

  // diagrams must be started. In case there's an error with dot, it should not report anything
  def scaladocSettings = "-doc-root-content " + resourcePath + "/doc-root"

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val foo = rootPackage._package("scala")._package("test")._package("scaladoc")._package("SI5780")._method("foo")
    // check that AnyRef is properly linked to its template:
    assert(foo.resultType.name == "AnyRef", foo.resultType.name + " == AnyRef")
    assert(foo.resultType.refEntity.size == 1, foo.resultType.refEntity + ".size == 1")
  }
}