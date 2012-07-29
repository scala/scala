import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.model.diagram._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
        package scala.test.scaladoc.variable.expansion {
          /** @define coll WROOOONG-A */
          class A

          object A {
            import language.implicitConversions
            implicit def aToC(a: A) = new C
            implicit def aToE(a: A) = new E with F
          }

          /** @define coll WROOOONG-B */
          class B {
            /** foo returns a $coll */
            def foo: Nothing = ???
          }

          /** @define coll collection */
          class C extends B

          /** @define coll WROOOONG-D */
          trait D {
            /** bar returns a $coll */
            def bar: Nothing = ???
          }

          /** @define coll result */
          //trait E { self: D => override def bar: Nothing = ??? }
          trait E extends D { override def bar: Nothing = ??? }

          /** @define coll WROOOONG-F */
          trait F
        }
    """

  // diagrams must be started. In case there's an error with dot, it should not report anything
  def scaladocSettings = "-implicits"

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val base = rootPackage._package("scala")._package("test")._package("scaladoc")._package("variable")._package("expansion")
    val foo = base._class("A")._method("foo")
    assert(foo.comment.get.body.toString.contains("foo returns a collection"), "\"" + foo.comment.get.body.toString + "\".contains(\"foo returns a collection\")")

    val bar = base._class("A")._method("bar")
    assert(bar.comment.get.body.toString.contains("bar returns a result"), "\"" + bar.comment.get.body.toString + "\".contains(\"bar returns a result\")")
  }
}