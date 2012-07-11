import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.model.diagram._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
        package scala.test.scaladoc.variable.expansion {
          /**
           * Blah blah blah
           */
          class A

          object A {
            import language.implicitConversions
            implicit def aToB(a: A) = new B
          }

          /**
           * @define coll collection
           */
          class B {
            /**
             * foo returns a $coll
             */
            def foo: Nothing = ???
          }
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
  }
}