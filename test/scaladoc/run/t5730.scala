import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
      package scala.test.scaladoc.T5730

      /**
       *  A link:
       *
       * [[scala.Option$ object Option]].
       */
      sealed abstract class A

      case object B extends A

      abstract final class C
      """

  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val p = rootPackage._package("scala")._package("test")._package("scaladoc")._package("T5730")

    val a = p._class("A")
    val c = p._class("C")

    assert(a.constructors.isEmpty, s"there should be no constructors, found: ${a.constructors}")
    assert(c.constructors.isEmpty, s"there should be no constructors, found: ${c.constructors}")
  }
}
