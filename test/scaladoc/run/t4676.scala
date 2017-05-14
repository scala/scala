import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.model.diagram._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
        class SI_4676 {
          type SS = (String,String)
          def x(ss: SS): Int = 3
        }
        class cbf[A, B, C]
    """

  // diagrams must be started. In case there's an error with dot, it should not report anything
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    // check correct expansion of the use case signature
    val x = rootPackage._class("SI_4676")._method("x")
    val resultType = x.valueParams(0)(0).resultType.name
    assert(resultType == "SS", s"parameter ss of method x has type $resultType, expected SS!")
  }
}