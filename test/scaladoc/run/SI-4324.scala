import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
        case class Test4324(arg11: String, arg12: Int)(arg21: String, arg22: Int)(arg31: Int, arg32: String)
    """

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    // just need to check the member exists, access methods will throw an error if there's a problem
    rootPackage._class("Test4324").asInstanceOf[Class].valueParams match {
      case List(List(arg11, arg12), List(arg21, arg22), List(arg31, arg32)) => //yeeey, do nothing
      case other =>
        assert(false, "Incorrect valueParams generated: " + other + " instead of (arg11, arg12)(arg21, arg22)(arg31, arg32)")
    }
  }
}