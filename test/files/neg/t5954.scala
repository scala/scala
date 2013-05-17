// if you ever think you've fixed the underlying reason for the warning
// imposed by SI-5954, then here's a test that should pass with two "succes"es
//
//import scala.tools.partest._
//
//object Test extends DirectTest {
//  def code = ???
//
//  def problemCode = """
//    package object A {
//      class B
//      object B
//      case class C()
//    }
//  """
//
//  def compileProblemCode() = {
//    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
//    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(problemCode)
//  }
//
//  def show() : Unit = {
//    for (i <- 0 until 2) {
//      compileProblemCode()
//      println(s"success ${i + 1}")
//    }
//  }
//}

package object A {
  // these should be prevented by the implementation restriction
  class B
  object B
  trait C
  object C
  case class D()
  // all the rest of these should be ok
  class E
  object F
  val g = "omg"
  var h = "wtf"
  def i = "lol"
  type j = String
  class K(val k : Int) extends AnyVal
  implicit class L(val l : Int)
}
