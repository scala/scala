import scala.tools.partest._
import scala.tools.nsc._

object Test extends DirectTest {
  override def code = "class C"
  override def newCompiler(args: String*): Global = {
    val settings = newSettings(tokenize("-d doesnotexist " + extraSettings) ++ args.toList)
    newCompiler(settings)
  }

  override def show(): Unit = {
    try {
      newCompiler()
    } catch {
      case fe: FatalError => println(fe.getMessage)
    }
  }
}
