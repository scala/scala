import scala.tools.partest._
import scala.tools.nsc._
import scala.tools.cmd.CommandLineParser

object Test extends DirectTest {
  override def code = "class C"
  override def newCompiler(args: String*): Global = {
    val settings = newSettings((CommandLineParser tokenize ("-d doesnotexist " + extraSettings)) ++ args.toList)
    newCompiler(settings)
  }

  override def show() {
    try {
      newCompiler()
    } catch {
      case fe: FatalError => println(fe.getMessage)
    }
  }
}
