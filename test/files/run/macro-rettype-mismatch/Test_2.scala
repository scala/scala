import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  import scala.reflect.mirror._
  val tree = Apply(Select(Ident("Macros"), newTermName("foo")), List(Literal(Constant(2))))

  val stderr = new java.io.ByteArrayOutputStream()
  Console.setErr(new java.io.PrintStream(stderr))

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  try { toolbox.runExpr(tree) }
  catch { case ex: Throwable => println(stderr); println(ex) }
}
