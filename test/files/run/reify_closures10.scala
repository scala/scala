import scala.reflect.Code._
import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val x = 2
  val y = 3
  val code = lift{println(x + y); x + y}

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  println(toolbox.runExpr(code.tree))
}
