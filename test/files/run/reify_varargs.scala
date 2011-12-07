import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    val msg = java.text.MessageFormat.format(
      "On {1} there was {2} on planet {0}.",
      "Hoth", "the fifth of August", "a disturbance in the Force")
    println("Message="+msg)
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
