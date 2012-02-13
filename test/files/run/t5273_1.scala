import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    List(1, 2, 3) match {
      case foo :: bar :: _ => println(foo * bar)
      case _ => println("this is getting out of hand!")
    }
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
