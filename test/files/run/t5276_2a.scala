import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    class C {
      lazy val x = 2
    }

    println(new C().x)
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  toolbox.runExpr(code.tree)
}
