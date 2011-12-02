import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    class C {
      val x = 2
    }

    println(new C().x)
  };

  val settings = new Settings
  settings.Yreifydebug.value = true
  settings.debug.value = true
  settings.nospecialization.value = true

  val reporter = new ConsoleReporter(settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  val evaluated = toolbox.runExpr(ttree)
  println("evaluated = " + evaluated)
}
