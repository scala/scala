import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    2
  };

  val settings = new Settings
  settings.Xprint.value = List("typer")

  val reporter = new ConsoleReporter(settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  println("result = " + toolbox.showAttributed(ttree))

  val evaluated = toolbox.runExpr(ttree)
  println("evaluated = " + evaluated)
}
