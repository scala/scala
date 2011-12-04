import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    def x = 2
    println(x)
  };

  val settings = new Settings
  settings.debug.value = true
  settings.Xshowtrees.value = true
  settings.Xprint.value = List("typer")
  settings.printtypes.value = true
  settings.Ytyperdebug.value = true

  val reporter = new ConsoleReporter(settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  val evaluated = toolbox.runExpr(ttree)
  println("evaluated = " + evaluated)
}