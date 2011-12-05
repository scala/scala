import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    val RegexParser = """(.*) \d+([A-Z]+) \| (.*) \|.*""".r
    val RegexParser(name, shortname, value) = "American Dollar 1USD | 2,8567 | sometext"
    println("name = %s, shortname = %s, value = %s".format(name, shortname, value))
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
