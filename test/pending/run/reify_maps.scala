import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    val colors = Map("red" -> 0xFF0000,
                     "turquoise" -> 0x00FFFF,
                     "black" -> 0x000000,
                     "orange" -> 0xFF8040,
                     "brown" -> 0x804000)
    for (name <- List("red", "green", "blue", "turquoise")) println(
      colors.get(name) match {
        case Some(code) =>
          name + " has code: " + code
        case None =>
          "Unknown color: " + name
      }
    )
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
