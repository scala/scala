import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    val sumOfSquares1 = (for (i <- 1 to 100; if (i % 3 == 0)) yield Math.pow(i, 2)).sum
    val sumOfSquares2 = (1 to 100).filter(_ % 3 == 0).map(Math.pow(_, 2)).sum
    assert(sumOfSquares1 == sumOfSquares2)
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
