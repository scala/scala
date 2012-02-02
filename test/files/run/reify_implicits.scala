import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    implicit def arrayWrapper[A : ClassManifest](x: Array[A]) =
      new {
        def sort(p: (A, A) => Boolean) = {
          util.Sorting.stableSort(x, p); x
        }
      }
    val x = Array(2, 3, 1, 4)
    println("x = "+ x.sort((x: Int, y: Int) => x < y).toList)
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
