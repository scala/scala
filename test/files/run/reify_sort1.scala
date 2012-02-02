import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    def sort(a: List[Int]): List[Int] = {
      if (a.length < 2)
        a
      else {
        val pivot = a(a.length / 2)
        sort(a.filter(_ < pivot)) :::
             a.filter(_ == pivot) :::
             sort(a.filter(_ > pivot))
      }
    }

    val xs = List(6, 2, 8, 5, 1)
    println(xs)
    println(sort(xs))
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
