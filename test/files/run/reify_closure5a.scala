import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  def foo[T](ys: List[T]): Int => Int = {
    val fun: reflect.Code[Int => Int] = x => {
      x + ys.length
    }

    val reporter = new ConsoleReporter(new Settings)
    val toolbox = new ToolBox(reporter)
    val ttree = toolbox.typeCheck(fun.tree)
    val dyn = toolbox.runExpr(ttree)
    dyn.asInstanceOf[Int => Int]
  }

  println(foo(List(1, 2, 3))(10))
  println(foo(List(1, 2, 3, 4))(10))
}
