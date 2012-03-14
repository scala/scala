import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  def foo[T](ys: List[T]): Int => Int = {
    class Foo[T](ys: List[T]) {
      val fun = reflect.Code.lift{(x: Int) => {
        x + ys.length
      }}
    }

    val reporter = new ConsoleReporter(new Settings)
    val toolbox = new ToolBox(reporter)
    val dyn = toolbox.runExpr(new Foo(ys).fun.tree)
    dyn.asInstanceOf[Int => Int]
  }

  println(foo(List(1, 2, 3))(10))
  println(foo(List(1, 2, 3, 4))(10))
}
