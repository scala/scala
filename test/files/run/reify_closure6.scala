import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  var q = 0
  def foo[T](ys: List[T]): Int => Int = {
    val z = 1
    var y = 0
    val fun = reflect.Code.lift{(x: Int) => {
      y += 1
      q += 1
      println("q = " + q)
      println("y = " + y)
      x + ys.length * z + q + y
    }}

    val reporter = new ConsoleReporter(new Settings)
    val toolbox = new ToolBox(reporter)
    val dyn = toolbox.runExpr(fun.tree)
    dyn.asInstanceOf[Int => Int]
  }

  println("first invocation = " + foo(List(1, 2, 3))(10))
  println("second invocation = " + foo(List(1, 2, 3, 4))(10))
  println("q after second invocation = " + q)
}