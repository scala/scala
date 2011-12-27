import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  def foo(y: Int): Int => Int = {
    class Foo(y: Int) {
      val fun: reflect.Code[Int => Int] = x => {
        x + y
      }
    }

    val reporter = new ConsoleReporter(new Settings)
    val toolbox = new ToolBox(reporter)
    val ttree = toolbox.typeCheck(new Foo(y).fun.tree)
    val dyn = toolbox.runExpr(ttree)
    dyn.asInstanceOf[Int => Int]
  }

  println(foo(1)(10))
  println(foo(2)(10))
}
