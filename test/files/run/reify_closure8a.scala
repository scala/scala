import scala.reflect.Code._
import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  class Foo(val y: Int) {
    def fun = lift{y}
  }

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val dyn = toolbox.runExpr(new Foo(10).fun.tree)
  val foo = dyn.asInstanceOf[Int]
  println(foo)
}
