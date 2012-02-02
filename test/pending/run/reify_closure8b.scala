import scala.reflect.Code._
import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  class Foo(y: Int) {
    def fun = lift{y}
  }

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(new Foo(10).fun.tree)
  val dyn = toolbox.runExpr(ttree)
  val foo = dyn.asInstanceOf[Int]
  println(foo)
}
