import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  // will fail because y is a private field
  // reification doesn't magically make unavailable stuff available
  class Foo(y: Int) {
    def fun = reify{y}
  }

  try {
    val dyn = cm.mkToolBox().eval(new Foo(10).fun.tree)
    val foo = dyn.asInstanceOf[Int]
    println(foo)
  } catch {
    case ex: Throwable =>
      println(ex)
  }
}