import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  def foo[T](ys: List[T]): Int => Int = {
    class Foo[T](ys: List[T]) {
      val fun = reify{(x: Int) => {
        x + ys.length
      }}
    }

    val toolbox = cm.mkToolBox()
    val dyn = toolbox.eval(new Foo(ys).fun.tree)
    dyn.asInstanceOf[Int => Int]
  }

  println(foo(List(1, 2, 3))(10))
  println(foo(List(1, 2, 3, 4))(10))
}