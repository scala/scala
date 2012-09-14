import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  def foo[T: TypeTag](ys: List[T]): Int => Int = {
    val fun = reify{(x: Int) => {
      x + ys.length
    }}

    val toolbox = cm.mkToolBox()
    val dyn = toolbox.eval(fun.tree)
    dyn.asInstanceOf[Int => Int]
  }

  var fun1 = foo(List(1, 2, 3))
  println(fun1(10))
  var fun2 = foo(List(1, 2, 3, 4))
  println(fun2(10))
}