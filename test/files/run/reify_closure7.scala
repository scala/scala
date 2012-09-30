import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.{currentMirror => cm}
import scala.tools.reflect.ToolBox

object Test extends App {
  var q = 0
  var clo: Int => Int = null
  def foo[T: TypeTag](ys: List[T]): Int => Int = {
    val z = 1
    var y = 0
    val fun = reify{(x: Int) => {
      y += 1
      q += 1
      println("q = " + q)
      println("y = " + y)
      x + ys.length * z + q + y
    }}

    if (clo == null) {
      val toolbox = cm.mkToolBox()
      val dyn = toolbox.eval(fun.tree)
      clo = dyn.asInstanceOf[Int => Int]
    }

    clo
  }

  val fun1 = foo(List(1, 2, 3))
  println("first invocation = " + fun1(10))
  val fun2 = foo(List(1, 2, 3, 4))
  println("second invocation = " + fun2(10))
}