import scala.reflect.mirror._

object Test extends App {
  def foo(y: Int): Int => Int = {
    val y1 = y

    val fun = reify{(x: Int) => {
      x + y1
    }}

    val toolbox = mkToolBox()
    val dyn = toolbox.runExpr(fun.tree)
    dyn.asInstanceOf[Int => Int]
  }

  println(foo(1)(10))
  println(foo(2)(10))
}
