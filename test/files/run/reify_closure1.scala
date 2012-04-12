import scala.reflect.mirror._

object Test extends App {
  def foo[T](ys: List[T]): Int => Int = {
    val fun = reify{(x: Int) => {
      x
    }}

    val toolbox = mkToolBox()
    val dyn = toolbox.runExpr(fun.tree)
    dyn.asInstanceOf[Int => Int]
  }

  println(foo(List(1, 2, 3))(10))
  println(foo(List(1, 2, 3, 4))(10))
}
