import scala.reflect.mirror._

object Test extends App {
  def foo[T: TypeTag](ys: List[T]): Int => Int = {
    val fun = reify{(x: Int) => {
      x + ys.length
    }}

    val toolbox = mkToolBox()
    val dyn = toolbox.runExpr(fun.tree)
    dyn.asInstanceOf[Int => Int]
  }

  var fun1 = foo(List(1, 2, 3))
  println(fun1(10))
  var fun2 = foo(List(1, 2, 3, 4))
  println(fun2(10))
}
