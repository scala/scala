object Test extends App {
  def foo[T](ys: List[T]): Int => Int = {
    class Foo[T](ys: List[T]) {
      val fun = reflect.mirror.reify{(x: Int) => {
        x + ys.length
      }}
    }

    val toolbox = mkToolBox()
    val dyn = toolbox.runExpr(new Foo(ys).fun.tree)
    dyn.asInstanceOf[Int => Int]
  }

  println(foo(List(1, 2, 3))(10))
  println(foo(List(1, 2, 3, 4))(10))
}
