object Test extends App {
  def foo[T](ys: List[T]) = {
    val fun: reflect.Code[Int => Int] = x => x + ys.length
    fun
  }
  println(foo(List(2)).tree)
}
