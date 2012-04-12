object Macros {
  def foo1[T] = macro Impls.fooNullary[T]
  def foo2[T]() = macro Impls.fooEmpty[T]
  def bar1[T](x: Int) = macro Impls.barNullary[T]
  def bar2[T](x: Int)() = macro Impls.barEmpty[T]
}

object Test extends App {
  Macros.foo1[Int]
  Macros.foo2[Int]
  Macros.foo2[Int]()
  Macros.bar1[Int](42)
  Macros.bar2[Int](42)()
  println("kkthxbai")
}