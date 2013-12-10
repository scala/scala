object Macros {
  def foo(xs: Int*): Unit = macro Impls.foo
}

object Test extends App {
  val numbers = List(1, 2, 3, 4, 5)
  Macros.foo(numbers: _*)
}