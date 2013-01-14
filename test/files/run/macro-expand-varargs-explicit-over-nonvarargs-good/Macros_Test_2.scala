object Macros {
  def foo(xs: Int*) = macro Impls.foo
  def bar(xs: _*) = macro Impls.foo
}

object Test extends App {
  val numbers = List(1, 2, 3, 4, 5)
  Macros.foo(numbers: _*)
  Macros.bar(numbers: _*)
}