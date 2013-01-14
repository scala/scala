object Macros {
  def foo(xs: Int*) = macro Impls.foo
  def bar(xs: _*) = macro Impls.foo
}

object Test extends App {
  Macros.foo(1, 2, 3, 4, 5)
  Macros.bar(1, 2, 3, 4, 5)
}