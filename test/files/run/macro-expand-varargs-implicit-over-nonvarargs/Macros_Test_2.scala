object Macros {
  def foo(xs: Int*): Unit = macro Impls.foo
}

object Test extends App {
  Macros.foo(1, 2, 3, 4, 5)
}