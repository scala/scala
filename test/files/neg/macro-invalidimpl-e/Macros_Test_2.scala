object Macros {
  def foo(x: Any) = macro Impls.foo
  def foo(x: Any, y: Any) = macro Impls.foo
}

object Test extends App {
  import Macros._
  foo(42)
}