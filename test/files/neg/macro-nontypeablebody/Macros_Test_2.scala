object Macros {
  def foo(x: Any) = macro Impls.foo2
}

object Test extends App {
  import Macros._
  foo(42)
}