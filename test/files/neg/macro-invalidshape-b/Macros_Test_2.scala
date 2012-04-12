object Macros {
  def foo(x: Any) = macro Impls.foo(null)(null)
}

object Test extends App {
  import Macros._
  foo(42)
}