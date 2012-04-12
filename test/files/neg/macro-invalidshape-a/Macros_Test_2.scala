object Macros {
  def foo(x: Any) = macro 2
}

object Test extends App {
  import Macros._
  foo(42)
}