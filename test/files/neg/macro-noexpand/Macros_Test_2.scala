object Macros {
  def foo(x: Any): Any = macro Impls.foo
}

object Test extends App {
  import Macros._
  foo(x)
}
