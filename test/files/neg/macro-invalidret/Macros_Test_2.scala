object Macros {
  def foo1 = macro Impls.foo1
  def foo2 = macro Impls.foo2
}

object Test extends App {
  import Macros._
  foo1
  foo2
}