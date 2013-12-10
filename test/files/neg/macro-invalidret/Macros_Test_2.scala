object Macros {
  def foo1 = macro Impls.foo1
  def foo2 = macro Impls.foo2
  def foo3 = macro Impls.foo3
  def foo4 = macro ???
  def foo5 = macro Impls.foo5
  def foo6 = macro Impls.foo6
}

object Test extends App {
  import Macros._
  foo1
  foo2
  foo3
  foo4
  foo5
  foo6
}