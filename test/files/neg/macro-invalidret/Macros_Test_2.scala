//> using options -Xlint:deprecation -Werror
import language.experimental.macros

object Macros {
  // result type required
  def foo1 = macro Impls.foo1
  def foo2 = macro Impls.foo2
  def foo3 = macro Impls.foo3
  def foo4 = macro ???
  def foo5 = macro Impls.foo5
  def foo6 = macro Impls.foo6

  // various flawed attempts to implement
  def bar1: Int = macro Impls.foo1
  def bar2: Int = macro Impls.foo2
  def bar3: Int = macro Impls.foo3
  def bar4: Int = macro ???
  def bar5: Int = macro Impls.foo5
  def bar6: Int = macro Impls.foo6
}

object Test extends App {
  import Macros._
  foo1
  foo2
  foo3
  foo4
  foo5
  foo6

  bar1
  bar2
  bar3
  bar4
  bar5
  bar6
}
