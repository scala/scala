import Impls._

object Macros {
  object Shmacros {
    def foo(x: Int): Int = macro foo_impl
  }
  def bar(x: Int): Int = macro bar_impl
}

class Macros {
  def quux(x: Int): Int = macro quux_impl
}