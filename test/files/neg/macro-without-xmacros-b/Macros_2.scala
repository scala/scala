object Macros {
  object Shmacros {
    def foo(x: Int): Int = macro Impls.foo_impl
  }
  def bar(x: Int): Int = macro Impls.bar_impl
}

class Macros {
  def quux(x: Int): Int = macro Impls.quux_impl
}