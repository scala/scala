object Macros {
  object Shmacros {
    def foo(x: Int): Int = macro Impls.foo
  }
  def bar(x: Int): Int = macro Impls.bar
}

class Macros {
  def quux(x: Int): Int = macro Impls.quux
}

object Test extends App {
  import Macros.Shmacros._
  println(foo(2) + Macros.bar(2) * new Macros().quux(4))
}