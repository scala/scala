object Macros {
  class Macros {
    def foo = macro Impls.foo
  }
}

object Test extends App {
  val outer = Macros
  new outer.Macros().foo
}