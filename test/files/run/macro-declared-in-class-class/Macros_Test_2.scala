class Macros {
  class Macros {
    def foo = macro Impls.foo
  }
}

object Test extends App {
  val outer = new Macros()
  new outer.Macros().foo
}