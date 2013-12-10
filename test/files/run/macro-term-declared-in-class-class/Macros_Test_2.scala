class Macros {
  class Macros {
    def foo: Unit = macro Impls.foo
  }
}

object Test extends App {
  val outer = new Macros()
  new outer.Macros().foo
}