class Macros {
  object Macros {
    def foo: Unit = macro Impls.foo
  }
}

object Test extends App {
  val outer = new Macros()
  outer.Macros.foo
}