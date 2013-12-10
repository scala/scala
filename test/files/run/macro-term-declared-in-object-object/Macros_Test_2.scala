object Macros {
  object Macros {
    def foo: Unit = macro Impls.foo
  }
}

object Test extends App {
  val outer = Macros
  outer.Macros.foo
}