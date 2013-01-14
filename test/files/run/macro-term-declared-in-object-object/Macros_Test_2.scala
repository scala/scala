object Macros {
  object Macros {
    def foo = macro Impls.foo
  }
}

object Test extends App {
  val outer = Macros
  outer.Macros.foo
}