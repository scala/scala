object Macros {
  def foo: Unit = macro Impls.foo
}

object Test extends App {
  Macros.foo
}