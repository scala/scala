object Macros {
  def foo = macro Impls.foo
}

object Test extends App {
  Macros.foo
}