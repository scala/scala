object Macros {
  def foo = macro Impls.foo
  def fooFoo = macro Impls.fooFoo
}

object Test extends App {
  Macros.fooFoo
}