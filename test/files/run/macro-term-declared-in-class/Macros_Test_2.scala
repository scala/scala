class Macros {
  def foo: Unit = macro Impls.foo
}

object Test extends App {
  new Macros().foo
}