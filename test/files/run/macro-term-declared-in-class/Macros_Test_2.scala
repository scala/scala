class Macros {
  def foo = macro Impls.foo
}

object Test extends App {
  new Macros().foo
}