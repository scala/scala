object Macros {
  def foo(x: String) = macro Impls.fooObjectString
  def foo(x: Int) = macro Impls.fooObjectInt
  def foo(x: Boolean) = println("fooObjectBoolean")
}

class Macros {
  def foo(x: String) = macro Impls.fooClassString
  def foo(x: Int) = macro Impls.fooClassInt
  def foo(x: Boolean) = println("fooClassBoolean")
}

object Test extends App {
  Macros.foo("42")
  Macros.foo(42)
  Macros.foo(true)
  new Macros().foo("42")
  new Macros().foo(42)
  new Macros().foo(true)
}