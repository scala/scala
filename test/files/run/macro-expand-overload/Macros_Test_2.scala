object Macros {
  def foo(x: String): Unit = macro Impls.fooObjectString
  def foo(x: Int): Unit = macro Impls.fooObjectInt
  def foo(x: Boolean): Unit = println("fooObjectBoolean")
}

class Macros {
  def foo(x: String): Unit = macro Impls.fooClassString
  def foo(x: Int): Unit = macro Impls.fooClassInt
  def foo(x: Boolean): Unit = println("fooClassBoolean")
}

object Test extends App {
  Macros.foo("42")
  Macros.foo(42)
  Macros.foo(true)
  new Macros().foo("42")
  new Macros().foo(42)
  new Macros().foo(true)
}