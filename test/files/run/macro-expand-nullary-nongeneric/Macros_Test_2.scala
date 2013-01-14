object Macros {
  def foo1 = macro Impls.fooNullary
  def foo2() = macro Impls.fooEmpty
  def bar1(x: Int) = macro Impls.barNullary
  def bar2(x: Int)() = macro Impls.barEmpty
  def baz1(x: _) = macro Impls.bazNullary
  def baz2(x: _)() = macro Impls.bazEmpty
}

object Test extends App {
  Macros.foo1
  Macros.foo2
  Macros.foo2()
  Macros.bar1(42)
  Macros.bar2(42)()
  Macros.baz1(42)
  Macros.baz2(42)()
  println("kkthxbai")
}