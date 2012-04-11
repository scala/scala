object Macros {
  def foo1 = macro Impls.fooNullary
  def foo2() = macro Impls.fooEmpty
  def bar1(x: Int) = macro Impls.barNullary
  def bar2(x: Int)() = macro Impls.barEmpty
}

object Test extends App {
  Macros.foo1
  Macros.foo2
  Macros.foo2()
  Macros.bar1(42)
  Macros.bar2(42)()
  println("kkthxbai")
}