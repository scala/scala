object Macros {
  def foo1: Unit = macro Impls.fooNullary
  def foo2(): Unit = macro Impls.fooEmpty
  def bar1(x: Int): Unit = macro Impls.barNullary
  def bar2(x: Int)(): Unit = macro Impls.barEmpty
}

object Test extends App {
  Macros.foo1
  Macros.foo2
  Macros.foo2()
  Macros.bar1(42)
  Macros.bar2(42)()
  println("kkthxbai")
}