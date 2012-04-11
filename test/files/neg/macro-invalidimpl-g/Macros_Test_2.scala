object Macros {
  def foo1 = macro Impls.fooEmpty
}

object Test extends App {
  Macros.foo1
  println("kkthxbai")
}