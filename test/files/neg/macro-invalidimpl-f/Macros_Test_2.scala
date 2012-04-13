object Macros {
  def bar1() = macro Impls.fooNullary
}

object Test extends App {
  Macros.bar1
  Macros.bar1()
  println("kkthxbai")
}