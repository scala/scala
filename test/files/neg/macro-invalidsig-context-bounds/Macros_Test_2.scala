object Macros {
  def foo[U] = macro Impls.foo[U]
}

object Test extends App {
  import Macros._
  println(foo[String])
}