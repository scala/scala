object Macros {
  def foo[U <: String] = macro Impls.foo[U]
}

object Test extends App {
  import Macros._
  foo[Int]
}