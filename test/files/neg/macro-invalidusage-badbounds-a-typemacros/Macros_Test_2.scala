object Macros {
  type Foo[U <: String] = macro Impls.foo[U]
}

object Test extends App {
  import Macros._
  class D extends Foo[Int]
  val x: Foo[Int] = new C
}