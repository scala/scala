object Macros {
  type Foo[U <: String] = macro Impls.foo[U]
}

object Test extends App {
  import Macros._
  class D extends Foo[String]
  val x: Foo[String] = new C
}