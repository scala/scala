class BoundChild extends Bound

object Macros {
  type Foo[U <: BoundChild] = macro Impls.foo[U]
}

object Test extends App {
  import Macros._
  class D extends Foo[BoundChild]
  val x: Foo[BoundChild] = new C
}