object Macros {
  type Foo[T] = macro Impls.foo
  type Bar[T](x: Int) = macro Impls.bar
}

object Test extends App {
  import Macros._

  class D1 extends Foo
  val x1: Foo = new C

  class D2 extends Bar(2)
  val x2: Bar(2) = new C
}