object Macros {
  type Foo[T](x: T) = macro Impls.impl[T]
  type FooFoo = macro Impls.implImpl
}

object Test extends App {
  import Macros._
  class D extends FooFoo
  println(new D().tpe)
}