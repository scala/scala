import language.experimental.macros

object Macros {
  type Foo = macro Impls.impl
  type FooFoo = macro Impls.implImpl
}

object Test extends App {
  import Macros._
  class D extends FooFoo
  val x: FooFoo = new D
}