import language.experimental.macros

object Macros {
  type Foo(x: Int) = macro Impls.impl
  type FooFoo = macro Impls.implImpl
}

object Test extends App {
  import Macros._
  class D extends FooFoo
  // Macros_Test_2.scala:11: error: type macro can expand into an Apply tree only in parent type position
  // val x: FooFoo = new D
}