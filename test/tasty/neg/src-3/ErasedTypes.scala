package tastytest

import language.experimental.erasedDefinitions

object ErasedTypes {

  trait Foo {
    def foo1(erased x: String): Int
    def foo2(using erased x: String): Int
  }

  class Bar[F <: Foo { def foo1(erased x: String): 0 }]
  class Baz[F <: Foo { def foo2(using erased x: String): 0 }]

  object ErasedCompileTimeOps {
    erased def theNothing: Nothing = ???
  }

}
