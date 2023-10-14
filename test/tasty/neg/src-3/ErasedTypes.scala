package tastytest

import language.experimental.erasedDefinitions

import scala.annotation.experimental

object ErasedTypes {

  @experimental
  trait Foo {
    def foo1(erased x: String): Int
    def foo2(using erased x: String): Int
  }

  @experimental
  class Bar[F <: Foo { def foo1(erased x: String): 0 }]

  @experimental
  class Baz[F <: Foo { def foo2(using erased x: String): 0 }]

  object ErasedCompileTimeOps {
    @experimental
    erased def theString: String = compiletime.erasedValue
  }

}
