package tastytest

import HKNest._

object TestHKNest extends Suite("TestHKNest") {

  class ConsumeInScala2[H[_[_]]] {
    def foo[F[X]](x: H[F]): String = x.toString()
  }

  test(assert(new ConsumeInScala2[Foo].foo(new Foo[Box]) == "Foo"))
  test(assert(new ConsumeInScala3[Foo].foo(new Foo[Box]) == "Foo"))
  test(assert(new ConsumeInScala3_2[Bar].foo(new Bar[Box, String]) == "Bar"))
  test(assert(new ConsumeInScala3_3[Baz].foo(new Baz[List]) == "Baz"))
  test(assert(new ConsumeInScala3_4[Qux].foo(new Qux[QuxArg]) == "Qux"))
  test(assert(new ConsumeInScala3_4[Qux].foo(new Qux[Arg1]) == "Qux"))
}
