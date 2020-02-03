package tastytest

import HKNest._

object TestHKNest extends Suite("TestHKNest") {

  class ConsumeInScala2[H[_[_]]] {
    def foo[F[A]](x: H[F]): String = x.toString()
  }

  test(assert(new ConsumeInScala2[Foo].foo(new Foo[Box]) == "Foo"))
//   test(assert(new ConsumeInScala3[Foo].foo(new Foo[Box]) == "Foo")) // broken
}
