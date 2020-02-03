package tastytest

import HKNest._

object TestHKNest extends Suite("TestHKNest") {
    class Consume[H[_[_]]] {
        def foo[F[A]](x: H[F]): String = x.toString()
    }

    test(assert(new Consume[Foo].foo(new Foo[Box]) == "Foo"))
}
