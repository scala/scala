package tastytest

object HKNest {

  class ConsumeInScala3[H[_[_]]] {
    def foo[F[A]](x: H[F]): String = x.toString()
  }

  class Box[A]

  class Foo[F[_]] {
    override def toString(): String = "Foo"
  }

}
