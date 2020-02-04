package tastytest

object HKNest {

  class ConsumeInScala3[H[_[_]]] {
    def foo[F[X]](x: H[F]): String = x.toString()
  }

  class ConsumeInScala3_2[G[_[_], _]] {
    def foo[F[X], A](x: G[F, A]): String = x.toString()
  }

  class ConsumeInScala3_3[K[F[X] <: List[X]]] {
    def foo[F[X] <: List[X]](x: K[F]): String = x.toString()
  }

  class ConsumeInScala3_4[L[F[X] <: QuxArg[X]]] {
    def foo[F[X] <: QuxArg[X]](x: L[F]): String = x.toString()
  }

  class Box[A]

  class Foo[F[_]] {
    override def toString(): String = "Foo"
  }

  class Bar[F[_], A] {
    override def toString(): String = "Bar"
  }

  class Baz[F[X] <: List[X]] {
    override def toString(): String = "Baz"
  }

  sealed trait QuxArg[T]
  case class Arg1[T]() extends QuxArg[T]

  class Qux[+F[X] <: QuxArg[X]] {
    override def toString(): String = "Qux"
  }

}
