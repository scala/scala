package tastytest

object HKNest {

  class HKClass_1[H[_[_]]] {
    def foo[F[X]](x: H[F]): String = x.toString()
  }

  class HKClass_2[G[_[_], _]] {
    def foo[F[X], A](x: G[F, A]): String = x.toString()
  }

  class HKClass_3[K[F[X] <: List[X]]] {
    def foo[F[X] <: List[X]](x: K[F]): String = x.toString()
  }

  class HKClass_4[L[+F[X] <: QuxArg[X]]] {
    def foo[F[X] <: QuxArg[X]](x: L[F]): String = x.toString()
  }

  class HKClass_5[M[-F[X] >: Arg1[X]]] {
    def foo[F1[X] >: Arg1[X]](x: M[F1]): String = x.toString()
  }

  class HKClass_6[N[-F[X] >: QuxArg[X]]] {
    def foo[F1[X] >: QuxArg[X]](x: N[F1]): String = x.toString()
  }

  class HKClass_7[O[F[X] <: Either[X, Int]]] {
    def foo[F[X] <: Either[X, Int]](x: O[F]): String = x.toString()
  }

  type OrInt[X] = Either[X, Int]

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

  class Quux[-F[X] >: Arg1[X]] {
    override def toString(): String = "Quux"
  }

  class Boo[F[X] <: Either[X, Int]] {
    override def toString(): String = "Boo"
  }

}
