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

  class HKClass_7[M[-F[X] >: Arg1[X] <: QuxArg[X]]] {
    def foo[F[X] >: Arg1[X] <: QuxArg[X]](x: M[F]): String = x.toString()
  }

  class HKClass_8[M[F >: [X] =>> Arg1[X] <: [Y] =>> QuxArg[Y]]] {
    def foo[F[X] >: Arg1[X] <: QuxArg[X]](x: M[F]): String = x.toString()
  }

  class HKClass_9[O[F[X] <: Either[X, Int]]] {
    def foo[F[X] <: Either[X, Int]](x: O[F]): String = x.toString()
  }

  class HKClass_10[F[G[X]] <: Foo[[T] =>> Either[Nothing, T]]] {
    def foo[G[X]](x: F[G]): String = x.toString()
  }

  class HKClass_11[F[G[X]] >: Foo[[T] =>> Either[Nothing, T]]] {
    def foo[G[X]](x: F[G]): String = x.toString()
  }

  class HKClass_12[F[-T >: Sum]] {
    def foo[T1 >: Sum](x: F[T1]): String = x.toString()
  }

  class HKClass_13[F[-T >: Prod]] {
    def foo[T1 >: Prod](x: F[T1]): String = x.toString()
  }

  def test8 = new HKClass_8[Quuux].foo(new Quuux[QuxArg])

  type ThrowawayHK[G[X]] = Foo[[T] =>> Either[Nothing, T]]

  // class HKClass_8[P[F[X <: String]] <: Hoo[StringOrInt]] {
  //   def foo[F[X <: String]](x: P[F]): String = x.toString() https://github.com/lampepfl/dotty/issues/8329
  // }

  type OrInt[X] = Either[X, Int]
  // type StringOrInt[X <: String] = Either[X, Int] https://github.com/lampepfl/dotty/issues/8329

  class Box[A]

  class Foo[F[_]] {
    override def toString(): String = "Foo"
  }

  class Foo1[F[T] <: [U] =>> Either[U, T]] {
    override def toString(): String = "Foo1"
  }

  class Bar[F[_], A] {
    override def toString(): String = "Bar"
  }

  class Baz[F[X] <: List[X]] {
    override def toString(): String = "Baz"
  }

  sealed trait QuxArg[T]
  class Arg1[T]() extends QuxArg[T]

  sealed trait Sum
  class Prod extends Sum

  class Qux[+F[X] <: QuxArg[X]] {
    override def toString(): String = "Qux"
  }

  class Quux[-F[X] >: Arg1[X]] {
    override def toString(): String = "Quux"
  }

  class Quuux[-F[X] >: Arg1[X] <: QuxArg[X]] {
    override def toString(): String = "Quuux"
  }

  class Boo[F[X] <: Either[X, Int]] {
    override def toString(): String = "Boo"
  }

  class Contra[-T >: Prod] {
    override def toString(): String = "Contra"
  }

  // class Hoo[F[X <: String] <: Either[X, Int]] { https://github.com/lampepfl/dotty/issues/8329
  //   override def toString(): String = "Hoo"
  // }

}
