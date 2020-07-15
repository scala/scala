package tastytest

object HKNest {

  class HKClass_4[F[+_]]
  class HKClass_5[F[-_]]

  class HKClass_6[N[-F[X] >: QuxArg[X]]] {
    def foo[F1[X] >: QuxArg[X]](x: N[F1]): String = x.toString()
  }

  class HKClass_13[F <: [T] =>> [U] =>> (U, T)] {
    def foo[T,U](x: F[T][U]): String = x.toString()
  }

  class HKClass_14[M[F >: [X] =>> Arg1[X] <: [Y,Z] =>> QuxArg[Y]]] {
    def foo[F >: [X] =>> Arg1[X] <: [Y,Z] =>> QuxArg[Y]](x: M[F]): String = x.toString()
  }

  class HKClass_15 {
    def foo[F[X] <: List[X] | Option[X], A](fa: F[A]): String = fa.toString()
  }

  class HKClass_16[M[_]] {
    def foo[F <: [T] =>> [U] =>> (U, T),T,U](x: M[F[T][U]]): String = x.toString()
  }

  class HKClass_17[F[X] <: List[X] | Option[X]] {
    def foo[A](fa: F[A]): String = fa.toString()
  }

  def test13 = new HKClass_13[HKLam].foo[Int,String](("",0))
  def test16 = new HKClass_16[List].foo[HKLam,Int,String](("",0) :: Nil)
  // def test6 = new HKClass_6[Qux]

  type HKLam = [T] =>> [U] =>> (U, T)

  type DummyQuxArg[Y,Z] = QuxArg[Y]

  sealed trait QuxArg[T]
  class Arg1[T]() extends QuxArg[T]

  class Qux[+F[X] >: QuxArg[X]] {
    override def toString(): String = "Quux"
  }

  class Quux[-F[X] >: Arg1[X]] {
    override def toString(): String = "Quux"
  }

  class Quuux[F[X] >: Arg1[X] <: QuxArg[X]] {
    override def toString(): String = "Quuux"
  }

  class Quuuux[F[Y,Z] >: Arg1[Y] <: QuxArg[Y]] {
    override def toString(): String = "Quuux"
  }

}
