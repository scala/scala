package tastytest

object HKNest {

  class HKClass_13[F <: [T] =>> [U] =>> (U, T)] {
    def foo[T,U](x: F[T][U]): String = x.toString()
  }

  class HKClass_14[M[F >: [X] =>> Arg1[X] <: [Y,Z] =>> QuxArg[Y]]] {
    def foo[F >: [X] =>> Arg1[X] <: [Y,Z] =>> QuxArg[Y]](x: M[F]): String = x.toString()
  }

  def test13 = new HKClass_13[HKLam].foo[Int,String](("",0))

  // def test14 = new HKClass_14[Quuux].foo(new Quuux[QuxArg])

  type HKLam = [T] =>> [U] =>> (U, T)

  sealed trait QuxArg[T]
  class Arg1[T]() extends QuxArg[T]

  class Quuux[F[X] >: Arg1[X] <: QuxArg[X]] {
    override def toString(): String = "Quuux"
  }

}
