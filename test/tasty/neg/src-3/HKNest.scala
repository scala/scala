package tastytest

object HKNest {

  class HKClass_13[F <: [T] =>> [U] =>> (U, T)] {
    def foo[T,U](x: F[T][U]): String = x.toString()
  }

  def test13 = new HKClass_13[HKLam].foo[Int,String](("",0))

  type HKLam = [T] =>> [U] =>> (U, T)

}
