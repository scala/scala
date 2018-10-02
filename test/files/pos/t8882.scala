import language.higherKinds

object Test {
  class A[T[_]]
  object B extends A[List]
  object C extends A[Option]

  val values1 = Seq[Any](B, C)
  val values2 = Seq(B, C)
}
