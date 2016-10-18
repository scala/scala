package test

// See: https://github.com/milessabin/si2712fix-demo/issues/3
object Test {
  trait A[T1, T2] { }
  trait B[T1, T2] { }
  class C[T] extends A[T, Long] with B[T, Double]
  class CB extends A[Boolean, Long] with B[Boolean, Double]

  trait A2[T]
  trait B2[T]
  class C2[T] extends A2[T] with B2[T]
  class CB2 extends A2[Boolean] with B2[Boolean]

  def meh[M[_], A](x: M[A]): M[A] = x

  val m0 = meh(new C[Boolean])
  m0: C[Boolean]
  val m1 = meh(new CB)
  m1: A[Boolean, Long]
  val m2 = meh(new C2[Boolean])
  m2: C2[Boolean]
  val m3 = meh(new CB2)
  m3: A2[Boolean]
}
