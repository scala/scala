trait C[A]

object C {
  implicit def ipl[A](implicit from: A => Ordered[A]): C[A] = null
}

object P {
  def foo[A](i: A, j: A)(implicit c: C[A]): Unit = ()
}

class ImplicitChainTest {
  def testTrivial: Unit = {
    P.foo('0', '9')
    P.foo('0', '9')
  }
}
