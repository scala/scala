trait E1[T] {
  def f(that: T) = ()
}

class E2[T, C] extends E1[E2[T, C]]
class E3[R[C] <: E2[R[C], C], C] extends E1[E3[R, C]]
class F3[R[C] <: E2[R[C], C], C] {
  def apply(value: C): E3[R, C] = ???
}
class E4[C] extends E2[E4[C], C]

object MyApp {
  implicit def int2string(value: Int): String = ???
  implicit def coef2rf[D, R[C] <: E2[R[C], C], C](value: D)(implicit ev: D => C, factory: F3[R, C]) = factory(value)

  def test: Unit = {
    implicit val r: F3[E4, String] = ???
    val y: E3[E4, String] = ???
    val i: Int = 0
    i.f(y)
    i.f(y) // second
    ()
  }
}
