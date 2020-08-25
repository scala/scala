// scalac: -Xsource:3.0
class A
class B extends A
class C extends B

trait X[-T, U] {
  val u: U
}

object XA extends X[A, Boolean] {
  val u = true
}

object XB extends X[B, Int] {
  val u = 23
}

object Test {
  implicit def f[T, U](t: T)(implicit x: X[T, U]): U = x.u
  implicit val xa: X[A, Boolean] = XA
  implicit val xb: X[B, Int] = XB

  val fa = f(new A)
  val ffa: Boolean = fa

  // Should be ambiguous
  val fb = f(new B)
  val ffb: Int = fb
}
