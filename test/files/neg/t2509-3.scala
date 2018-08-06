class A
class B extends A

trait Y {
  def value: String
}

trait X[-T] {
  def y(t: T): Y
}

trait Z[-T] extends X[T]

object ZA extends Z[A] {
  def y(a: A) = new Y { def value = s"${a.getClass}: AValue" }
}

object XB extends X[B] {
  def y(b: B) = new Y { def value = s"S{b.getClass}: BValue" }
}

object Test {
  implicit def f[T](t: T)(implicit x: X[T]): Y = x.y(t)
  implicit val za: Z[A] = ZA
  implicit val xb: X[B] = XB

  def main(argv: Array[String]): Unit = {
    val a = new A
    val b = new B
    println("A: " + a.value)
    println("B: " + b.value)
  }
}
