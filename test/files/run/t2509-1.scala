// scalac: -Xsource:3.0
import scala.language.implicitConversions

class A
class B extends A

trait Y {
  def value: String
}

trait X[-T] {
  def y(t: T): Y
}

object XA extends X[A] {
  def y(a: A) = new Y { def value = s"${a.getClass}: AValue" }
}

object XB extends X[B] {
  def y(b: B) = new Y { def value = s"${b.getClass}: BValue" }
}

object Test {
  implicit def f[T](t: T)(implicit x: X[T]): Y = x.y(t)
  implicit val xa: X[A] = XA
  implicit val xb: X[B] = XB

  def main(argv: Array[String]): Unit = {
    val a = new A
    val b = new B
    println(s"A: ${a.value}")
    println(s"B: ${b.value}")
  }
}
