import scala.language.implicitConversions

case class A(b: Int, c: String)

object Test extends App {
  implicit def s2i(s: String): Int = s.length
  implicit def toA[T](t: T)(implicit f: T => Int): A = A(f(t), t.toString)
  println("asdf".copy(b = 3))
}