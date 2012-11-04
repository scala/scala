class A
class B

object Test {
  val a: A = null
  implicit def a2b(a: A) = new B

  Option[B](a)

  val b: Option[B] = Option(a)
}
