// scalac: -Xlint:option-implicit -Xfatal-warnings
//
class A
class B

object Test {
  val a: A = null
  implicit def a2b(a: A) = new B

  Option.whenNonNull[B](a)

  val b: Option[B] = Option.whenNonNull(a)
}
