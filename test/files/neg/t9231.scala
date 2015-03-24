class M[A]
class C {
  implicit def M1: M[Int] = null
  implicit def M2: M[String] = null

  def foo[A](implicit M: M[A]) = null

  foo[DoesNotExist]
}
