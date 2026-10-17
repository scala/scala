class A { def m(i: Int) = 0; def m(s: String) = 0 }
class D { def n[T](t: T)(implicit i: Int): List[T] = Nil }
class C
object Test {
  implicit val i: Int = 0
  implicit class Inject(a: A) { def m(c: C) = c }
  def g(a: A, c: C, d: D): String = d.n(a.m(c))
}
