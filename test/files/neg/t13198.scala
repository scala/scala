class A { def m(i: Int) = 0; def m(s: String) = 0 }
class C { def m(c: C)(implicit i: Int) = "" }
object Test {
  implicit val i: Int = 0
  implicit class Inject(a: A) { def m(c: C) = c }
  def f(a: A, c: C): C = a.m(c).m(c)
}
