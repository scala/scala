class A(val a: Int)(implicit val F: Int)
class B(implicit F: Int) extends A({ implicit val v: Int = 1; implicitly[Int] }) // ambiguous

class C(x: Int) {
  implicit def f: Int = 1
  def this() = this(implicitly[Int]) // neg
  def this(s: String) = this(f) // neg (`this` is not in scope!)
}

class D(x: Int) {
  import D.f
  def this() = { this(implicitly[Int]) } // not in scope (spec 5.3.1, scope which is in effect at the point of the enclosing class definition)
}
object D {
  implicit def f: Int = 1
}
