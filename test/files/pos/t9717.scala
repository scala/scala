//> using options -Yno-predef

import scala.Predef.implicitly

class A(val a: Int)
class B(implicit F: Int) extends A( implicitly[Int] )
class C(implicit F: Int) extends A( {
  val v = implicitly[Int]
  v
})


class A1(val a: Int)(implicit val F: Int)
class B1(implicit F: Int) extends A1(implicitly[Int])
class C1(implicit F: Int) extends A1({
  val v = implicitly[Int]
  v
})
class D1 extends A1({
  implicit val v: Int = 1; implicitly[Int]
})(0)

class D(x: Int) {
  import D.f
  def this() = { this(D.f); implicitly[Int] } // D.f in scope after self constr call
}
object D {
  implicit def f: Int = 1
}
