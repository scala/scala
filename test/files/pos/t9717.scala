// scalac: -Yno-predef

import scala.Predef.implicitly

class A(val a: Int)
class B(implicit F: Int) extends A( implicitly[Int] )
class C(implicit F: Int) extends A( {
  val v = implicitly[Int]
  v
})
