// scalac: -Xsource:2.12
//
package test

trait Two[A, B]

object Test {
  def foo[M[_], A](m: M[A]) = ()
  def test(ma: Two[Int, String]) = foo(ma) // should fail with -Xsource:2.12
}
