package test

class X1
class X2
class X3

trait One[A]
trait Two[A, B]

class Foo extends Two[X1, X2] with One[X3]
object Test {
  def test1[M[_], A](x: M[A]): M[A] = x

  val foo = new Foo

  test1(foo): One[X3]     // fails without -Xsource:2.12
  test1(foo): Two[X1, X2] // fails with -Xsource:2.12
}
