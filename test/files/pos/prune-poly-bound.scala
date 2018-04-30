class Base[T0]
class Derived[T1] extends Base[T1]

class Foo[T2, U2]

object Foo {
  implicit def mkFoo[T3, U3 <: Base[T3]](implicit ev: U3 <:< Base[T3]) : Foo[U3, Base[T3]] = ???
}

object Test {
  def foo[T4, U4](t: T4)(implicit ftu: Foo[T4, U4]): U4 = ???
  val bi: Base[Int] = foo(null.asInstanceOf[Derived[Int]])
}
