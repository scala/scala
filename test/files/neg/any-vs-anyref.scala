trait Quux extends Any
trait QuuxRef extends AnyRef
final class Bippy(val x: Any) extends AnyVal with Quux

object Foo {
  def foo1[A <: Product](a: A)                                      = { type X = a.type }
  def foo2[A <: Product with Quux](a: A)                            = { type X = a.type }
  def foo3(a: Product)                                              = { type X = a.type }
  def foo4(a: Product with Quux)                                    = { type X = a.type }
  def foo5(x: Quux with Product)                                    = (x eq "abc") && ("abc" eq x)
  def foo6(x: Quux with Product { def f: Int })                     = (x eq "abc") && ("abc" eq x)
  def foo7(x: Quux with Product { def eq(other: String): Boolean }) = (x eq "abc") && ("abc" eq x)

  def ok1[A <: QuuxRef](a: A)                            = { type X = a.type }
  def ok2[A <: Product with QuuxRef](a: A)               = { type X = a.type }
  def ok3(a: QuuxRef)                                    = { type X = a.type }
  def ok4(a: Product with QuuxRef)                       = { type X = a.type }
  def ok5(x: QuuxRef with Product)                       = (x eq "abc") && ("abc" eq x)
  def ok6(x: QuuxRef with Product { def f: Int })        = (x eq "abc") && ("abc" eq x)
  def ok7(x: QuuxRef { def eq(other: String): Boolean }) = (x eq "abc") && ("abc" eq x)

  def bad1(x: Bippy, y: Bippy) = x eq y
}

object Bar {
  def f(x: Quux { def g(x: Int): Int }): Int = x g 5
  f(new Quux { def g(x: String) = x })
  f(new Quux { def g(x: Int) = x })
}
