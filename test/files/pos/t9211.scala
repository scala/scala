//> using options -Werror -Xlint

trait T[A]
class C extends T[Any]

class Test {
  def f[A](t: T[A]) = ()
  def g() = f(new C)
}
