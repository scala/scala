trait Bip[T] { def h: T }
trait BoolBip extends Bip[Boolean]

class A {
  def g(x: Boolean): Unit = ()
  def f(xs: List[Bip[_]]) = xs foreach { case x: BoolBip => g(x.h) }
}

class B {
  def g(x: Boolean): Unit = ()
  def g(x: Int): Unit = ()
  def f(xs: List[Bip[_]]) = xs foreach { case x: BoolBip => g(x.h) }
}
