
class A {
  def f(xs: List[Int], ys: AnyRef*) = ()
  def f(xs: AnyRef*) = ()

  f()
  f(List[AnyRef](): _*)
  f(List(): _*)
  f(Nil: _*)
  f(Array(): _*)
  f(Array[AnyRef](): _*)
  f(List(1))
  f(List(1), Nil: _*)
  f(List(1), Array(): _*)
}
