package p

object O {
  protected[p] def f(x: Int = 1) = x
  private[p] def g(x: Int = 1) = x
  private def h(x: Int = 1) = x
}

object Test {
  O.f()
  O.f$default$1
  O.g()
  O.g$default$1
  O.h()
  O.h$default$1
}
